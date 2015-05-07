package com.stockit

import java.util.regex.Pattern
import java.util.Date

import com.stockit.cache.FileSolrDocumentListCacheStrategy
import com.stockit.module.service.SolrClientModule
import org.apache.solr.client.solrj.request.QueryRequest
import org.apache.solr.client.solrj.{SolrRequest, SolrQuery, SolrClient}
import org.apache.solr.common.SolrDocument
import scaldi.Injectable

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
/**
 * Created by dmcquill on 4/26/15.
 */
object TestCache extends Injectable {

    def compareDocLists(left: List[SolrDocument], right: List[SolrDocument] ): Boolean = {
        left.zip(right).map { docTuple =>
            val leftFields = docTuple._1.getFieldNames.asScala.toSet
            val rightFields = docTuple._2.getFieldNames.asScala.toSet

            if(leftFields.diff(rightFields).size > 0) {
                println("found mismatched field names")
                false
            } else {
                val fields = leftFields
                val leftDocument = docTuple._1
                val rightDocument = docTuple._2

                val result = fields.map { field =>
                    (leftDocument.getFieldValue(field), rightDocument.getFieldValue(field))
                }
                .map(valueTuple => valueTuple._1.equals(valueTuple._2)).reduce(_ && _)

                if(!result) {
                    println("found mismatched field values")
                }

                result
            }
        }.reduce(_ && _)
    }

    def main(args: Array[String]): Unit = {
        implicit val module = new SolrClientModule

        val cacheStrategy = new FileSolrDocumentListCacheStrategy("cache")

        val solrClient = inject[SolrClient]('solrClient and 'httpSolrClient and 'articlesSolrClient)

        val rows = 1000
        val querySize = 500
        val startValues = (0 to Math.floor(rows.toDouble / querySize).toInt).map(_ * querySize)

        val queryMillis = new ListBuffer[Long]()
        val cacheMillis = new ListBuffer[Long]()
        val cacheRetrieveMillis = new ListBuffer[Long]()

        var numCorrect = 0
        var numIncorrect = 0

        var numRan = 0

        startValues.foreach { start =>
            val query = new SolrQuery
            query.setQuery("*:*")
            query.setStart(start)
            query.setRows(querySize)

            val queryRequest = new QueryRequest(query)
            val params = queryRequest.getParams

            val dateBeforeQuery = new Date
            val docList = solrClient.query(params).getResults
            val dateAfterQuery = new Date

            var documentList = new ListBuffer[SolrDocument]()

            val it = docList.listIterator

            while(it.hasNext) {
                documentList += it.next
            }

            val docs = documentList.toList

            val dateBeforeStore = new Date
            cacheStrategy.store(params, docs)
            val dateAfterStore = new Date

            val dateBeforeCacheRetrieve = new Date
            val cachedValue = cacheStrategy.retrieve(params)
            val dateAfterCacheRetrieve = new Date

            cachedValue match {
                case Some(cachedDocs) =>
                    if(compareDocLists(docs, cachedDocs)) {
                        numCorrect += 1
                    } else {
                        numIncorrect += 1
                    }
                case None => numIncorrect += 1
            }

            val millisForQuery = dateAfterQuery.getTime - dateBeforeQuery.getTime
            val millisForCache = dateAfterStore.getTime - dateBeforeStore.getTime
            val millisForCacheRetrieve = dateAfterCacheRetrieve.getTime - dateBeforeCacheRetrieve.getTime

            queryMillis += millisForQuery
            cacheMillis += millisForCache
            cacheRetrieveMillis += millisForCacheRetrieve

            numRan += 1
            println(s"processed $numRan of ${ startValues.size }")
        }

        val queryMillisAvg = queryMillis.sum.toDouble / queryMillis.size
        val cacheRetrieveMillisAvg = cacheRetrieveMillis.sum.toDouble / cacheRetrieveMillis.size
        val cacheMillisAvg = cacheMillis.sum.toDouble / cacheMillis.size
        var accuracy = numCorrect.toDouble / (numCorrect + numIncorrect)

        println(s"average time to query: [$queryMillisAvg ms]")
        println(s"average time to query cache: [$cacheRetrieveMillisAvg ms]")
        println(s"average time to store to cache: [$cacheMillisAvg ms]")
        println(s"accuracy: $accuracy")

        numCorrect = 0
        numIncorrect = 0
        numRan = 0

        startValues.foreach { start =>
            val query = new SolrQuery
            query.setQuery("*:*")
            query.setStart(start)
            query.setRows(querySize)

            val queryRequest = new QueryRequest(query)
            val params = queryRequest.getParams

            val docList = solrClient.query(params).getResults

            var documentList = new ListBuffer[SolrDocument]()

            val it = docList.listIterator

            while(it.hasNext) {
                documentList += it.next
            }

            val docs = documentList.toList
            val cachedValue = cacheStrategy.retrieve(params)

            cachedValue match {
                case Some(cachedDocs) =>
                    if(compareDocLists(docs, cachedDocs)) {
                        numCorrect += 1
                    } else {
                        numIncorrect += 1
                    }
                case None => numIncorrect += 1
            }

            numRan += 1
            println(s"processed $numRan of ${ startValues.size }")
        }

        accuracy = numCorrect.toDouble / (numCorrect + numIncorrect)
        println(s"accuracy after cache: $accuracy")

        val query = new SolrQuery
        query.setQuery("*:*")
        query.setStart(0)
        query.setRows(1)

        val doc = solrClient.query(query).getResults.get(0)

        val pattern = Pattern.compile("[^\\s\\d\\w]+") // Pattern.compile("([\\\\(|\\\\)|\\\\+|\\\\-|\\\\?|\\\\*|\\\\{|\\\\}|\\\\[|\\\\]|\\\\:|\\\\~|\\\\!|\\\\^|&&|\\\"|\\\\\\\\|\\\\||\", \"\")])");
        var queryString = doc.getFieldValue("content").toString
        queryString = pattern.matcher(queryString).replaceAll("")

        queryString = queryString.substring(0, List(200, queryString.length).min)

        val neighborQuery = new SolrQuery()
        neighborQuery.setParam("q", queryString)
        neighborQuery.setRows(10)
        neighborQuery.setStart(0)
        neighborQuery.setFields("articleId","title","content","date","stockHistoryId","symbol","historyDate","open","high","low","close","adjClose","volume","id","score","_version_")

        val queryRequest = new QueryRequest(neighborQuery)
        val params = queryRequest.getParams

        var documentList = new ListBuffer[SolrDocument]()
        val docList = solrClient.query(params).getResults

        val it = docList.listIterator

        while(it.hasNext) {
            documentList += it.next
        }

        val docs = documentList.toList

        cacheStrategy.store(params, docs)

        val cachedValue = cacheStrategy.retrieve(params)

        cachedValue match {
            case Some(cachedDocs) =>
                if(compareDocLists(docs, cachedDocs)) {
                    println("neighbor cache correct")
                } else {
                    println("neighbor cache incorrect")
                }
            case None => println("neighbor cache incorrect")
        }
    }

}
