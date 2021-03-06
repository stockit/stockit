package com.stockit.client

import java.io.IOException
import java.net.URL
import java.text.SimpleDateFormat
import java.util.regex.Pattern
import java.util.{SimpleTimeZone, Date}
import com.stockit.module.service.SolrClientModule
import org.apache.solr.client.solrj.request.QueryRequest
import org.apache.solr.client.solrj.{SolrRequest, SolrQuery, SolrClient}
import org.apache.solr.common.{SolrDocumentList, SolrDocument}
import scaldi.Injectable

import scala.collection.mutable.ListBuffer

/**
 * Created by jmcconnell1 on 4/22/15.
 */
class Client extends Injectable {

    implicit val module = new SolrClientModule

    val host = "http://solr.deepdishdev.com:8983/solr"
    val client: SolrClient = inject[SolrClient]('solrClient and 'httpSolrClient and 'articleStockSolrClient)
    val instanceCount = 10000
    val queryCutoff = 5000 // 100 performed better?

    def fetch(date: Date) = {
        val request = dateQueryRequest(date)
        try {
            val response = request.process(client)
            documentListToList(response.getResults)
        } catch {
            case e: IOException => {
                println("Error on query:" + request.toString)
                throw e
            }
        }
    }


    def ensureNeighborsBeforeDate(documents: List[SolrDocument], latestDate: Date) = {
        documents.foreach((doc) => {
            val date = dateOfDoc(doc)
            if (date.after(latestDate)) {
                println(s"Article ${doc.getFieldValue("articleId")} has date:[$date] which is after $latestDate}")
            }
        })
    }

    def ensureNeighborsDontIncludeSelf(documents: List[SolrDocument], docId: String) = {
        documents.foreach((doc) => {
            val id = idOfDoc(doc)
            if (id == docId) {
                throw new Exception(s"Article $id was returned as neighbor")
            }
        })
    }

    def neighbors(trainDocs: List[SolrDocument], doc: SolrDocument, number: Int): List[SolrDocument] = {
        val request = neighborQuery(trainDocs, doc, number)
        try {
            val response = client.query(request.getParams, SolrRequest.METHOD.POST)
            val documents = documentListToList(response.getResults)
            ensureNeighborsBeforeDate(documents, dateOfDoc(doc))
            documents
        } catch {
            case e: IOException => {
                println("Error on query:" + request.toString)
                neighbors(trainDocs, doc, number)
            }
            case e: Exception => {
                println("Really bad fuck up returning no neighbors")
                Nil
            }
        }
    }

    def dateOfDoc(doc: SolrDocument) = {
        doc.getFieldValue("historyDate").asInstanceOf[Date]
    }

    def idOfDoc(doc: SolrDocument) = {
        doc.get("articleId").toString()
    }

    def neighborQuery(trainDocs: List[SolrDocument], doc: SolrDocument, count: Int) = {
        val pattern = Pattern.compile("[^\\s\\d\\w]+") // Pattern.compile("([\\\\(|\\\\)|\\\\+|\\\\-|\\\\?|\\\\*|\\\\{|\\\\}|\\\\[|\\\\]|\\\\:|\\\\~|\\\\!|\\\\^|&&|\\\"|\\\\\\\\|\\\\||\", \"\")])");
        var queryString = doc.getFieldValue("content").toString
        queryString = pattern.matcher(queryString).replaceAll("");

        queryString = queryString.substring(0, List(queryCutoff, queryString.length).min)
        val (minDate: Date, maxDate: Date) = minMaxDate(trainDocs)

        val query = new SolrQuery()
        query.setParam("q", queryString)
        query.setRows(count)
        query.setStart(0)

        query.setFields("articleId","title","content","date","stockHistoryId","symbol","historyDate","open","high","low","close","score","adjClose","volume","id","_version_")

        val fq =  String.format("historyDate:[%s TO %s]", formatDateForSolr(minDate, isMin = true), formatDateForSolr(maxDate, isMin = false))
        query.setFilterQueries(fq)

        new QueryRequest(query)
    }

    def formatDateForSolr(date: Date, isMin: Boolean) = {
        try {
            s"${dayFormatter.format(date)}${if(isMin) "T00:00:00Z" else "T59:59:59Z"}"
        } catch {
            case e: Exception => {
                println(s"Error: $date, $isMin")
                throw e
            }
        }
    }

    def minMaxDate(trainDocs: List[SolrDocument]) = {

        val dates: List[Date] = trainDocs.map{ doc =>
            doc.getFieldValue("historyDate") match {
                case date:Date => Some(date)
                case _ => None
            }
        }.flatten

        val maxDate = dates.max
        val minDate = dates.min

        (minDate, maxDate)
    }

    def sortedByDate() = {
        val request = sortedByDateQuery
        try {
            val response = request.process(client)
            val documents = documentListToList(response.getResults())
            documents.sortBy(_.getFieldValue("historyDate").asInstanceOf[Date])
        } catch {
            case e: IOException => {
                println("Error on query:" + request.toString)
                throw e
            }
        }
    }

    def sortedByDateQuery = {
        val query = new SolrQuery
        query.setSort("historyDate", SolrQuery.ORDER.desc)
        query.setQuery("*:*")
        query.setRows(instanceCount)
        new QueryRequest(query)
    }

    def dateQueryRequest(date: Date) = {
        val string = parseDate(date)
        val query = new SolrQuery
        query.setQuery("historyDate:" + string)
        new QueryRequest(query) // dateToString(date)))
    }

    def parseDate(date: Date) = {
        parseDateString(dateToString(date))
    }

    def parseDateString(rawString: String) = {
        val withChars = rawString.substring(0,10) + "T" + rawString.substring(11, rawString.length) + "Z"
        withChars.replace(":", "\\:")
    }

    def dateToString(date: Date) = {
        formatter.format(date)
    }

    def createParsableString(dateString: String) = {
        dateString.replace("T", " ").replace("Z", "")
    }

    def documentListToList(list: SolrDocumentList) = {
        var listBuffer = new ListBuffer[SolrDocument]

        val it = list.listIterator()
        while(it.hasNext) {
            listBuffer += it.next
        }

        listBuffer.toList
    }

    def formatter = synchronized {
        val format = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ssZ")
        format.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"))
        format
    }

    def dayFormatter = synchronized {
        val format = new SimpleDateFormat("yyyy-MM-dd")
        format
    }
}
