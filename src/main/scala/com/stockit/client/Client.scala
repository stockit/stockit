package com.stockit.client

import java.io.IOException
import java.net.URL
import java.text.SimpleDateFormat
import java.util.{SimpleTimeZone, Date}
import com.github.seratch.scalikesolr.{SolrClient, SolrDocument, Solr}
import com.github.seratch.scalikesolr.request.query.{MaximumRowsReturned, Sort, Query}
import com.github.seratch.scalikesolr.request.QueryRequest

/**
 * Created by jmcconnell1 on 4/22/15.
 */
object Client {
    val host = "http://solr.deepdishdev.com:8983/solr"
    val client: SolrClient = Solr.httpServer(new URL(host + "/articleStock")).newClient(100 * 1000, 100 * 1000)
    var format: SimpleDateFormat = null
    val instanceCount = 8000
    val queryCutoff = 100 // 100 performed better?

    def fetch(date: Date) = {
        val request = dateQueryRequest(date)
        try {
            val response = client.doQuery(request)
            response.response.documents
        } catch {
            case e: IOException => {
                println("Error on query:" + request.queryString)
                throw e
            }
        }
    }


    def ensureNeighborsBeforeDate(documents: List[SolrDocument], latestDate: Date) = {
        documents.foreach((doc) => {
            val date = dateOfDoc(doc)
            if (date.after(latestDate)) {
                throw new Exception(s"Article ${doc.get("articleId")} has date:[$date] which is after $latestDate}")
            }
        })
    }

    def ensureNeighborsDontIncludeSelf(documents: List[SolrDocument], docId: String) = {
        documents.foreach((doc) => {
            val id = idOfDoc(doc)
            if (id == docId) {
                throw new Exception(s"Article ${id} was returned as neighbor")
            }
        })
    }

    def neighbors(trainDocs: List[SolrDocument], doc: SolrDocument, number: Int): List[SolrDocument] = {
        val request = neighborQuery(trainDocs, doc, number)
        try {
            val response = client.doQuery(request)
            ensureNeighborsBeforeDate(response.response.documents, dateOfDoc(doc))
            response.response.documents
        } catch {
            case e: IOException => {
                println("Error on query:" + request.queryString)
                return neighbors(trainDocs, doc, number)
            }
        }
    }

    def dateOfDoc(doc: SolrDocument) = {
        formatter.parse(createParsableString(doc.get("historyDate").toString()))
    }

    def idOfDoc(doc: SolrDocument) = {
        doc.get("articleId").toString()
    }

    def neighborQuery(trainDocs: List[SolrDocument], doc: SolrDocument, count: Int) = {
        var query = doc.get("content").toString().replaceAll("[^\\s\\d\\w]+", "")
        query = query.substring(0, List(queryCutoff, query.length).min)
        val (minDate, maxDate) = minMaxDate(trainDocs)
        val request = new QueryRequest(Query(query))
        request.remove("wt")
        request.remove("start")
        request.set("wt", "json")
        request.setMaximumRowsReturned(new MaximumRowsReturned(count))
        val fq =  String.format("historyDate:[%s TO %s]", minDate, maxDate)
        // println(fq, s"docDate${doc.get("historyDate").toString}")
        request.set("fq", fq)
        request
    }

    def minMaxDate(trainDocs: List[SolrDocument]) = {
        (trainDocs.head.get("historyDate").toString(), trainDocs.last.get("historyDate").toString())
    }

    def sortedByDate() = {
        val request = sortedByDateQuery
        try {
            val response = client.doQuery(request)
            response.response.documents
        } catch {
            case e: IOException => {
                println("Error on query:" + request.queryString)
                throw e
            }
        }
    }

    def sortedByDateQuery = {
        val request = new QueryRequest(Query("*:*"))
        request.setSort(Sort.as("historyDate asc"))
        request.setMaximumRowsReturned(new MaximumRowsReturned(instanceCount))
        request
    }

    def dateQueryRequest(date: Date) = {
        val string = parseDate(date)
        new QueryRequest(Query("historyDate:" + string)) // dateToString(date)))
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

    def formatter = {
        if (format == null) {
            format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
            format.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"))
        }
        format
    }
}
