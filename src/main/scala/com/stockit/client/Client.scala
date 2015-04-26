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
    val client: SolrClient = Solr.httpServer(new URL(host + "/articleStock")).newClient(30 * 1000, 30 * 1000)
    var format: SimpleDateFormat = null
    val instanceCount = 5000
    val queryCutoff = 100

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

    def neighbors(trainDocs: List[SolrDocument], doc: SolrDocument, number: Int) = {
        val request = neighborQuery(trainDocs, doc, number)
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

    def formatter = {
        if (format == null) {
            format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
            format.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"))
        }
        format
    }
}
