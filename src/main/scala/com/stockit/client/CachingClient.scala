package com.stockit.client

import java.util.Date

import com.stockit.cache.FileSolrDocumentListCacheStrategy
import org.apache.solr.common.SolrDocument

/**
 * Created by dmcquill on 4/25/15.
 */
class CachingClient extends Client {

    var fetchCache = Map[Date, List[SolrDocument]]()
    val neighborsCacheStrategy = new FileSolrDocumentListCacheStrategy

    override def fetch(date: Date) = {
        super.fetch(date)
    }

    override def neighbors(trainDocs: List[SolrDocument], doc: SolrDocument, number: Int) = {
        val request = neighborQuery(trainDocs, doc, number)
//        val fqString = s"${request.get(1).key}->${request.extraParams(1).value.toString}"
//        val qString = request.getQuery

        val documents = super.neighbors(trainDocs, doc, number)


//        documents(0).getMap()

//        neighborsCacheStrategy.store(fqString + "::" + qString, documents)

        documents
    }

    override def minMaxDate(trainDocs: List[SolrDocument]) = {
        super.minMaxDate(trainDocs)
    }

}
