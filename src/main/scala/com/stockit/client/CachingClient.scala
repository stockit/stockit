package com.stockit.client

import java.util.Date

import com.stockit.cache.FileSolrDocumentListCacheStrategy
import org.apache.solr.common.SolrDocument

/**
 * Created by dmcquill on 4/25/15.
 */
class CachingClient extends Client {

    val cacheStrategy = new FileSolrDocumentListCacheStrategy("neighborsCache")

    override def fetch(date: Date) = {
        super.fetch(date)
    }

    override def neighbors(trainDocs: List[SolrDocument], doc: SolrDocument, number: Int) = {
        val request = neighborQuery(trainDocs, doc, number)

        cacheStrategy.retrieve(request.getParams) match {
            case Some(documents) => documents
            case None =>
                val neighbors = super.neighbors(trainDocs, doc, number)
                cacheStrategy.store(request.getParams, neighbors)
                neighbors
        }
    }

    override def minMaxDate(trainDocs: List[SolrDocument]) = {
        super.minMaxDate(trainDocs)
    }

}
