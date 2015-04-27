package com.stockit.algorithm

import com.stockit.client.{CachingClient}
import org.apache.solr.common.SolrDocument

import scala.collection.JavaConverters._


/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Searcher {
    val client = new CachingClient

    def fetchNeighbors(trainDocs: List[SolrDocument], doc: SolrDocument) = {
        client.neighbors(trainDocs, doc, 5)
    }
}
