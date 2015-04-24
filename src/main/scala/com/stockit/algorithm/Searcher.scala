package com.stockit.algorithm

import com.github.seratch.scalikesolr.SolrDocument
import com.stockit.client.Client
import weka.core.Instance
import weka.core.Instances
import weka.core.neighboursearch.LinearNNSearch

import scala.collection.JavaConverters._


/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Searcher {
    def fetchNeighbors(doc: SolrDocument) = {
        Client.neighbors(doc, 5)
    }
}
