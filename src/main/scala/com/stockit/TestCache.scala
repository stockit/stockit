package com.stockit

import com.stockit.module.service.SolrClientModule
import org.apache.solr.client.solrj.{SolrQuery, SolrClient}
import scaldi.Injectable

/**
 * Created by dmcquill on 4/26/15.
 */
object TestCache extends Injectable {

    def main(args: Array[String]): Unit = {
        implicit val module = new SolrClientModule

        val solrClient = inject[SolrClient]('solrClient and 'httpSolrClient and 'articleStockSolrClient)

        val query = new SolrQuery()
    }

}
