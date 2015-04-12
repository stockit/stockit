package com.stockit

import com.stockit.module.service.SolrClientModule
import org.apache.solr.client.solrj.response.QueryResponse
import org.apache.solr.client.solrj.{SolrQuery, SolrClient}
import scaldi.Injectable

/**
 * Created by dmcquill on 4/12/15.
 */
object Main extends Injectable {

    def main(args: Array[String]): Unit = {

        implicit val module = new SolrClientModule()

        val solrClient = inject[SolrClient]('solrClient and 'httpSolrClient and 'articlesSolrClient)

        var query = new SolrQuery()
        query.setQuery("*:*")
        query.setRows(50)
        query.setStart(0)

        val response: QueryResponse = solrClient.query(query)
        val documentList = response.getResults()

        println(documentList)
    }

}
