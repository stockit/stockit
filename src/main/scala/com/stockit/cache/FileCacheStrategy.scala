package com.stockit.cache

import com.github.seratch.scalikesolr.SolrDocument
import com.lambdaworks.jacks.JacksMapper

class FileSolrDocumentListCacheStrategy[A] {

    def store(key: A, value: List[SolrDocument]): Unit = {
//        val stringValue = JacksMapper.writeValueAsString[List[SolrDocument]](value)
//
//        val objectValue = JacksMapper.readValue[List[SolrDocument]](stringValue)
//        println(objectValue)
    }

    def retrieve(key: A): Option[List[SolrDocument]] = {
        None
    }

}