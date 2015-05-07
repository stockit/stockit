package com.stockit.cache

import java.io._
import java.text.{SimpleDateFormat, MessageFormat}
import java.util.Date

import com.stockit.serialization.SerializationUtil
import org.apache.commons.lang.SerializationUtils
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.xml.{Elem, XML}

case class SolrCacheDocumentField(key: String, value: AnyRef) {
    val serializationUtil = new SerializationUtil()

    def toXml() = {
        val serializedString = serializationUtil.objectToString(value)

        <field name={ key } class={ value.getClass.getCanonicalName}>{ serializedString }</field>
    }
}

case class SolrCacheDocument(fields: List[SolrCacheDocumentField]) {
    def toXml() =
        <document>{
                for {
                    field <- fields
                } yield field.toXml()
            }</document>
}

case class SolrCacheDocumentSet(query: String, documents: List[SolrCacheDocument]) {

    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ssZ")

    def toXml() =
        <cacheEntry><query>{ query }</query><documents>{
                for {
                document <- documents
                } yield document.toXml()}</documents><date>{ dateFormat.format(new Date()) }</date></cacheEntry>
}

class FileSolrDocumentListCacheStrategy {

    val baseDirectoryPath = "cache"

    val baseDirectory = new File(baseDirectoryPath)

    var cacheInitialized = false

    val cacheFileNameFormat = "solr-document-list-cache-{0}.xml"

    val storedQueries = mutable.Map[String,String]()

    val serializationUtil = new SerializationUtil()

    private def outputFile(query: String): File = synchronized {
        val fileNoPattern = "(\\d+)".r

        if(storedQueries.contains(query)) {
            return new File(baseDirectory, storedQueries(query))
        }

        val ids = baseDirectory.listFiles
            .map(_.getName)
            .map(fileNoPattern.findFirstIn)
            .flatten
            .map { str: String =>
                str.toInt
            }

        val id: Integer = if(ids.isEmpty) 1 else ids.max + 1
        new File(baseDirectory, MessageFormat.format(cacheFileNameFormat, id))
    }

    private def init(): Unit = synchronized {

        if(cacheInitialized) return

        if(!baseDirectory.exists) {
            baseDirectory.mkdir
        }

        val queries = baseDirectory.listFiles
            .map { file =>
                (XML.loadFile(file), file.getName())
            }
            .map { tup =>
                val el = tup._1
                ((el \\ "cacheEntry" \\ "query").text, tup._2)
            }
            .foreach { tup =>
                storedQueries += tup._1 -> tup._2
            }

        cacheInitialized = true
    }

    def store(query: SolrQuery, docs: List[SolrDocument]): Unit = synchronized {

        val key = serializationUtil.objectToString(query)

        init()

        val documentSet = SolrCacheDocumentSet(key, docs.map { doc =>
            SolrCacheDocument(fields = {
                doc.getFieldNames.asScala.map { field =>
                    SolrCacheDocumentField(field, doc.getFieldValue(field))
                }.toList
            })
        })

        val outputLocation = outputFile(key)
        val bufferedWriter = new BufferedWriter(new FileWriter(outputLocation))

        XML.write(bufferedWriter, documentSet.toXml(), "UTF-8", xmlDecl = true, null)

        storedQueries += key -> outputLocation.getName

        bufferedWriter.close
    }

    def retrieve(query: SolrQuery): Option[List[SolrDocument]] = {

        val key = serializationUtil.objectToString(query)

        if(storedQueries.contains(key)) {
            val fileName = storedQueries(key)
            val xml = XML.loadFile(new File(baseDirectory, fileName))

            val documents = for {
                cacheEntry <- xml \\ "cacheEntry"
                document <- cacheEntry \\ "document"
            } yield document

            val solrCacheDocuments = documents
                .map({ docEl =>
                    SolrCacheDocument(fields = (docEl \ "field").map { fieldEl =>
                        val name = fieldEl.attribute("name").map(_.text).head
                        val className = fieldEl.attribute("class").map(_.text).head

                        val value = serializationUtil.objectFromString(fieldEl.text)

                        SolrCacheDocumentField(name,value)
                    }.toList)
                })

            val solrDocuments = solrCacheDocuments.map { solrCacheDocument =>
                val solrDocument = new SolrDocument()

                solrCacheDocument.fields.foreach { field =>
                    solrDocument.setField(field.key, field.value)
                }

                solrDocument
            }.toList

            Some(solrDocuments)
        } else {
            None
        }
    }

}