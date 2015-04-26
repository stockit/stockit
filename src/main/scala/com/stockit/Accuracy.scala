package com.stockit

import com.github.seratch.scalikesolr.SolrDocument
import com.stockit.client.Client
import com.stockit.algorithm.{Searcher, Predictor}
import com.stockit.exporters.NetCaptureExporter
import com.stockit.statistics.Statistics

import scala.collection.mutable
import scala.reflect.internal.util.Statistics

/**
 * Created by jmcconnell1 on 4/23/15.
 */
object Accuracy extends App {

    override def main(args: Array[String]): Unit = {
        val client = Client
        val documents: List[SolrDocument] = Client.sortedByDate()

        val train = trainGroupFold(documents, 4, 5)
        val test = testGroupFold(documents, 4, 5)
        println(s"train.length:[${train.length}], test.length:[${test.length}]")

        val (trainMin, trainMax) = minMaxDate(train)
        val (testMin, testMax) = minMaxDate(test)
        println(s"train:[${trainMin}, ${trainMax}] test:[${testMin}, ${testMax}]")

        val predictor = new Predictor(searcher = new Searcher(), train = train, test = test)
        val statistics = new Statistics(data = predictor.cachedData)

        println(s"Accuracy: ${statistics.accuracy * 100} %")
        println(s"Net Percentage Change Per Article: ${statistics.percentageChangePerArticle * 100} %")
        println(s"Aggressive Capture Per Article: ${statistics.aggressiveCapturePerArticle}")

        NetCaptureExporter.export("net_capture_by_date.csv", statistics.captureOverTime)
        NetCaptureExporter.export("aggressive_capture_by_date.csv", statistics.aggressiveCaptureOverTime)
        NetCaptureExporter.export("correct_count_by_date.csv", statistics.correctCountOverTime)
    }

    def trainGroupFold(documents: List[SolrDocument], groupId: Int, groupCount: Int) = {
        if (groupId > groupCount) {
            throw new Exception("groupID must be below groupCount")
        }
        if (groupId < 0 || groupCount < 0) {
            throw new Exception("groupID and groupCount must be positive")
        }
        val trainGroupCount = groupCount - 1
        var trainGroups: mutable.MutableList[SolrDocument] = mutable.MutableList()
        for (offset <- 0 to trainGroupCount - 1) {
            val index = (groupId + offset + 1) % groupCount
            val sliceRange = groupSliceRange(documents, index, groupCount)
            trainGroups ++= documents.slice(sliceRange.start.toInt, sliceRange.end.toInt)
        }
        trainGroups.toList
    }

    def groupSliceRange(list: List[Any], groupIdx: Int, groupCount: Int) = {
        val groupSize = Math.round(list.length.toDouble / groupCount)
        val groupRanges = (0 to groupCount - 1).map(index =>{ ((groupSize * index) to (groupSize * (index + 1) - 1)) })
        groupRanges(groupIdx)
    }

    def testGroupFold(documents: List[SolrDocument], groupId: Int, groupCount: Int) = {
        if (groupId > groupCount) {
            throw new Exception("groupID must be below groupCount")
        }
        if (groupId < 0 || groupCount < 0) {
            throw new Exception("groupID and groupCount must be positive")
        }
        val sliceRange = groupSliceRange(documents, groupId, groupCount)
        documents.slice(sliceRange.start.toInt, sliceRange.end.toInt)
    }

    def minMaxDate(docs: List[SolrDocument]) = {
        (docs.head.get("historyDate").toString(), docs.last.get("historyDate").toString())
    }
}
