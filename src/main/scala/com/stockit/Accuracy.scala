package com.stockit

import com.github.seratch.scalikesolr.SolrDocument
import com.stockit.client.Client
import com.stockit.algorithm.{Searcher, Predictor}
import com.stockit.exporters.{CompleteStatsExporter, NetCaptureExporter}
import com.stockit.statistics.Statistics

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.reflect.internal.util.Statistics

/**
 * Created by jmcconnell1 on 4/23/15.
 */
object Accuracy extends App {

    override def main(args: Array[String]): Unit = {
        val client = Client
        val documents: List[SolrDocument] = Client.sortedByDate()

        val train = trainGroupFold(documents, 1, 2)
        val test = testGroupFold(documents, 1, 2)

        val (trainMin, trainMax) = minMaxDate(train)
        val folds = testFolds(test, 5)

        val stats = folds.zipWithIndex.map{ case(fold, index) =>
            println(s"train.length:[${train.length}], test.length:[${fold.length}]")
            val (foldMin, foldMax) = minMaxDate(fold)
            println(s"train:[${trainMin}, ${trainMax}] test:[${foldMin}, ${foldMax}]")

            val predictor = new Predictor(searcher = new Searcher(), train = train, test = fold)
            val statistics = new Statistics(data = predictor.results)

            printStats(statistics)
            (statistics, index)
        }

        val statsList = stats.map { case (stats, index) => stats }.toList
        exportFolds(statsList)
        stats.foreach{ case(statistics, index) =>
            exportFiles(index, statistics)
        }
    }

    def exportFolds(stats: List[Statistics]) = {
       CompleteStatsExporter.export(s"results/experiment.csv", stats)
    }


    def printStats(statistics: Statistics) = {
        println(s"Positive Movement Likelihood: ${statistics.positiveMovementLikelihood * 100} %")
        println(s"Model's Positive Movement Likelihood: ${statistics.guessPositiveMovementLikelihood * 100} %")
        println(s"Accuracy: ${statistics.accuracy * 100} %")
        println(s"Net Percentage Change Per Article: ${statistics.percentageChangePerArticle * 100} %")
        println(s"Aggressive Capture Per Article: ${statistics.aggressiveCapturePerArticle}")
    }

    def exportFiles(index: Int, statistics: Statistics) = {
        NetCaptureExporter.export(s"results/fold_$index/net_capture_by_date.csv", statistics.captureOverTime)
        NetCaptureExporter.export(s"results/fold_$index/aggressive_capture_by_date.csv", statistics.aggressiveCaptureOverTime)
        NetCaptureExporter.export(s"results/fold_$index/correct_count_by_date.csv", statistics.correctCountOverTime)
    }

    def testFolds(test: List[SolrDocument], count: Int) = {
        val groupSize = Math.round(test.length.toDouble / count)
        (0 until count).map((index) => {
            val start = index * groupSize
            val end = (index + 1) * groupSize
            test.slice(start.toInt, end.toInt)
        })
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
