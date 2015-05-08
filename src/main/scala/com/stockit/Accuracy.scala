package com.stockit

import java.util.Date

import com.stockit.client.{CachingClient}
import com.stockit.algorithm.{WeightedPredictor, Searcher, Predictor}
import com.stockit.exporters.{CompleteStatsExporter, NetCaptureExporter}
import com.stockit.statistics.Statistics
import org.apache.solr.common.SolrDocument

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.reflect.internal.util.Statistics
import scala.util.Random

/**
 * Created by jmcconnell1 on 4/23/15.
 */
object Accuracy extends App {

    def ensureOrderedDocuments(docs: List[SolrDocument]): Unit = {
        var lastDate: Date = null

        val it = docs.iterator
        while(it.hasNext) {
            val doc = it.next
            val curDate: Date = doc.getFieldValue("historyDate").asInstanceOf[Date]

            if(lastDate != null && curDate.before(lastDate)) {
                throw new RuntimeException("documents are out of order")
            }

            lastDate = curDate
        }
    }

    override def main(args: Array[String]): Unit = {
        val client = new CachingClient
        val documents: List[SolrDocument] = client.sortedByDate()
        ensureOrderedDocuments(documents)

        for (((segment1, segment2), index) <- cutPairs(documents, 5).zipWithIndex) {
            val train = segment1
            val test = segment2

            val (trainMin, trainMax) = minMaxDate(train)
            val folds = testFolds(test, 5)

            val stats = folds.zipWithIndex.map{ case(fold, index) =>
                println(s"train.length:[${train.length}], test.length:[${fold.length}]")
                val (foldMin, foldMax) = minMaxDate(fold)
                println(s"train:[${trainMin}, ${trainMax}] test:[${foldMin}, ${foldMax}]")

                val predictor = new WeightedPredictor(searcher = new Searcher(), train = train, test = fold)
                val statistics = new Statistics(data = predictor.results)

                printStats(statistics)
                (statistics, index)
            }

            val statsList = stats.map { case (stats, index) => stats }.toList
            exportFolds(statsList, index)
            stats.foreach{ case(statistics, index) =>
                // exportFiles(index, statistics)
            }

            println(s"Finished Segment: $index")
        }
    }

    def exportFolds(stats: List[Statistics], counter: Int) = {
       CompleteStatsExporter.export(s"results/exp.$counter.csv", stats)
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
        val client = new CachingClient

        val folds = split(Random.shuffle(test), count)
        folds.map((fold) => {
            fold.sortBy((doc) => {
                client.dateOfDoc(doc)
            })
        })
    }

    def cutPairs[T](list: List[T], groups: Int): IndexedSeq[(List[T], List[T])] = {
        val cuts = cut(list, groups)
        cuts.zip(cuts.tail)
    }

    def cut[T](list: List[T], groups: Int): IndexedSeq[List[T]] = {
        var prevSplit = 0
        (0 until groups).map((groupIdx) => {
            val delta = Math.round((list.size - prevSplit.toDouble) / (groups - groupIdx)).asInstanceOf[Int]
            prevSplit += delta
            list.slice(prevSplit - delta, prevSplit)
        })
    }

    def split[T](list: List[T], groups: Int): IndexedSeq[List[T]] = {
        val folds = (0 until groups).map(_ => mutable.MutableList[T]())
        var counter = 0
        list.foreach((element) => {
            val foldIndex = counter % groups
            val fold = folds(foldIndex)
            fold += element
            counter += 1
        })
        folds.map((L) => L.toList)
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
        val dates: List[Date] = docs.map{ doc =>
            doc.getFieldValue("historyDate") match {
                case date:Date => Some(date)
                case _ => None
            }
        }.flatten

        val maxDate = dates.max
        val minDate = dates.min

        (minDate, maxDate)
    }
}
