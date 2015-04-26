package com.stockit.algorithm

import com.github.seratch.scalikesolr.SolrDocument


/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Predictor(searcher: Searcher, train: List[SolrDocument], test: List[SolrDocument]) {
    var data: List[(String, Double, Double)] = Nil

    def historicOutcomes = {
        test.map(doc => {
            percentageChange(doc)
        })
    }

    def predictions = {
        var count = 0
        test.map(doc => {
            val date: String = historicDate(doc)
            val neighbors = searcher.fetchNeighbors(train, doc)
            val percentageChanges = neighbors.map(neighbor => {
                percentageChange(neighbor)
            })
            val mean = arithmeticMean(percentageChanges)
            count += 1
            println(s"Calculated Prediction ${count}")
            (date, mean)
        })
    }

    def results = {
        cachedData
    }

    def cachedData = {
        if (data == Nil) {
            data =  predictions.zip(historicOutcomes).map(
                Function.tupled((result, actual) => {
                    val (date, predicted) = result
                    (date, predicted, actual)
                })
            )
        }
        data
    }

    def historicDate(doc: SolrDocument) = {
        val date = doc.get("historyDate").toString()
        if (date.isEmpty) {
            throw new Exception(s"[$date] is empty")
        }
        date
    }
    
    def percentageChange(doc: SolrDocument): Double = {
        val open = doc.get("open").toDoubleOrElse(0)
        val close = doc.get("close").toDoubleOrElse(0)
        if (close == 0.0 || open == 0.0) {
            println(s"ZeroOpenClose, articleId${doc.get("articleId").toString()}")
            return 0.0
        }
        val delta = close - open
        delta / open
    }

    def arithmeticMean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
        num.toDouble(ts.sum) / ts.size
    }
}
