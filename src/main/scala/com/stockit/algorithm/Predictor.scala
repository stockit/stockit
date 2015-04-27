package com.stockit.algorithm

import org.apache.solr.common.SolrDocument


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
        test.par.map(doc => {
            val date: String = historicDate(doc)
            val neighbors = searcher.fetchNeighbors(train, doc)
            val percentageChanges = neighbors.map(neighbor => {
                percentageChange(neighbor)
            })
            val mean = arithmeticMean(percentageChanges)
            count += 1

            println(s"Calculated Prediction ${count}")
            (date, mean)
        }).seq.toList
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
        val date = doc.get("historyDate").toString
        if (date.isEmpty) {
            throw new Exception(s"[$date] is empty")
        }
        date
    }
    
    def percentageChange(doc: SolrDocument): Double = {
        val change = (doc.getFieldValue("open"), doc.getFieldValue("close")) match {
            case (open: java.lang.Double, close: java.lang.Double) => (close - open) / open
            case _ =>
                println(s"ZeroOpenClose, articleId${doc.getFieldValue("articleId").toString}")
                0.0
        }

        if(change.isNaN || change.isInfinity) 0.0 else change
    }

    def arithmeticMean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
        val result = num.toDouble(ts.sum) / ts.size
        if (result.isNaN) {
           throw new ArithmeticException(s"result is NaN, sum is ${ts.sum}, size is ${ts.size}")
        }
        result
    }
}
