package com.stockit.algorithm

import com.github.seratch.scalikesolr.SolrDocument
import weka.core.{Instance, Instances}

import scala.collection.mutable

/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Predictor(searcher: Searcher, train: List[SolrDocument], test: List[SolrDocument]) {
    def accuracy = {
        correctCount / test.size.toDouble
    }

    def results = {
        val (classifications, outcomes) = (predictions, historicOutcomes)
        var correctCount, score = 0.0
    }

    def correctCount = {
        predictions.zip(historicOutcomes).count(
            Function.tupled((result, percentageChange) => {
                val (symbol, mean) = result
                if (symbol == 'positve) {
                    if (percentageChange > 0.0) {
                        println(s"correct:[${symbol}, ${mean} vs ${percentageChange}]")
                        true
                    } else {
                        println(s"incorrect:[${symbol}, ${mean} vs ${percentageChange}]")
                        false
                    }
                } else {
                    if (percentageChange <= 0.0) {
                        println(s"correct:[${symbol}, ${mean} vs ${percentageChange}]")
                        true
                    } else {
                        println(s"incorrect:[${symbol}, ${mean} vs ${percentageChange}]")
                        false
                    }
                }
            })
        )
    }

    def historicOutcomes = {
        test.map(doc => {
            percentageChange(doc)
        })
    }

    def predictions = {
        test.map(doc => {
            val neighbors = searcher.fetchNeighbors(train, doc)
            val percentageChanges = neighbors.map(neighbor => {
                percentageChange(neighbor)
            })
            val mean = arithmeticMean(percentageChanges)
            if (mean > 0.0) {
                ('positive, mean)
            } else {
                ('negative, mean)
            }
        })
    }
    
    def percentageChange(doc: SolrDocument): Double = {
        val open = doc.get("open").toDoubleOrElse(0)
        val close = doc.get("close").toDoubleOrElse(0)
        if (close == 0.0 || open == 0.0) {
            println(s"ZeroOpenClose, articleId${doc.get("articleId").toString()}")
            return 0.0
        }
        val delta = close - open
        delta / ((open + close) / 2)
    }

    def arithmeticMean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
        num.toDouble(ts.sum) / ts.size
    }
}
