package com.stockit.algorithm

import com.github.seratch.scalikesolr.SolrDocument
import weka.core.{Instance, Instances}

import scala.collection.mutable

/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Predictor(searcher: Searcher, train: List[SolrDocument], test: List[SolrDocument]) {
    def accuracy = {
        correctCount / test.size
    }

    def correctCount = {
        predictions.zip(historicOutcomes).count(
            Function.tupled((symbol, percentageChange) => {
                if (symbol == 'positve) {
                    if (percentageChange > 0.0) {
                        println("correct")
                        true
                    } else {
                        false
                    }
                } else {
                    if (percentageChange <= 0.0) {
                        println("correct")
                        true
                    } else {
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
            val neighbors = searcher.fetchNeighbors(doc)
            val percentageChanges = neighbors.map(neighbor => {
                percentageChange(neighbor)
            })
            val mean = arithmeticMean(percentageChanges)
            if (mean > 0.0) {
                'positive
            } else {
                'negative
            }
        })
    }
    
    def percentageChange(doc: SolrDocument) = {
        val open = doc.get("open").toDoubleOrElse(0)
        val close = doc.get("open").toDoubleOrElse(0)
        val delta = close - open
        delta / ((open + close) / 2)
    }

    def arithmeticMean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
        num.toDouble(ts.sum) / ts.size
    }
}
