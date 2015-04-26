package com.stockit.algorithm

import java_cup.symbol

import com.github.seratch.scalikesolr.SolrDocument
import weka.core.{Instance, Instances}

import scala.collection.mutable

/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Predictor(searcher: Searcher, train: List[SolrDocument], test: List[SolrDocument]) {
    var data: List[(Symbol, Double, Double)] = Nil

    def accuracy = {
        correctCount / test.size.toDouble
    }

    def percentageChangePerArticle = {
        totalPercentageChange / test.size.toDouble
    }

    def cachedData = {
        if (data == Nil) {
            data =  predictions.zip(historicOutcomes).map(
                Function.tupled((result, percentageChange) => {
                    val (symbol, mean) = result
                    (symbol, mean, percentageChange)
                })
            )
        }
        data
    }

    def totalPercentageChange = {
        cachedData.foldLeft(0.0)((sum: Double, datum: (Symbol, Double, Double)) => {
            val (symbol, predicted, actual) = datum
            val lossOrGain = actual.abs
            if (symbol == 'positve) {
                if (actual > 0.0) {
                    sum + lossOrGain
                } else {
                    sum - lossOrGain
                }
            } else {
                if (actual <= 0.0) {
                    sum + lossOrGain
                } else {
                    sum - lossOrGain
                }
            }
        })
    }

    def correctCount = {
        cachedData.count((datum: (Symbol, Double, Double)) => {
            val (symbol, predicted, actual) = datum
            if (symbol == 'positve) {
                if (actual > 0.0) {
                    true
                } else {
                    false
                }
            } else {
                if (actual <= 0.0) {
                    true
                } else {
                    false
                }
            }
        })
    }

    def historicOutcomes = {
        train.map(doc => {
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
