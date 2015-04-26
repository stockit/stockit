package com.stockit.algorithm

import java_cup.symbol

import com.github.seratch.scalikesolr.SolrDocument
import weka.core.{Instance, Instances}

import scala.collection.mutable

/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Predictor(searcher: Searcher, train: List[SolrDocument], test: List[SolrDocument]) {
    var data: List[(String, Symbol, Double, Double)] = Nil

    def accuracy = {
        correctCount / test.size.toDouble
    }

    def percentageChangePerArticle = {
        totalPercentageChange / test.size.toDouble
    }

    def captureOverTime = {
        var sum = 0.0
        cachedData.map((datum: (String, Symbol, Double, Double)) => {
            val (date, symbol, predicted, actual) = datum
            sum += percentageChangeCaptured(symbol, predicted, actual)
            (date, symbol, sum)
        })

    }

    def percentageChangeCaptured(symbol: Symbol, predicted: Double, actual: Double): Double = {
        if (symbol == 'positve) {
            if (actual > 0.0) {
                actual
            } else {
                -actual
            }
        } else {
            if (actual <= 0.0) {
                -actual
            } else {
                actual
            }
        }
    }


    def totalPercentageChange = {
        cachedData.foldLeft(0.0)((sum: Double, datum: (String, Symbol, Double, Double)) => {
            val (date, symbol, predicted, actual) = datum
            sum + percentageChangeCaptured(symbol, predicted, actual)
        })
    }

    def correctCount = {
        cachedData.count((datum: (String, Symbol, Double, Double)) => {
            val (date, symbol, predicted, actual) = datum
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
            if (mean > 0.0) {
                (date, 'positive, mean)
            } else {
                (date, 'negative, mean)
            }
        })
    }

    def cachedData = {
        if (data == Nil) {
            data =  predictions.zip(historicOutcomes).map(
                Function.tupled((result, percentageChange) => {
                    val (date, symbol, mean) = result
                    (date, symbol, mean, percentageChange)
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
        delta / ((open + close) / 2)
    }

    def arithmeticMean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
        num.toDouble(ts.sum) / ts.size
    }
}
