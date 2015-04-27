package com.stockit.statistics

/**
 * Created by jmcconnell1 on 4/25/15.
 */
class Statistics(data: List[(String, Symbol, Double, Double)]) {
    def accuracy = {
        correctCount / data.size.toDouble
    }

    def percentageChangePerArticle = {
        totalPercentageChange / data.size.toDouble
    }

    def aggressiveCapturePerArticle = {
        totalAggressiveCapture / data.size.toDouble
    }

    def aggressiveCapture(prediction: Symbol, predictedChange: Double, actualChange: Double): Double = {
        100 * predictedChange * actualChange
    }

    def totalAggressiveCapture = {
        val arr = data.map((datum: (String, Symbol, Double, Double)) => {
            val (_, symbol, predicted, actual) = datum
            aggressiveCapture(symbol, predicted, actual)
        })

        arr.sum
    }

    def aggressiveCaptureOverTime = {
        var sum = 0.0
        data.map((datum: (String, Symbol, Double, Double)) => {
            val (date, symbol, predicted, actual) = datum
            sum += aggressiveCapture(symbol, predicted, actual)
            (date, symbol, sum)
        })

    }

    def captureOverTime = {
        var sum = 0.0
        data.map((datum: (String, Symbol, Double, Double)) => {
            val (date, symbol, predicted, actual) = datum
            sum += percentageChangeCaptured(symbol, predicted, actual)
            (date, symbol, sum)
        })
    }

    def percentageChangeCaptured(prediction: Symbol, predictedChange: Double, actualChange: Double): Double = {
        if (prediction == 'positve) {
            actualChange
        } else {
            -actualChange
        }
    }

    def totalPercentageChange = {
        data.foldLeft(0.0)((sum: Double, datum: (String, Symbol, Double, Double)) => {
            val (_, symbol, predicted, actual) = datum
            sum + percentageChangeCaptured(symbol, predicted, actual)
        })
    }

    def correctCountOverTime = {
        var sum = 0.0
        data.map((datum: (String, Symbol, Double, Double)) => {
            val (date, prediction, _, actualChange) = datum
            if (correctPrediction(prediction, actualChange)) {
                sum += 1
            } else {
                sum -= 1
            }
            (date, prediction, sum)
        })

    }

    def correctPrediction(prediction: Symbol, actualChange: Double) = {
        if (prediction == 'positve) {
            actualChange > 0.0
        } else {
            actualChange <= 0.0
        }
    }

    def correctCount = {
        data.count((datum: (String, Symbol, Double, Double)) => {
            val (date, prediction, _, actualChange) = datum
            correctPrediction(prediction, actualChange)
        })
    }
}
