package com.stockit.statistics

/**
 * Created by jmcconnell1 on 4/25/15.
 */
class Statistics(data: List[(String, Double, Double)]) {
    def accuracy = {
        correctCount / data.size.toDouble
    }

    def positiveMovementLikelihood = {
        positiveMovementCount / data.size.toDouble
    }

    def guessPositiveMovementLikelihood = {
        guessPositiveMovementCount / data.size.toDouble
    }

    def percentageChangePerArticle = {
        totalPercentageChange / data.size.toDouble
    }

    def aggressiveCapturePerArticle = {
        totalAggressiveCapture / data.size.toDouble
    }

    def aggressiveCapture(predicted: Double, actual: Double): Double = {
        val magnitude = Math.log(predicted.abs + Math.E)
        if (magnitude.isNaN) {
            throw new ArithmeticException(s"mag is NaN, predicted.abs is ${predicted.abs}, magnitude is $magnitude")
        }
        if (predicted > 0.0 ) {
            val result = magnitude * actual
            if (result.isNaN) {
                throw new ArithmeticException(s"result is NaN result $result, actual $actual")
            }
            result
        } else {
            val result = -magnitude * actual
            if (result.isNaN) {
                throw new ArithmeticException(s"result is NaN result $result, actual $actual")
            }
            result
        }
    }

    def totalAggressiveCapture = {
        val arr = data.map((datum: (String, Double, Double)) => {
            val (_, predicted, actual) = datum
            aggressiveCapture(predicted, actual)
        })

        arr.sum
    }

    def aggressiveCaptureOverTime = {
        var sum = 0.0
        data.map((datum: (String, Double, Double)) => {
            val (date, predicted, actual) = datum
            sum += aggressiveCapture(predicted, actual)
            (date, sum)
        })

    }

    def captureOverTime = {
        var sum = 0.0
        data.map((datum: (String, Double, Double)) => {
            val (date, predicted, actual) = datum
            sum += percentageChangeCaptured(predicted, actual)
            (date, sum)
        })
    }

    def percentageChangeCaptured(predicted: Double, actual: Double): Double = {
        if (correctPrediction(predicted, actual)) {
            actual.abs
        } else {
            -actual.abs
        }
    }

    def totalPercentageChange = {
        data.foldLeft(0.0)((sum: Double, datum: (String, Double, Double)) => {
            val (_, predicted, actual) = datum
            sum + percentageChangeCaptured(predicted, actual)
        })
    }

    def correctCountOverTime = {
        var sum = 0.0
        data.map((datum: (String, Double, Double)) => {
            val (date, prediction, actual) = datum
            if (correctPrediction(prediction, actual)) {
                sum += 1
            } else {
                sum -= 1
            }
            (date, sum)
        })

    }

    def correctPrediction(prediction: Double, actual: Double) = {
        (prediction * actual > 0.0) || (prediction == actual)
    }

    def guessPositiveMovementCount = {
        data.count((datum: (String, Double, Double)) => {
            val (_, prediction, _) = datum
            prediction > 0.0
        })
    }

    def positiveMovementCount = {
        data.count((datum: (String, Double, Double)) => {
            val (_, _, actual) = datum
            actual > 0.0
        })
    }

    def correctCount = {
        var count = 0
        data.foreach((datum: (String, Double, Double)) => {
            val (_, prediction, actual) = datum
            if (correctPrediction(prediction, actual)) {
                count += 1
                // println(s"Correct:[$prediction vs $actual, $count]")
            } else {
                // println(s"Incorrect:[$prediction vs $actual, $count]")
            }
        })
        count
    }
}
