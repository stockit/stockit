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

    def aggressiveCapture(predictedChange: Double, actualChange: Double): Double = {
        val magnitude = Math.log(predictedChange.abs + Math.E)
        if (predictedChange > 0.0 ) {
            magnitude * actualChange
        } else {
            -magnitude * actualChange
        }
    }

    def totalAggressiveCapture = {
        data.foldLeft(0.0)((sum: Double, datum: (String, Double, Double)) => {
            val (_, predicted, actual) = datum
            sum + aggressiveCapture(predicted, actual)
        })
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
            val (date, prediction, actualChange) = datum
            if (correctPrediction(prediction, actualChange)) {
                sum += 1
            } else {
                sum -= 1
            }
            (date, sum)
        })

    }

    def correctPrediction(prediction: Double, actualChange: Double) = {
        (prediction * actualChange > 0.0) || (prediction == actualChange)
    }

    def guessPositiveMovementCount = {
        data.count((datum: (String, Double, Double)) => {
            val (_, prediction, _) = datum
            prediction > 0.0
        })
    }

    def positiveMovementCount = {
        data.count((datum: (String, Double, Double)) => {
            val (_, _, actualChange) = datum
            actualChange > 0.0
        })
    }

    def correctCount = {
        var count = 0
        data.foreach((datum: (String, Double, Double)) => {
            val (_, prediction, actualChange) = datum
            if (correctPrediction(prediction, actualChange)) {
                count += 1
                // println(s"Correct:[$prediction vs $actualChange, $count]")
            } else {
                // println(s"Incorrect:[$prediction vs $actualChange, $count]")
            }
        })
        count
    }
}
