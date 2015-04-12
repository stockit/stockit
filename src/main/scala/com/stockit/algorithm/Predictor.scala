package com.stockit.algorithm

/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Predictor(train: List[AnyVal], test: List[AnyVal]) {
    def accuracy = {
        correct_count / test.size
    }

    def correct_count = {
        predictions.zip(historic_outcomes).count(
            Function.tupled((symbol, percentage_change) => {
                if (symbol == 'positve) {
                    percentage_change > 0.0
                } else {
                    percentage_change <= 0.0
                }
            })
        )
    }

    def historic_outcomes = {
        test.map(doc => {
            // doc.percentage_change
            0.30
        })
    }

    def predictions = {
        test.map(doc => {
            // val neighbors = Knn nearest neighbors
            // val percentage_changes = neighbors.map(neighbor => neighbor.percentage_change)
            // val mean = arithmetic_mean(percentage_changes)
            // if (mean > 0) {
            //  'positive
            // else {
            //  'negative
            // }
            'test
        })
    }

    def arithmetic_mean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
        num.toDouble(ts.sum) / ts.size
    }
}
