package com.stockit.algorithm

import weka.core.{Instance, Instances}

import scala.collection.mutable

/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Predictor(searcher: Searcher, train: Instances, test: Instances, trainMeta: Any, testMeta: Any) {
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
        map_instances(test, doc => {
            0.3 // testMeta[index]['percentage_change]
        })
    }

    def predictions = {
        map_instances(test, doc => {
            val neighbors = searcher.fetchNeighbors(doc)
            val percentage_changes = map_instances(neighbors, instance => {
                0.3 // testMeta[index]['percentage_change]
            })
            val mean = arithmetic_mean(percentage_changes)
            if (mean > 0.0) {
                'positive
            } else {
                'negative
            }
        })
    }

    def map_instances[T](instances: Instances, func: (Instance => T)) = {
        var list: mutable.MutableList[T] = mutable.MutableList()
        for(x <- 0 until instances.size) {
            val instance = instances.get(x)
            list += func(instance)
        }
        list
    }

    def arithmetic_mean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
        num.toDouble(ts.sum) / ts.size
    }
}
