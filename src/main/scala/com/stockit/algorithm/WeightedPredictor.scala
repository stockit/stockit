package com.stockit.algorithm

import org.apache.solr.common.SolrDocument

/**
 * Created by dmcquill on 5/6/15.
 */
class WeightedPredictor(searcher: Searcher, train: List[SolrDocument], test: List[SolrDocument]) extends Predictor(searcher, train, test) {

    def score(doc: SolrDocument): Double = {
        val score: Double = doc.getFieldValue("score") match {
            case score: java.lang.Double => score
            case _ => 0.0
        }

        if(score.isNaN || score.isInfinity) 0.0 else score
    }

    def weightedMean(sP: Iterable[(Double, Double)]): Double = {
        val sumScores = sP.map(_._1).sum
        sP.map(_._1 / sumScores)
            .zip(sP.map(_._2))
            .map(t => t._1 * t._2)
            .sum
    }

    override def predictions = {
        var count = 0
        test.par.map(doc => {
            val date: String = historicDate(doc)
            val neighbors = searcher.fetchNeighbors(train, doc)
            val scoredPercentageChanges = neighbors.map(neighbor => {
                (score(neighbor), percentageChange(neighbor))
            })
            val mean = weightedMean(scoredPercentageChanges)
            count += 1

            if (count % 100 == 0) println(s"Calculated Prediction ${count}")

            (date, mean)
        }).seq.toList
    }
}
