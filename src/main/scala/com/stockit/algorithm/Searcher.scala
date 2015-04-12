package com.stockit.algorithm

import weka.core.{Instance, Instances}
import weka.core.neighboursearch.LinearNNSearch


/**
 * Created by jmcconnell1 on 4/12/15.
 */
class Searcher extends LinearNNSearch {
    protected var cached_instances: Instances = null
    protected var k = 5
    setInstances(instances)

    def fetchNeighbors(doc: Instance): Instances = {
        kNearestNeighbours(doc, 5)
    }

    def instances = {
        if (cached_instances == null) {
            cached_instances = Instances
        }
        cached_instances
    }
}
