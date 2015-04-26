package com.stockit.exporters

import java.io.{File, PrintWriter}

import scala.util.Sorting


/**
 * Created by jmcconnell1 on 4/25/15.
 */
object NetCaptureExporter {
    def export(fileName: String, data: List[(String, Symbol, Double)]) = {
        val result = csvString(data)
        val file = new PrintWriter(new File(fileName))
        file.print(result)
        file.close()
    }

    def csvString(data: List[(String, Symbol, Double)]) = {
        val buffer = new StringBuffer
        val netByDate = netCaputureByDate(data)
        buffer.append("Date, Real Date, Net Capture\n")
        netByDate.foreach((datum) => {
            val (date, net) = datum
            buffer.append(s"$date, , $net\n")
        })
        buffer.toString
    }

    def netCaputureByDate(data: List[(String, Symbol, Double)]) = {
        val dataByDate = data.groupBy((datum) => {
            val (date, _, _) = datum
            date
        })
        // Just grab one of the results
        dataByDate.map((dataGroup) => {
            val (date, group) = dataGroup
            val (_, _, capture) = group.head
            (date, capture * 100)
        })
    }
}
