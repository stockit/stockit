package com.stockit.exporters

import java.io.{File, PrintWriter}

import com.stockit.statistics.Statistics

/**
 * Created by jmcconnell1 on 4/26/15.
 */
object CompleteStatsExporter {
    def export(fileName: String, data: List[Statistics]) = {
        val result = csvString(data)
        val file = new PrintWriter(new File(fileName))
        file.print(result)
        file.close()
    }

    def csvString(stats: List[Statistics]) = {
        val buffer = new StringBuffer
        val values = valueByDateByFold(stats)
        buffer.append("Date, Real Date, Value\n")
        var counter = 0
        values.foreach((valueByDate) => {
            counter += 1
            buffer.append(s"Fold $counter\n")
            valueByDate.foreach{ case(date, value) =>
                buffer.append(s"$date, ,$value\n")
            }
        })
        buffer.toString
    }

    def valueByDateByFold(data: List[Statistics]): IndexedSeq[Map[String, Double]] = {
        val valueByDateByFold: List[Map[String, List[(String, Double)]]] = data.map((stats) => {
            stats.aggressiveCaptureOverTime.groupBy{ case(date, value) =>
                date
            }
        })
        // Just grab one of the results
        valueByDateByFold.map((resultsByDate) => {
            resultsByDate.map { case (key, value) =>
                value.head
            }
        })
    }
}
