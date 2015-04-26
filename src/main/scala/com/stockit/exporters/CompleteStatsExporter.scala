package com.stockit.exporters

import java.io.{File, PrintWriter}

import com.stockit.statistics.Statistics

import scala.collection.immutable.Iterable

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

    def flattenStats(count: List[Map[String, Double]], capture: List[Map[String, Double]], aggCapture: List[Map[String, Double]]) = {
        count.zipWithIndex.map{ case(countByDate, index) =>
            countByDate.map{ case(date, count) =>
                (date, count, capture(index)(date), aggCapture(index)(date))
            }
        }
    }

    def csvString(stats: List[Statistics]) = {
        val buffer = new StringBuffer
        val count = netCountByDateByFold(stats)
        val capture =  captureByDateByFold(stats)
        val aggCapture = aggCaptureByDateByFold(stats)
        val values = flattenStats(count, capture, aggCapture)
        buffer.append("Date, Real Date, Net Count, Capture, Aggressive Capture\n")
        var counter = 0
        values.foreach((valueByDate) => {
            counter += 1
            buffer.append(s"Fold $counter\n")
            valueByDate.foreach{ case(date, count, capture, aggCapture) =>
                buffer.append(s"$date, ,$count, $capture, $aggCapture\n")
            }
        })
        buffer.toString
    }

    def captureByDateByFold(data: List[Statistics]): List[Map[String, Double]] = {
        data.map((stats) => {
            val valuesByDate = stats.captureOverTime.groupBy{ case(date, value) =>
                date
            }
            valuesByDate.map { case (key, value) =>
                value.head
            }
        })
    }

    def netCountByDateByFold(data: List[Statistics]): List[Map[String, Double]] = {
        data.map((stats) => {
            val valuesByDate = stats.correctCountOverTime.groupBy{ case(date, value) =>
                date
            }
            valuesByDate.map { case (key, value) =>
                value.head
            }
        })
    }

    def aggCaptureByDateByFold(data: List[Statistics]): List[Map[String, Double]] = {
        data.map((stats) => {
            val valuesByDate = stats.aggressiveCaptureOverTime.groupBy{ case(date, value) =>
                date
            }
            valuesByDate.map { case (key, value) =>
                value.head
            }
        })
    }
}
