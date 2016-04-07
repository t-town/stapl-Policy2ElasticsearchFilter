package tom.Policy2Filter.performance

/**
 *    Copyright 2015 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */

import spray.json._
import scala.collection.mutable.ListBuffer

/**
 * Class used for representing a timer that times code evaluation,
 * keeps all values of the test and provides some statistics about
 * these timings.
 *
 * The timer can register timings in two ways:
 * 1. start() and stop()
 * 2. time()
 *
 * These cannot be used in parallel!
 */
class PerformanceResult(label: String = "unnamed-timer") {

  // timings in milliseconds
  val timings = ListBuffer[TimeResult]()
  
  def +=(result: TimeResult) = {
    timings += result
  }


  def count: Int = timings.size

  def mean: Double = {
    if (count == 0) {
      -1
    } else if (count == 1) {
      timings(0).getTotal
    } else {
      grizzled.math.stats.mean(timings.map { x => x.getTotal }: _*)
    }
  }

  def stdDev: Double = grizzled.math.stats.sampleStdDev(timings.map { x => x.getTotal }: _*)

  def total: Double = timings.map { x => x.getTotal }.sum

  /**
   * The size of the confidence interval wrt to the avg.
   *
   * @param d How do you call this?
   */
  def confInt(d: Double = 1.95996398454): Double = {
    // Inverse normal distributions (results of norm.ppf((1 + x)/2.0) in python):
    // x = 0.9 -> 1.64485362695
    // x = 0.91 -> 1.69539771027
    // x = 0.92 -> 1.75068607125
    // x = 0.93 -> 1.81191067295
    // x = 0.94 -> 1.88079360815
    // x = 0.95 -> 1.95996398454
    // x = 0.96 -> 2.05374891063
    // x = 0.97 -> 2.17009037758
    // x = 0.98 -> 2.32634787404
    // x = 0.99 -> 2.57582930355
    // x = 0.999 -> 3.29052673149
    val confIntervalLow = mean - (d * stdDev / math.sqrt(count))
    val confIntervalHigh = mean + (d * stdDev / math.sqrt(count))
    val confIntervalSizeAbs = confIntervalHigh - confIntervalLow
    confIntervalSizeAbs / mean
  }
  


  def reset = {
    timings.clear
  }

  override def toString(): String = {
    return f"$label: nbruns = $count, mean = $mean%2.2f ms, confInt = ${confInt() * 100}%2.2f%%"
  }

  def getJson = JsObject(
    "nbReceived" -> JsNumber(count),
    "mean" -> JsNumber(mean),
    "confInt" -> JsNumber(confInt()),
    "stdev" -> JsNumber(stdDev),
    "totals" -> JsArray(totalTimings.map(x => JsNumber(x)).toList),
    "serverDuration" -> JsArray(timings.map(x => JsNumber(x.durationServer)).toList),
    "otherDuration" -> JsArray(timings.map(x => JsNumber(x.durationRest)).toList))

  def totalTimings: ListBuffer[Double] = timings.map(x => x.getTotal)
  def printAllMeasurements() = {
    timings.foreach(println(_))
  }

  def printHistogram(): Unit = {
    val timings2 = timings.map { x => x.getTotal }
    val max = timings2.foldLeft(0.0)((a, b) => math.max(a, b))
    if(max <= 0) { // this can happen in case we don't care for latencies but still use this timer 
      return
    }
    //val min = timings.foldLeft(Double.MaxValue)((a,b) => math.min(a,b))
    val min = 0
    val nbBins = 10
    val binSize = (max - min)/nbBins

    // put the values in bins
    val bins = ListBuffer[Int]()
    (0 to nbBins).foreach(x => bins.append(0))
    timings2.foreach(x => {
      val index = math.floor((x - min) / binSize).toInt
      bins(index) = bins(index) + 1
    })

    // print the values
    val nbCharacters = 50
    val maxBin = bins.foldLeft(0)((a, b) => math.max(a, b))
    val characterSize = (maxBin.toDouble / nbCharacters.toDouble).ceil
    val intervalSize = f"[${(nbBins - 1) * binSize + min},${nbBins * binSize + min})".size
    val labelSize = s"${maxBin}".size
    for (i <- 0 until nbBins) {
      val value = bins(i)
      val lowerBound = i * binSize + min
      val upperBound = (i + 1) * binSize + min
      val interval = f"[$lowerBound,$upperBound)".padTo(intervalSize, ' ')
      val label = s"$value".padTo(labelSize, ' ')
      val bar = "".padTo(math.round((value - min) / characterSize).toInt, '=')
      println(s"$interval | $label | $bar")
    }
  }
  
}

class TimeResult {
	var durationServer = 0.0;
	var durationRest = 0.0;
	def getTotal: Double = {
			return durationServer + durationRest
	}
}
