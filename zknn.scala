// z-knn

package zknn

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ArrayBuffer

class zknn(alpha: Int, gamma: Int) {

  val r = scala.util.Random

  type Point = Array[Double]

  def interleave(in: Array[String]): String = {
    // get max length
    val maxLen = in.map(str => str.length).max
    val L = in.length
    var res = ""

    for (i <- 0 until maxLen) {
      for (j <- 0 until L) {
        if (i >= in(L - 1 - j).length) {
          res = 0 + res
        } else {
          res = in(L - 1 - j)(in(L - 1 - j).length - 1 - i) + res
        }
      }
    }
    res
  }

  def zValue(in: Point): Int = {
   Integer.parseInt(interleave(in.map(x => x.toInt.toBinaryString)), 2)
  }

  def distance(a: Point, b: Point): Double = {
    math.sqrt(a.zipWithIndex.map { x =>
      (x._1 - b(x._2)) * (x._1 - b(x._2))
    }.sum)
  }

  def basicknnQuery(train: ArrayBuffer[Point], test: ArrayBuffer[Point], k: Int):
  ArrayBuffer[(Point, Array[Point])] = {

    val queue = new PriorityQueue[ (Point, Double) ]()(Ordering.by(x => x._2))
    val out = new ArrayBuffer[ (Point, Array[Point]) ]

    for (testPoint <- test) {      
      val outSingle = new Array[Point](k)
      for (trainPoint <- train) {
        // (training vector, input vector, input key, distance)
        queue.enqueue((trainPoint, distance(testPoint, trainPoint)))
        if (queue.size > k) {
          queue.dequeue()
        }
      }
      for (i <- 0 until k) {
        outSingle(i) = queue.dequeue()._1
      }
      out += ((testPoint, outSingle))
    }   
  return out 
  }

  def basicknnQuerySingleTest(train: ArrayBuffer[Point], test: Point, k: Int):
  (Point, Array[Point]) = {
    basicknnQuery(train, ArrayBuffer(test), k).head
 }

  // main zknn query
  def zknnQuery(train: ArrayBuffer[Point], test: ArrayBuffer[Point], k: Int):
  ArrayBuffer[(Point, Array[Point])] = {

    // shift points to make sure all entries are positive (what about random shifts?)
    val rSeq = ArrayBuffer.fill(alpha)(ArrayBuffer.fill(train.head.length)(r.nextDouble))

    var res = new ArrayBuffer[(Point, Array[Point])]

      val zTrainSetShiftedSortedFull = rSeq.map{ rVec =>
        train.map{trainPoint => (trainPoint,
        trainPoint.zipWithIndex.map(trainPointZipped =>
          trainPointZipped._1 - rVec(trainPointZipped._2)))
        }.map {
        shiftTrainPoint =>
          (shiftTrainPoint._1, zValue(shiftTrainPoint._2))
        }.sortBy(x => x._2).toArray // array for O(1) access
      }

    for (v <- test) {
      var candidatePointsFromZvalue = new ArrayBuffer[Point]
      for (i <- 0 until alpha) {
        val zQueryShifted = zValue(v.zipWithIndex.map { vZip => vZip._1 - rSeq(i)(vZip._2) })
  
        // get 2*gamma points about query point q, gamma points above and below based on z value
        // if there aren't gamma points above, still grab 2*gamma points
        if (zQueryShifted < zTrainSetShiftedSortedFull(i).head._2){
            candidatePointsFromZvalue ++= zTrainSetShiftedSortedFull(i).slice(0,2*gamma).map(x => x._1)
          } else if (zQueryShifted > zTrainSetShiftedSortedFull(i)(zTrainSetShiftedSortedFull(i).length - 1)._2 ){
            candidatePointsFromZvalue ++= zTrainSetShiftedSortedFull(i).slice(
              zTrainSetShiftedSortedFull(i).length - 1 - 2*gamma, zTrainSetShiftedSortedFull(i).length).map(x => x._1)
          } else {

        val index = getIndexSortedList(zTrainSetShiftedSortedFull(i), (v,zQueryShifted))

        val posLen = zTrainSetShiftedSortedFull(i).length - index
        val negLen = index

        if (posLen >= gamma && negLen >= gamma) {
          candidatePointsFromZvalue ++=  zTrainSetShiftedSortedFull(i).slice(i - gamma,i + gamma).map(x => x._1)
        } else if (posLen < gamma && posLen + negLen >= 2*gamma) {
          candidatePointsFromZvalue ++= zTrainSetShiftedSortedFull(i).slice(i - 2*gamma - posLen, i + posLen).map(x => x._1)
        } else if (negLen < gamma && posLen + negLen >= 2*gamma) {
          candidatePointsFromZvalue ++= zTrainSetShiftedSortedFull(i).slice(i - negLen,i + 2*gamma - negLen).map(x => x._1)
        } else {
          throw new IllegalArgumentException(s" Error: gamma is too large!")
        }  
      }
    }
      res += basicknnQuerySingleTest(candidatePointsFromZvalue, v, k)
    }
     res
  }

  def getIndexSortedList (list: Array[(Point, Int)], P: (Point, Int)): Int = {
    getIndexHelper(list, P, 0, list.length-1)
  }

  def getIndexHelper (list: Array[(Point, Int)], P: ((Point, Int)), low: Int, high: Int): Int = {
    val i = ((high + low)/2).floor.toInt
    if ( P._2 >= list(i)._2 && P._2 <= list(i + 1)._2 ) {
      return i
    } else if ( P._2 < list(i)._2 ) {
      getIndexHelper(list, P, low, i)
    } else  {
      getIndexHelper(list, P, i, high)
    }
  }
}
