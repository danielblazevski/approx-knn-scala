// z-knn

package zknn

import scala.collection.mutable.ListBuffer

class zknn(alpha: Int, gamma: Int) {

  val r = scala.util.Random

  type Point = ListBuffer[Double]

  def interleave(in: ListBuffer[String]): String = {
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

  def basicknnQuery(train: ListBuffer[Point], test: ListBuffer[Point], k: Int):
  Seq[(Point, Array[Point])] = {
    test.map { v => (v,
      train.map {
        x => (x, distance(v, x))
      }.sortBy(_._2).take(k).map(_._1).toArray)
    }
  }

  def basicknnQuerySingleTest(train: ListBuffer[Point], test: Point, k: Int):
  (Point, Array[Point]) = {
    (test,
      train.map {
        x => (x, distance(test, x))
      }.sortBy(_._2).take(k).map(_._1).toArray)
    }

  // main zknn query
  def zknnQuery(train: ListBuffer[Point], test: ListBuffer[Point], k: Int):
  ListBuffer[(Point, Array[Point])] = {

    // shift points to make sure all entries are positive (what about random shifts?)

    var candidatePointsFromZvalue = new ListBuffer[Point]
    val rSeq = Seq.fill(alpha)(Seq.fill(train.head.length)(r.nextDouble))

    var res = new ListBuffer[(Point, Array[Point])]

      val zTrainSetShiftedSortedFull = rSeq.map{ rVec =>
        train.map{trainPoint => (trainPoint,
        trainPoint.zipWithIndex.map(trainPointZipped =>
          trainPointZipped._1 - rVec(trainPointZipped._2)))
        }.map {
        shiftTrainPoint =>
          (shiftTrainPoint._1, zValue(shiftTrainPoint._2))
        }.sortBy(x => x._2)
      }

      var count = 0
    for (v <- test) {
      for (i <- 0 until alpha) {
        val zQueryShifted = zValue(v.zipWithIndex.map { vZip => vZip._1 - rSeq(i)(vZip._2) })

        val zTrainSetShiftedSorted = zTrainSetShiftedSortedFull(i)
  
        // get 2*gamma points about query point q, gamma points above and below based on z value
        // if there aren't gamma points above, still grab 2*gamma points
        val zTrainSetShiftedByZTrain = zTrainSetShiftedSorted.map { tuple =>
          (tuple._1, tuple._2 - zQueryShifted)
        }

        val posFilter = zTrainSetShiftedByZTrain.filter(x => x._2 > 0)
        val negFilter = zTrainSetShiftedByZTrain.filter(x => x._2 < 0)

        if (posFilter.length >= gamma && negFilter.length >= gamma) {
          candidatePointsFromZvalue ++=  posFilter.take(gamma).map(x => x._1)
          candidatePointsFromZvalue ++=  negFilter.take(gamma).map(x => x._1)
        } else if (posFilter.length < gamma && posFilter.length + negFilter.length >= 2*gamma) {
          candidatePointsFromZvalue ++= posFilter.take(posFilter.length).map(x => x._1)
          candidatePointsFromZvalue ++= negFilter.take(2*gamma - posFilter.length).map(x => x._1)
        } else if (negFilter.length < gamma && posFilter.length + negFilter.length >= 2*gamma) {
          candidatePointsFromZvalue ++= negFilter.take(negFilter.length).map(x => x._1)
          candidatePointsFromZvalue ++= posFilter.take(2*gamma - negFilter.length).map(x => x._1)
      } else {
          throw new IllegalArgumentException(s" Error: gamma is too large!")
        }

      res += basicknnQuerySingleTest(candidatePointsFromZvalue, v, k)
      }
      candidatePointsFromZvalue.clear
    }
     res
  }

}
