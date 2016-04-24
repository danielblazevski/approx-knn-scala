// z-value based approximate k-nearest neighbors algorithm

package approxknn

import scala.collection.mutable.ArrayBuffer

class zKNN(alpha: Int, gamma: Int) extends approxKNN() {

  def interleave(in: Array[String]): String = {
    // get max length
    // need to change to allow to add/reduce bits
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
   //println((0,Integer.parseInt(interleave(in.map(x => ((Math.pow(2,5)*x).toInt).toBinaryString)), 2)))   
   Integer.parseInt(interleave(in.map(x => x.toInt.toBinaryString)), 2)
  }

  // main zknn query
  def zknnQuery(train: ArrayBuffer[Point], test: ArrayBuffer[Point], k: Int):
  ArrayBuffer[(Point, Array[Point])] = {

    val dim = train.head.length
    val tabArr = Array.tabulate(dim)(x => x)

    val bitMult = (30/dim).floor.toInt

    // normalize points so that they lie in [0,1]^N
    // this guarantees that randomly shifted points lie in [0,2]^N
    val trainMinMax = getNormalizingParameters(train)
    val testMinMax = getNormalizingParameters(test)
    
    val trainingNormalized = normalizePoints(train, trainMinMax._1, trainMinMax._2)
    val testingNormalized = normalizePoints(test, testMinMax._1, testMinMax._2)

    val rSeq = ArrayBuffer.fill(alpha)(ArrayBuffer.fill(dim)(r.nextDouble))

    var res = new ArrayBuffer[(Point, Array[Point])]

      val zTrainSetShiftedSortedFull = rSeq.map{ rVec =>
        trainingNormalized.map{trainPoint => (trainPoint,
        trainPoint.zipWithIndex.map(trainPointZipped =>
          trainPointZipped._1 + rVec(trainPointZipped._2)))
        }.map {
        shiftTrainPoint =>
          (shiftTrainPoint._1, zValue(shiftTrainPoint._2))
        }.sortBy(x => x._2).toArray // array for O(1) access
      }

    for (v <- testingNormalized) {
      var candidatePointsFromZvalue = new ArrayBuffer[Point]
      for (i <- 0 until alpha) {
        val zQueryShifted = zValue(v.zipWithIndex.map{ vZip => vZip._1 + rSeq(i)(vZip._2) })
  
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
      
      res.map { tuple => (denormalizePoint(tuple._1, testMinMax._1, testMinMax._2),
        denormalizePoints(tuple._2, trainMinMax._1, trainMinMax._2))
      }
  }

}
