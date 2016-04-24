// z-value based approximate k-nearest neighbors algorithm

package approxknn

import scala.collection.mutable.ArrayBuffer

class zKNN(alpha: Int, gamma: Int) extends approxKNN() {

  // main zknn query
  def zknnQuery(train: ArrayBuffer[Point], test: ArrayBuffer[Point], k: Int):
  ArrayBuffer[(Point, Array[Point])] = {

    val dim = train.head.length
    val tabArr = Array.tabulate(dim)(x => x)

    val bitMult = (30/dim).floor.toInt - 1

    // normalize points so that they lie in [0,1]^N
    // this guarantees that randomly shifted points lie in [0,2]^N
    val trainMinMax = getNormalizingParameters(train)
    val testMinMax = getNormalizingParameters(test)
    
    val trainingNormalized = normalizePoints(train, trainMinMax._1, trainMinMax._2)
    val testingNormalized = normalizePoints(test, testMinMax._1, testMinMax._2)

    val rSeq = ArrayBuffer.fill(alpha)(ArrayBuffer.fill(dim)(r.nextDouble))

    // for each random vector, shifts the trainging set and gets z-values
    // that is, tuples of the form: (trainShifted, zValue(trainShifted)) 
    val zTrainShiftedSorted = rSeq.map{ rVec =>
        trainingNormalized.map{P => (P,
        P.zipWithIndex.map(trainZip =>
          trainZip._1 + rVec(trainZip._2)))
        }.map { shifted =>
          (shifted._1, zValue(shifted._2.map(x=>Math.pow(2,bitMult))))
        }.sortBy(x => x._2).toArray // array for O(1) access
      }

    var res = new ArrayBuffer[(Point, Array[Point])]

    for (v <- testingNormalized) {
      var candidatePoints = new ArrayBuffer[Point]
      for (i <- 0 until alpha) {
        val zQueryShifted = zValue(v.zipWithIndex.map{ vZip => vZip._1 + rSeq(i)(vZip._2) }
          .map(x=>Math.pow(2,bitMult)))
  
        // get 2*gamma points about query point q, gamma points above and below based on z value
        // if there aren't gamma points above, still grab 2*gamma points
        if (zQueryShifted < zTrainShiftedSorted(i).head._2){
            candidatePoints ++= zTrainShiftedSorted(i).slice(0,2*gamma).map(x => x._1)
          } else if (zQueryShifted > zTrainShiftedSorted(i)(zTrainShiftedSorted(i).length - 1)._2 ){
            candidatePoints ++= zTrainShiftedSorted(i).slice(
              zTrainShiftedSorted(i).length - 1 - 2*gamma, zTrainShiftedSorted(i).length).map(x => x._1)
          } else {

        val index = getIndexSortedList(zTrainShiftedSorted(i), (v,zQueryShifted))

        val posLen = zTrainShiftedSorted(i).length - index
        val negLen = index

        if (posLen >= gamma && negLen >= gamma) {
          candidatePoints ++=  zTrainShiftedSorted(i).slice(i - gamma,i + gamma).map(x => x._1)
        } else if (posLen < gamma && posLen + negLen >= 2*gamma) {
          candidatePoints ++= zTrainShiftedSorted(i).slice(i - 2*gamma - posLen, i + posLen).map(x => x._1)
        } else if (negLen < gamma && posLen + negLen >= 2*gamma) {
          candidatePoints ++= zTrainShiftedSorted(i).slice(i - negLen,i + 2*gamma - negLen).map(x => x._1)
        } else {
          throw new IllegalArgumentException(s" Error: gamma is too large!")
        }  
      }
    }
      res += basicknnQuerySingleTest(candidatePoints, v, k)
    }
      // denormalize result (Point, Array[Point])
      res.map { tuple => (denormalizePoint(tuple._1, testMinMax._1, testMinMax._2),
        denormalizePoints(tuple._2, trainMinMax._1, trainMinMax._2))
      }
  }


  // Helper functions to compute the z-value, normalize points and do a binary search
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

  // get parameters for normalizing data
  def getNormalizingParameters(vec: ArrayBuffer[Point]):  /// NEED TO FIX THIS
    (Point, Point) = {

    val MinArr = Array.tabulate(vec.head.size)(x => x)
    val MaxArr = Array.tabulate(vec.head.size)(x => x)

    val minVec = MinArr.map(i => vec.map(x => x(i)).min)
    val maxVec = MaxArr.map(i => vec.map(x => x(i)).max)

    (minVec, maxVec)
  }

  // normalize data to unit cube
  def normalizePoints(arr: ArrayBuffer[Point], minArr: Array[Double], maxArr: Array[Double]):
    ArrayBuffer[Point] = {
    val dim = arr.head.length

    val tabArr = Array.tabulate(dim)(x => x)
    arr.map { x => tabArr.map( 
      i => (x(i) - minArr(i)) / (maxArr(i) - minArr(i))) 
    }
  }

  // denormalize data from unit cube
  def denormalizePoints(arr: Array[Point], minArr: Array[Double], maxArr: Array[Double]):
    Array[Point] = {
    val dim = arr.head.length

    val tabArr = Array.tabulate(dim)(x => x)
    arr.map { x => tabArr.map( 
      i => x(i)*((maxArr(i) - minArr(i))) + minArr(i))  
    }
  }

  // denormalize data from unit cube
  def denormalizePoint(p: Point, minArr: Array[Double], maxArr: Array[Double]):
    Point = {
    val dim = p.length

    val tabArr = Array.tabulate(dim)(x => x)
    tabArr.map( 
      i => p(i)*((maxArr(i) - minArr(i))) + minArr(i))  
    
  }

  // Does a binary search to find the closest index in a sorted
  // list to another item P not in the list
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
