// approximate k nearest neighbors

// for small and moderate dimensions, uses the z-value based knn
// for larger dimensions, uses LSH based method

package approxknn

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ArrayBuffer

class approxKNN() {

  val r = scala.util.Random

  type Point = Array[Double]

  val alpha = 2
  val gamma = 5

  def approxKNN(train: ArrayBuffer[Point], test: ArrayBuffer[Point], k: Int):
  ArrayBuffer[(Point, Array[Point])] = {
    if ( train.head.length < 30) {
      val ZknnClass = new zKNN(alpha, gamma)
      ZknnClass.zknnQuery(train, test, k)
    } else {
      // do LSH method, below is a place-holder for now
      val ZknnClass = new zKNN(alpha, gamma)
      ZknnClass.zknnQuery(train, test, k)
    }

  }
  /// a bunch of methods, will likely be ovelap for both z-value and LSH methods
  
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
