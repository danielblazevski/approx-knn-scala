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
  val dim = train.head.length
    if ( dim < 30) {
      val ZknnClass = new zKNN(alpha, gamma)
      ZknnClass.zknnQuery(train, test, k)
    } else {
      // do LSH method, below is a place-holder for now
      val lshKnnClass = new lshKNN(alpha)
      lshKnnClass.lshknnQuery(train, test, k)
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


}
