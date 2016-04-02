import scala.collection.mutable.ListBuffer

import zknn.zknn

object zknnTest{

  def distance(a: ListBuffer[Double], b: ListBuffer[Double]): Double = {
    math.sqrt(a.zipWithIndex.map { x =>
      (x._1 - b(x._2)) * (x._1 - b(x._2))
    }.sum)
  }

  def benchmark_zknn(numPoints: Int){
    /// generate random set of points in [1,2]^6
    // to-do allow for negative entries, even when shifted in zknn query
    val r = scala.util.Random

    val training = ListBuffer.fill(numPoints)(ListBuffer(r.nextDouble + 1.0, r.nextDouble + 1.0,
      r.nextDouble + 1.0, r.nextDouble + 1.0,
      r.nextDouble + 1.0, r.nextDouble + 1.0))

    val testing = ListBuffer.fill(numPoints)(ListBuffer(r.nextDouble + 1.0, r.nextDouble + 1.0,
      r.nextDouble + 1.0, r.nextDouble + 1.0,
      r.nextDouble + 1.0, r.nextDouble + 1.0))
      
    val alpha = 1
    val gamma = 5

    val t0 = System.nanoTime()
    val ZknnClass = new zknn(alpha, gamma)
    val zknn = ZknnClass.zknnQuery(training, testing, 1)
    val tf = System.nanoTime()
    println("Elapsed time for zknn =       : " + (tf - t0)/1000000000 + "s")

      //println("nearest neighbor BENCHMARK =  " + zknn.head._2.head)

    val t0_brute = System.nanoTime()
    testing.map { v => (v,
      training.map {
        x => (x, distance(v, x))
      }.sortBy(_._2).take(1).map(_._1).toArray)
    }
      
    val tf_brute = System.nanoTime()
    println("Elapsed time for brute force knn =       : " + (tf_brute - t0_brute)/1000000000 + "s")

  }

  def main(args: Array[String]) {

    val alpha = 1
    val gamma = 1

    val ZknnClass = new zknn(alpha, gamma)

    val lb = ListBuffer(12.2, 34.3, 2.0)
    val Zval = ZknnClass.zValue(lb)
    println("zVal =  " + Zval)

    val train = ListBuffer(ListBuffer(1.2, 4.3), ListBuffer(25.0, 3.0), ListBuffer(29.0, 7.5))
    val test = ListBuffer(ListBuffer(0.8, 3.5), ListBuffer(1.6, 0.2))

    val knn = ZknnClass.zknnQuery(train, test, 1)
    println("nearest neighbor =  " + knn.head._2.head)

    val numPoints = 10000
     benchmark_zknn(numPoints)

  }
}
