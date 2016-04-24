import scala.collection.mutable.ArrayBuffer

import approxknn.approxKNN
import approxknn.zKNN
import approxknn.lshKNN

object zknnTest{

  def benchmark_knn(numPoints: Int){
    // generate random set of points in [1,2]^6
    // to-do allow for negative entries, even when shifted in zknn query
    val r = scala.util.Random

    val training = ArrayBuffer.fill(numPoints)(Array(r.nextDouble + 1.0, r.nextDouble + 1.0,
      r.nextDouble + 1.0,r.nextDouble + 1.0,r.nextDouble + 1.0))

    val testing = ArrayBuffer.fill(numPoints)(Array(r.nextDouble + 1.0, r.nextDouble + 1.0,
      r.nextDouble + 1.0,r.nextDouble + 1.0,r.nextDouble + 1.0))
      
    val alpha = 2
    val gamma = 5

    val t0 = System.nanoTime()
    val knnClass = new approxKNN()
    val zknn = knnClass.approxKNN(training, testing, 1)
    val tf = System.nanoTime()
    println("Elapsed time for zknn =       : " + (tf - t0)/1000000000 + "s")

    val t0_lsh = System.nanoTime()
    val lshKnnClass = new lshKNN(2)
    val lshknn = lshKnnClass.lshknnQuery(training, testing, 1)
    val tf_lsh = System.nanoTime()
    println("Elapsed time for lsh-knn =       : " + (tf_lsh - t0_lsh)/1000000000 + "s")


    val t0_brute = System.nanoTime()
    val knn = knnClass.basicknnQuery(training, testing, 1)
    val tf_brute = System.nanoTime()
    println("Elapsed time for brute force knn =       : " + (tf_brute - t0_brute)/1000000000 + "s")

  }

  def main(args: Array[String]) {

    val alpha = 1
    val gamma = 1

    val ZknnClass = new zKNN(alpha, gamma)

    val lb = Array(2.0, 6.0)
    val Zval = ZknnClass.zValue(lb)
    println("zVal =  " + Zval)

    val train = ArrayBuffer(Array(1.2, 4.3), Array(25.0, 3.0), Array(29.0, 7.5))
    val test = ArrayBuffer(Array(0.8, 3.5), Array(1.6, 0.2))

    //val knn = ZknnClass.zknnQuery(train, test, 1)
    //println("nearest neighbor =  " + knn.head._2.head)

    val numPoints = 5000
     benchmark_knn(numPoints)

  }
}
