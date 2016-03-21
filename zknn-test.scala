import scala.collection.mutable.ListBuffer

import zknn.zknn

object zknnTest{

	def benchmark(numPoints: Int){
		/// generate random set of points in [1,2]^6
		// to-do allow for negative entries, even when shifted in zknn query
 		val r = scala.util.Random

		val training = Seq.fill(numPoints)(ListBuffer(r.nextDouble + 1.0, r.nextDouble + 1.0,
			r.nextDouble + 1.0, r.nextDouble + 1.0,
			r.nextDouble + 1.0, r.nextDouble + 1.0))

		val testing = Seq.fill(numPoints)(ListBuffer(r.nextDouble + 1.0, r.nextDouble + 1.0,
			r.nextDouble + 1.0, r.nextDouble + 1.0,
			r.nextDouble + 1.0, r.nextDouble + 1.0))
	    
	    val alpha = 1
    	val gamma = 5

		val ZknnClass = new zknn(alpha, gamma)
		val knn = ZknnClass.zknnQuery(training, testing, 1)
    	println("nearest neighbor BENCHMARK =  " + knn.head._2.head)

	}

  def main(args: Array[String]) {

    val alpha = 1
    val gamma = 1

    val ZknnClass = new zknn(alpha, gamma)

    val lb = ListBuffer(12.2, 34.3, 2.0)
    val Zval = ZknnClass.zValue(lb)
    println("zVal =  " + Zval)

    val train = Seq(ListBuffer(1.2, 4.3), ListBuffer(25.0, 3.0), ListBuffer(29.0, 7.5))
    val test = Seq(ListBuffer(0.8, 3.5), ListBuffer(1.6, 0.2))

    val knn = ZknnClass.zknnQuery(train, test, 1)
    println("nearest neighbor =  " + knn.head._2.head)

     val t0 = System.nanoTime()
     benchmark(10000)
     val tf = System.nanoTime()
 	 println("Elapsed time for benchmark =       : " + (tf - t0)/1000000000 + "s")

  }
}
