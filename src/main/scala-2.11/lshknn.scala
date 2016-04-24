// Locality Sensitive Hashing (LSH) based approximte k-nearest neighbors algorithm

// WIP
package approxknn

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.control._

class lshKNN(alpha: Int) extends zKNN(alpha, gamma = 5) {

	val b = 1.0
	val W = 0.1

  def lshknnQuery(train: ArrayBuffer[Point], test: ArrayBuffer[Point], k: Int):
  ArrayBuffer[(Point, Array[Point])] = {

    val dim = train.head.length
    val rSeq = ArrayBuffer.fill(alpha)(ArrayBuffer.fill(dim)(r.nextDouble))

    var res = new ArrayBuffer[(Point, Array[Point])]
    val tabArr = Array.tabulate(alpha)(x => x)

    val hashMapTrain = HashMap[Int,ArrayBuffer[Point]]()

    for (v <- train){
    	val hashes = lshHash(v,rSeq,b,W)
    	for (hash <- hashes){
	    	if (hashMapTrain.contains(hash)){
	    		hashMapTrain(hash) += v
	    	} else {
	    		hashMapTrain(hash) = ArrayBuffer(v)
	    	}
    	}
	}

    for (v <- test) {
      var candidatePoints = new ArrayBuffer[Point]
       val hashesV = lshHash(v, rSeq, b, W)
       var numCol = 0
       val loop = new Breaks;
       loop.breakable{
       	for (hash <- hashesV){
       	if (hashMapTrain.contains(hash)){
       		numCol += hashMapTrain(hash).length
       		candidatePoints ++= hashMapTrain(hash)
       }
       if (numCol > k){
       	loop.break
       }
   	  }
   	}
      res += basicknnQuerySingleTest(candidatePoints, v, k)
	}
    res
}

  def lshHash(in: Point, randomDirections: ArrayBuffer[ArrayBuffer[Double]], bParam: Double, wParam: Double):
  ArrayBuffer[Int] = {
  	randomDirections.map{ r => 
  		((in.zipWithIndex.map(inZip => inZip._1*r(inZip._2)).sum + bParam)/wParam).floor.toInt
  	}
  }

}
