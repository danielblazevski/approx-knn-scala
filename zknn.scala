/////////// z-knn
import scala.collection.mutable.ListBuffer
import scala.collection.SortedSet

object zknn{

	val alpha = 3
	val gamma = 10
	val r = scala.util.Random

	def interleave(in:ListBuffer[String]): String = {
		// get max length
		val maxLen = in.map(str => str.length).max
		val L = in.length
		var res = ""

		for (i <- 0 until maxLen){
			for (j <- 0 until L){
				if(i >= in(L - 1 - j).length){
					res = 0 + res
				}else{
					res = in(L - 1 - j)(in(L - 1 - j).length - 1 - i) + res
				}
			}
		}
		res
	}

	def zValue(in:ListBuffer[Double]): Int = {
		Integer.parseInt(interleave(in.map(x=>x.toInt.toBinaryString)),2)
	}

 	def subTwo(tuple: (ListBuffer[ListBuffer[Double]], Double)) = tuple._2

 	def distance(a:ListBuffer[Double], b:ListBuffer[Double]): Double = {
 		math.sqrt(a.zipWithIndex.map{ x => 
 									(x._1 - b(x._2))*(x._1 - b(x._2))
 									}.sum)
 	}

	def basicknnQuery(train:ListBuffer[ListBuffer[Double]], test:ListBuffer[ListBuffer[Double]], k: Int){
		test.map{ v=> (v,
			train.map{
      		x => (x, distance(v, x))
      		}.sortBy(_._2).take(k).map(_._1).toArray)
				}
	}

/////////////// main zknn query
	def zknnQuery(train:ListBuffer[ListBuffer[Double]], test:ListBuffer[ListBuffer[Double]], k: Int){

		var zTrainSet = SortedSet[(ListBuffer[ListBuffer[Double]], Double)]()(Ordering.by(subTwo)) ++ 
		train.map(v=>(v,zValue(v)) )

		val candidatePointsFromZvalue = new ListBuffer[ListBuffer[Double]]
		val rSeq = Seq.fill(alpha)(Array.fill(train.head.length)(r.nextFloat))

		for (v <- test){
			for (i <- 0 until alpha){
				/////////get z_test_shifted; get nearest zip
				val zTestCur = zValue(v) /////////  NEED TO SHIFT!!

				/////////get gamma points about query point q need to check if near end

				//candidatePointsFromZvalue ++ //////////// get corresponding points in real space 
			}
			basicknnQuery(ListBuffer(v), candidatePointsFromZvalue, k)
		}
	}

	def main(args: Array[String]){

		val lb = ListBuffer(12.2, 34.3, 2.0)
		val Zval = zValue(lb)
		println(Zval)

	}

}
