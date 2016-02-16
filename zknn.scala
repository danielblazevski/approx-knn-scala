/////////// z-knn
import scala.collection.mutable.ListBuffer
import scala.collection.SortedSet

object zknn{

	val alpha = 3
	val gamma = 10
	val r = scala.util.Random

	def interleave(in:ListBuffer[String]): String = {
		// get max length
		var maxLen = 0
		var res = ""
		val L = in.length

		for (i <- 0 until L){
			if (in(i).length > maxLen){
				maxLen = in(i).length
			}
		}

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


 	def distance(a:Seq[Double], b:Seq[Double]): Double = {
 		var res = 0.0
 		for (i <-0 until a.length){
 			res += (a(i) - b(i))*(a(i) - b(i)) 
 		}
 		math.sqrt(res)
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

		val C = new ListBuffer[ListBuffer[Double]]
		val rSeq = Seq.fill(alpha)(Array.fill(train.head.length)(r.nextFloat))

		for (itest <- 0 until test.length){
			for (i <- 0 until alpha){
				/////////get z_test_shifted; get nearest zip
				val zTestCur = zValue(test(itest)) /////////  NEED TO SHIFT!!

				/////////get gamma points aobut query point q need to check if near end

				//C++ //////////// get corresponding points in real space 
			}
			//basicknnQuery(itest, C, k)
		}
	}

	def main(args: Array[String]){

		val lb = ListBuffer(12.2, 34.3, 2.0)
		val Zval = zValue(lb)
		println(Zval)

	}

}
