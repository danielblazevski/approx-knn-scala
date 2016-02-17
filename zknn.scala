// z-knn
import scala.collection.mutable.ListBuffer
import scala.collection.SortedSet

object zknn {

  val alpha = 1
  val gamma = 1
  val r = scala.util.Random

  type Point = ListBuffer[Double]

  def interleave(in: ListBuffer[String]): String = {
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

  def subTwo(tuple: (ListBuffer[Double], Double)) = tuple._2

  def distance(a: Point, b: Point): Double = {
    math.sqrt(a.zipWithIndex.map { x =>
      (x._1 - b(x._2)) * (x._1 - b(x._2))
    }.sum)
  }

  def basicknnQuery(train: ListBuffer[Point], test: ListBuffer[Point], k: Int):
  ListBuffer[(ListBuffer[Double], Array[ListBuffer[Double]])] = {
    test.map { v => (v,
      train.map {
        x => (x, distance(v, x))
      }.sortBy(_._2).take(k).map(_._1).toArray)
    }
  }

  // main zknn query
  def zknnQuery(train: Seq[Point], test: Seq[Point], k: Int):
  ListBuffer[(Point, Array[Point])] = {

    var candidatePointsFromZvalue = new ListBuffer[Point]
    val rSeq = Seq.fill(alpha)(Seq.fill(train.head.length)(r.nextFloat))

    var res = new ListBuffer[(Point, Array[Point])]
    val v = test.head
    for (i <- 0 until alpha) {
      val zQueryShifted = zValue(v.zipWithIndex.map { vZip => vZip._1 - rSeq(i)(vZip._2) })

      val zTrainSetShiftedSorted = train.map { trainPoint => (trainPoint,
        trainPoint.zipWithIndex.map(trainPointZipped =>
          trainPointZipped._1 - rSeq(i)(trainPointZipped._2)))
      }.map {
        shiftTrainPoint =>
          (shiftTrainPoint._1, zValue(shiftTrainPoint._2))
      } .sortBy(x => x._2)

      // get 2*gamma points about query point q, need to check if near end...wip
      val zTrainSetShiftedByZTrain = zTrainSetShiftedSorted.map { tuple =>
        (tuple._1, tuple._2 - zQueryShifted)
      }

      val posFilter = zTrainSetShiftedByZTrain.filter(x => x._2 > 0)
      val negFilter = zTrainSetShiftedByZTrain.filter(x => x._2 < 0)

      if (posFilter.length >= gamma && negFilter.length >= gamma) {
        println("posFilter.take(gamma).map(x => x._1)   =  " + posFilter.take(gamma).map(x => x._1))
        println("negFilter.take(gamma).map(x => x._1)   =  " + negFilter.take(gamma).map(x => x._1))
        candidatePointsFromZvalue = candidatePointsFromZvalue ++ posFilter.take(gamma).map(x => x._1)
        candidatePointsFromZvalue = candidatePointsFromZvalue ++ negFilter.take(gamma).map(x => x._1)

      }
      res = res ++ basicknnQuery(candidatePointsFromZvalue, ListBuffer(v), k)
    }
    return res
  }

  def main(args: Array[String]) {

    val lb = ListBuffer(12.2, 34.3, 2.0)
    val Zval = zValue(lb)
    println("zVal =  " + Zval)

    val train = Seq(ListBuffer(1.2, 4.3), ListBuffer(2.0, 0.0), ListBuffer(2.0, 1.0))
    val test = Seq(ListBuffer(0.8, 3.5))

    val knn = zknnQuery(train, test, 1)
    println("nearest neighborr =  " + knn.head._2.head)

  }
}
