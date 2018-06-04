package ua.edu.ucu.cs.parallel

import org.scalameter
import org.scalameter._

import scala.util.Random

object MonteCarloIntegrationEstimation {

  def sumAreas(f: Double => Double, arr: Array[Double], a:Int, b:Int): Double ={
    def iter(sum:Double, index:Int): Double ={
      if(index >= arr.length) sum
      else iter(sum + f(arr(index)*(b-a)), index + 1)
    }

    iter(0, a)
  }

  def sumAreas(f: Double => Double, a:Int, b:Int, n:Int): Double ={
    def iter(sum:Double, index:Int): Double ={
      if(index >= n) sum
      else iter(sum + f((a + (b - a) * rnd.nextDouble())*(b-a)), index + 1)
    }

    iter(0, a)
  }

  def sumAreasParPoint(f: Double => Double, a:Int, b:Int, n:Int): Double ={

    def iter(a:Int, b:Int, i:Int, n:Int): Double = {
      if(i < split){
        val middle = a + (b-a)/2
        val (sum1, sum2) = parallel(iter(a, middle, i + 1, n/2), iter(middle, b, i + 1, n/2))
        sum1 + sum2
      }
      else{
        sumAreas(f, a, b, n)
      }

    }

    iter(a, b, 0, n)
  }

  def sumAreasParPoint2(f: Double => Double, a:Int, b:Int, n:Int): Double = {
    val part = (b - a) / 4
    val((sum1, sum2), (sum3, sum4)) = parallel(
      parallel(sumAreas(f, a, a+part, n/4), sumAreas(f, a+part+1, a+2*part, n/4)),
      parallel(sumAreas(f, a+2*part+1, a+3*part, n/4),sumAreas(f, a+3*part+1, b, n/4)))

    sum1+sum2+sum3+sum4
  }


  val rnd = new Random
  val N = 1000000
  val treshold = 500000

  def sumAreasPar(f: Double => Double, arr: Array[Double], a:Int, b:Int): Double ={
    if(b-a <= treshold)
      sumAreas(f, arr, a, b)
    else{
//      println("parallel")
      val middle = a + (b-a)/2
      val (sum1, sum2) = parallel(sumAreasPar(f, arr, a, middle),
        sumAreasPar(f, arr, middle, b))
//      val (sum1, sum2) = parallel(sumAreasPar(f, arr.filter(_>a).filter(_<middle), a, middle),
//                                  sumAreasPar(f, arr.filter(_>middle).filter(_<b), middle, b))
      sum1 + sum2
    }
  }

  def sumAreasParNew(f: Double => Double, a:Int, b:Int): Double ={
    if(b-a <= treshold){
      val input = (0 until b-a).map(x => (a + (b - a) * rnd.nextDouble())).toArray
      sumAreas(f, input, a, b)}
    else{
      //      println("parallel")
      val middle = a + (b-a)/2
      //      val (sum1, sum2) = parallel(sumAreasPar(f, arr, a, middle),
      //        sumAreasPar(f, arr, middle, b))
      val (sum1, sum2) = parallel(sumAreasParNew(f, a, middle),
        sumAreasParNew(f, middle, b))
      sum1 + sum2
    }
  }

  val split = 2

  def sumAreasPar2(f: Double => Double, arr: Array[Double], a:Int, b:Int): Double ={

    def iter(a:Int, b:Int, i:Int): Double = {
      if(i < split){
        val middle = a + (b-a)/2
        val (sum1, sum2) = parallel(iter(a, middle, i + 1), iter(middle, b, i + 1))
        sum1 + sum2
      }
      else{
        sumAreas(f, arr, a, b)
      }

    }

    iter(a, b, 0)
  }

  def sumAreasPar3(f: Double => Double, arr: Array[Double], a:Int, b:Int): Double = {
    val part = (b - a) / 4
    val((sum1, sum2), (sum3, sum4)) = parallel(
      parallel(sumAreas(f, arr, a, a+part), sumAreas(f, arr, a+part+1, a+2*part)),
      parallel(sumAreas(f, arr, a+2*part+1, a+3*part),sumAreas(f, arr, a+3*part+1, b)))

    sum1+sum2+sum3+sum4
  }

  def integrate(f:Double=>Double, arr: Array[Double], a:Int, b:Int): Double = {
    sumAreas(f, arr, a, b)/N
  }

  def integratePoint(f:Double=>Double, a:Int, b:Int): Double = {
    sumAreas(f, a, b, N)/N
  }

  def integratePointPar(f:Double=>Double, a:Int, b:Int): Double = {
    sumAreasParPoint(f, a, b, N)/N
  }

  def integratePointPar2(f:Double=>Double, a:Int, b:Int): Double = {
    sumAreasParPoint2(f, a, b, N)/N
  }

  def integratePar(f:Double=>Double, arr: Array[Double], a:Int, b:Int): Double = {
    sumAreasPar(f, arr, a, b)/N
  }

  def integrateParNew(f:Double=>Double, a:Int, b:Int): Double = {
    sumAreasParNew(f, a, b)/N
  }

  def integratePar2(f:Double=>Double, arr: Array[Double], a:Int, b:Int): Double = {
    sumAreasPar2(f, arr, a, b)/N
  }

  def integratePar3(f:Double=>Double, arr: Array[Double], a:Int, b:Int): Double = {
    sumAreasPar3(f, arr, a, b)/N
  }

  def main(args: Array[String]): Unit = {
    val rnd = new Random
    val a = 0
    val b = 1000000
    def f(x:Double):Double = x
    val input = (0 until N).map(x => (a + (b - a) * rnd.nextDouble())).toArray.sorted

    println(integrate(f, input, a, b))
    println(integratePar(f, input, a, b))
    println(integratePoint(f, a, b))
    println(integratePointPar(f, a, b))
    println(integratePointPar2(f, a, b))


    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer (new scalameter.Warmer.Default)

    val seqtime = standardConfig measure {
      integrate(f, input, a, b)
    }

    val seqtimePoint = standardConfig measure {
      integratePoint(f, a, b)
    }

    val parPoint = standardConfig measure {
      integratePointPar(f, a, b)
    }

    val parPoint2 = standardConfig measure {
      integratePointPar2(f, a, b)
    }

//    val partime = standardConfig measure {
//      integratePar(f, input, a, b)
//    }
//
//    val partime2 = standardConfig measure {
//      integratePar2(f, input, a, b)
//    }
//
//
//    val partime3 = standardConfig measure {
//      integratePar3(f, input, a, b)
//    }


    println(s"sequential time is $seqtime ms")
    println(s"sequential time with points is $seqtimePoint ms")
    println(s"parallel time with points is $parPoint ms")
    println(s"parallel time with points is $parPoint2 ms")
//    println(s"paralell time is $partime ms")
//    println(s"speedup: ${seqtime.value  / partime.value}")
//    println(s"speedup2: ${seqtime.value  / partime2.value}")
//    println(s"speedup3: ${seqtime.value  / partime3.value}")
    println(s"speedupPoint: ${seqtime.value  / seqtimePoint.value}")
    println(s"speedupPointPar: ${seqtimePoint.value  / parPoint.value}")
    println(s"speedupPointPar: ${seqtimePoint.value  / parPoint2.value}")

  }
}
