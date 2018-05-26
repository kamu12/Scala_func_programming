package ua.edu.ucu.cs.parallel

import org.scalameter
import org.scalameter._

object WordCount {

  trait Monoid[A] {
    def op(x: A, y: A): A

    def zero: A
  }


  case class WordCounter(leftWord:Boolean, count:Int, rightWord:Boolean)

  val monoid = new Monoid[WordCounter]{
    def op(l:WordCounter, r:WordCounter): WordCounter = (l, r) match{
      case(WordCounter(_,-1,_), WordCounter(rl, rc, rr)) => r
      case(WordCounter(ll, lc, lr), WordCounter(_,-1,_)) => l
      case(WordCounter(ll, lc, true), WordCounter(true, rc, rr)) => WordCounter(ll, lc+rc, rr)
      case(WordCounter(ll, lc, true), WordCounter(false, rc, rr)) => WordCounter(ll, lc+rc+1, rr)
      case(WordCounter(ll, lc, false), WordCounter(true, rc, rr)) => WordCounter(ll, lc+rc, rr)
      case(WordCounter(ll, lc, false), WordCounter(false, rc, rr)) => WordCounter(ll, lc+rc, rr)
    }

    def zero = WordCounter(false, -1, false)
  }

  def containsNoSpecialChars(c: Char): Boolean = {
    val chars: String = "., ()[]{}!?@"
    return chars contains c
  }

  def charToTuple(c:Char):WordCounter = {
    if(containsNoSpecialChars(c)){
      WordCounter(true, 0, true)
    }
    else{
      WordCounter(false, 0, false)
    }

  }

  def foldMapPar[A, B](xs: IndexedSeq[A],
                       from: Int, to: Int,
                       m: Monoid[B])(f: A => B)
                      (implicit theresholdSize: Int): B =
    if (to - from <= theresholdSize)
      foldMapSegment(xs, from, to, m)(f)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldMapPar(xs, from, middle, m)(f)
        (theresholdSize),
        foldMapPar(xs, middle, to, m)(f)
        (theresholdSize))
      m.op(l, r)
    }

  def foldMapSegment[A, B](xs: IndexedSeq[A],
                           from: Int, to: Int,
                           m: Monoid[B])
                          (f: A => B): B = {
    var res = f(xs(from))
    var index = from + 1
    while (index < to) {
      res = m.op(res, f(xs(index)))
      index = index + 1
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("/text.txt")
    val text = source.getLines().mkString(" ").toVector

    source.close
    implicit val threshold: Int = 100
    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer (new scalameter.Warmer.Default)

    println(foldMapSegment(text, 0, text.length, monoid)(charToTuple))
    println(foldMapPar(text, 0, text.length, monoid)(charToTuple))

    val seqtime = standardConfig measure {
      foldMapSegment(text, 0, text.length, monoid)(charToTuple)
    }

    val partime = standardConfig measure {
      foldMapPar(text, 0, text.length, monoid)(charToTuple)
    }

    println(s"sequential time is $seqtime ms")
    println(s"sequential time is $partime ms")
    println(s"speedupPoint: ${seqtime.value  / partime.value}")

  }
}
