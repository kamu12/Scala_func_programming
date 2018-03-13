class Interval(left:Double, right:Double){
  require(left < right, s"$left is bigger than $right")


  def lowerBound = left
  def upperBound = right

  override def toString: String =
    "[" + lowerBound + ";" + upperBound + "]"

  def +(that: Interval): Interval =
    new Interval(
      this.lowerBound + that.lowerBound,
      this.upperBound + that.upperBound
    )

  def unary_- : Interval =
    new Interval(-upperBound, -lowerBound)

  def *(that:Interval): Interval = {
    def min(a:Double, b:Double, c:Double, d:Double)={
      val min1=math.min(a, b)
      val min2=math.min(c, d)
      math.min(min1, min2)
    }

    def max(a:Double, b:Double, c:Double, d:Double)={
      val max1=math.max(a, b)
      val max2=math.max(c, d)
      math.max(max1, max2)
    }

    val p1 =this.lowerBound * that.lowerBound
    val p2 =this.lowerBound * that.upperBound
    val p3 =this.upperBound * that.lowerBound
    val p4 =this.upperBound * that.upperBound

    new Interval(min(p1,p2,p3,p4), max(p1,p2,p3,p4))
  }

  def /(that: Interval): Interval =
    this * new Interval(1.0 / that.upperBound, 1.0 / that.lowerBound)

}

val i1 = new Interval(2.4, 5)
val i2 = new Interval(22.4, 5)