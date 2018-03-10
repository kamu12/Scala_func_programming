
object workshop_1{
  def fib(n: Int): Int =
    if (n==0) 0
    else {
      val x = 9

      if (n==1) 1
      else fib(n-1)+fib(n-2)
    }

  def iter_fib(n: Int): Int = {
    @annotation.tailrec
    def fib_iter(left: Int, right: Int, i: Int): Int =
      if (n == i) left
      else
        fib_iter(left + right, left, i + 1)

    fib_iter(0, 1, 0)
  }

  def func(x:Int, y: => Int):Int = {
    if (x > y) x else y
  }

  def loop ( b : Boolean ) : Boolean = loop ( b )

}

workshop_1.fib(2)
workshop_1.iter_fib(150)

workshop_1.func(2+3, 3+4)
workshop_1.func(5, 3+4)
if(5>(3+4))5 else (3+4)

val y0=9
workshop_1.func(5, y0)

//{
//  val y0=10
//  if(5>y0) 5 else y0
//}

//if(5>9)5 else 9
def x = workshop_1.loop(true)
