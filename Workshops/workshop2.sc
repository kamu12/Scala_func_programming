

def sumOfFuncs(f: Double => Double, g: Double => Double): Double => Double ={
  def sum(x:Double) = f(x) + g(x)
  sum
}

def sumOfFuncs2(f: Double => Double, g: Double => Double): Double => Double ={
  (x:Double) => f(x) + g(x)
}

def func1(x:Double) = x+2
def func2(x:Double) = x*2

val sum= sumOfFuncs2(func1, func2)

sum
sum(4)

def sumOfFuncs3(f: Double => Double, g: Double => Double): Double => Double ={
  (x:Double) => f(x) + g(x)
}

def sumOfFuncs4(f: Double => Double):
  (Double => Double) => Double => Double ={
  (g) =>
    (x) => f(x) + g(x)
}

def identity(x:Int) = x
def identity(x:Double) = x

//def sumOfInts(x:Int, y:Int)=x+y
def sumOfInts(x:Double)(y:Int)=x+y

def asds = sumOfInts(2)_
asds(3)
identity(2)
identity(2.3)

type Vector = Int => Double

type Matrix = (Int, Int) => Double

val matrix1: Matrix = (row, col) => if(row == col) 1.0 else 0.0

def getRow(m:Matrix, row:Int): Vector = col => m(row, col)

def getCol(m:Matrix, col:Int): Vector = row => m(row, col)

def sdf(index:Int):Int = 1
val vector1: Vector = sdf
val vector2: Vector = index => index

vector1(3)
vector2(5)

