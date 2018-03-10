//task 1 - recursive process
def pascal(c:Int, r:Int):Int={
  if(c > r) -1
  else if(c==0 || c==r) 1
  else{
    pascal(c-1, r-1)+pascal(c, r-1)
  }

}

def factorial(n:Int):Int={
  @annotation.tailrec
  def factorial_iter(res:Int, curr_n:Int):Int={
    if(curr_n > n) res
    else {
      factorial_iter(res*curr_n, curr_n+1)
    }
  }

  factorial_iter(1, 1)
}

def C(n:Int, k:Int):Int ={
  factorial(n)/(factorial(k) * factorial(n-k))
}

// task 1 - tail recursion process
def pascal2(c:Int, r:Int):Int={
  if(c > r) -1
  else if(c==0 || c==r) 1
  else {
    C(r-1, c) + C(r - 1, c - 1)
  }
}

factorial(12)
pascal(1,0)
pascal(5,2)
pascal(10,20)
pascal(5,10)
pascal(7,14)

// task 2
def balance(chars: List[Char]): Boolean ={
  @annotation.tailrec
  def balance_iter(chars: List[Char], counter:Int): Boolean ={
    if(counter == -1) false
    else if(chars.isEmpty){if(counter == 0) true else false}
    else{
      if(chars.head == '('){
        balance_iter(chars.tail, counter+1)
      }
      else if(chars.head == ')') {
        balance_iter(chars.tail, counter - 1)
      }
      else{
        balance_iter(chars.tail, counter)
      }
    }
  }

  balance_iter(chars, 0)
}


balance("h( (  )  ( ) )".toList)
balance("h( тут ( та й тут ) i може) (тут) ".toList)
balance("())(()".toList)
balance("sd() (())))9(() )(()( 0(0()()()) ) ()(0) 90( )90 (0() 0( ))() )()FR))E()C0d0(C)d0v0d()v )()()c0()d( f ) fsdf".toList)


//task 3 - complex approach
//def countChange(money: Int, coins: List[Int]): Int={
//  @annotation.tailrec
//  def check_combination(sum:Int, counter:Int, curr_coins:List[Int]):Int={
//    if(curr_coins.isEmpty) counter
//    else {
//      if(sum == money) {
//        def negator = if(curr_coins.tail.isEmpty) curr_coins.head else curr_coins.tail.head
//        check_combination(sum - negator, counter + 1, curr_coins.tail)
//      }
//      else if(sum < money){
//        check_combination(sum + curr_coins.head, counter, curr_coins)
//      }
//      else{
//        check_combination(sum - curr_coins.head, counter, curr_coins)
//      }
//    }
//  }
//
//  @annotation.tailrec
//  def countChange_iter(counter:Int, curr_coins:List[Int]):Int= {
//    if (curr_coins.isEmpty) counter
//    else {
//      println(counter)
//      countChange_iter(counter + check_combination(0, 0, curr_coins), curr_coins.tail)
//    }
//  }
//
////  check_combination(0,0,coins)
//  countChange_iter(0, coins)
//}

//task 3 - simple approach
def countChange(money: Int, coins: List[Int]): Int={
  if(money == 0) return 1
  if(money < 0 || coins.isEmpty) return 0

  val left = countChange(money-coins.head, coins)
  val right = countChange(money, coins.tail)

  left+right
}


countChange(5, List(1,2,3))
