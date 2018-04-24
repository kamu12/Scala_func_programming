object KaminskyyToImport {

  sealed trait Expr {
    def eval(): Expr = this match {
      case n: Number  => n
      case Sum(l, r)  => l.eval + r.eval
      case Prod(l, r) => l.eval * r.eval
      case b: Bool    => b
      case v: Var     => v
    }

    def isReducible(): Boolean = this match {
      case n: Number => false
      case b: Bool   => false
      case _         => true
    }

    def +(that: Expr): Expr = {
      this match {
        case Number(n) => that match {
          case Number(m) => Number(n + m)
        }
      }
    }

    def *(that: Expr): Expr = {
      this match {
        case Number(n) => that match {
          case Number(m) => Number(n * m)
        }
      }
    }

    override def toString: String = this.toString

  }

  case class Number(n: Int) extends Expr {
    override def toString: String = n.toString
  }
  case class Bool(b: Boolean) extends Expr {
    override def toString: String = b.toString
  }
  case class Sum(l: Expr, r: Expr) extends Expr {
    override def toString: String = l.toString + '+' + r.toString
  }
  case class Prod(l: Expr, r: Expr) extends Expr {
    override def toString(): String = {
      val left = l match {
        case Number(n)  => n.toString
        case Sum(l, r)  => '(' + l.toString + '+' + r.toString + ')'
        case Prod(l, r) => l.toString + '*' + r.toString
        case Var(v)     => v
      }
      val right = r match {
        case Number(n)  => n.toString
        case Sum(l, r)  => '(' + l.toString + '+' + r.toString + ')'
        case Prod(l, r) => l.toString + '*' + r.toString
        case Var(v)     => v
      }

      left + '*' + right
    }
  }
  case class Less(l: Expr, r: Expr) extends Expr {
    override def toString: String = l.toString + '<' + r.toString
  }
  case class IfElse(cond: Expr, t: Expr, e: Expr) extends Expr {
    override def toString: String = "if(" + cond.toString + ") { " + t.toString + " } else { " + e.toString + " }"
  }
  case class Var(v: String) extends Expr {
    override def toString: String = v
  }

  sealed trait Stat {}

  case class DoNothing() extends Stat {}
  case class Assign(name: String, value: Expr) extends Stat {}
  case class If(cond: Expr, t: Stat, e: Stat) extends Stat {}
  case class Seq(seq: List[Stat]) extends Stat {}
  case class While(cond: Expr, t: Stat) extends Stat {}

  final class Machine {
    def reduce(expr: Expr, env: Map[String, Expr]): Expr = {
      if (expr.isReducible) {
        val newExpr = reductionStep(expr, env)
        if (newExpr != expr)
          reduce(newExpr, env)
        else
          expr
      } else
        expr
    }

    def run(stat: Stat, env: Map[String, Expr]): Map[String, Expr] = stat match {
      case DoNothing() => env
      case Assign(name: String, v: Expr) => {
        reduce(v, env) match {
          case n: Number => env + (name -> n)
          case b: Bool => env + (name -> b)
          case e:Expr => env + ("__error" -> e)
          case _ => env
        }

      }
      case If(c: Expr, t: Stat, e: Stat) => {
        reduce(c, env) match {
          case Bool(true)  => run(t, env)
          case Bool(false) => run(e, env)
          case _           => env
        }
      }
      case Seq(commands) => {
        if(env.contains("__error")) return env
        if (commands.nonEmpty) {

          run(Seq(commands.tail), run(commands.head, env))
        }
        else
          env
      }
      case While(c: Expr, t: Stat) => {
        if(env.contains("__error")) return env
        var cond: Expr = new Expr {}
        if (cond.isReducible)
          cond = reduce(c, env)
        else
          cond = c

        cond match {
          case Bool(true) => {
            run(While(c, t), run(t, env))
          }
          case Bool(false) => env
          case _           => env
        }
      }
      case _ => env
    }

    def reductionStep(expr: Expr, env: Map[String, Expr]): Expr = expr match {
      //edge case handling
      case Sum(n: Number, b: Bool)             => Sum(n, b)
      case Sum(b: Bool, n: Number)             => Sum(b, n)
      case Prod(n: Number, b: Bool)            => Prod(n, b)
      case Prod(b: Bool, n: Number)            => Prod(b, n)
      case Less(n: Number, b: Bool)            => Less(n, b)
      case Less(b: Bool, n: Number)            => Less(b, n)
      case IfElse(c: Number, t: Expr, e: Expr) => IfElse(c, t, e)

      case Var(v)                              => try { env(v) } catch { case _: Throwable => return Var(v) }
      case Number(n)                           => Number(n)
      case Bool(b)                             => Bool(b)
      case Sum(l: Number, r: Number)           => Number(l.n + r.n)
      case Prod(l: Number, r: Number)          => Number(l.n * r.n)
      case Less(l: Number, r: Number)          => Bool(l.n < r.n)

      case IfElse(c: Bool, t: Number, e: Expr) => if (c.b) { Number(t.n) } else { reductionStep(e, env) }
      case IfElse(c: Bool, t: Expr, e: Number) => if (c.b) { reductionStep(t, env) } else { Number(e.n) }
      case IfElse(c: Number, t: Expr, e: Expr) => expr
      case IfElse(c: Expr, t: Expr, e: Expr)   => IfElse(reductionStep(c, env), t, e)
      case Prod(n: Number, r)                  => Prod(n, reductionStep(r, env))
      case Sum(n: Number, r)                   => Sum(n, reductionStep(r, env))
      case Less(n: Number, r)                  => Less(n, reductionStep(r, env))
      case Sum(l, r)                           => Sum(reductionStep(l, env), r)
      case Prod(l, r)                          => Prod(reductionStep(l, env), r)
      case Less(l, r)                          => Less(reductionStep(l, env), r)
    }
  }
  val machine = new Machine()

  assert(machine.reduce(Var("z"), Map("x" -> Number(3), "y" -> Number(1), "b" -> Bool(true))) == Var("z"))
  assert(machine.reduce(Prod(Sum(Var("x"), Number(2)),
    Sum(Number(4), Var("y"))), Map("x" -> Number(1), "y" -> Number(3))) == Number(21))

  //test("Number does not reduce") {
  assert(machine.reduce(Number(4), Map("x" -> Number(1), "y" -> Number(3))) == Number(4))
  //test("Bool does not reduce") {
  assert(machine.reduce(Bool(true), Map("x" -> Number(1), "y" -> Number(3))) == Bool(true))
  //test("Number Var reduces to its value") {
  assert(machine.reduce(Var("x"), Map("x" -> Number(5), "y" -> Number(3))) == Number(5))
  //test("Bool Var reduces to its value") {
  assert(machine.reduce(Var("x"), Map("x" -> Bool(true), "y" -> Number(3))) == Bool(true))
  //test("Unknown Var does not reduce") {
  assert(machine.reduce(Var("z"), Map("x" -> Number(5), "y" -> Number(3))) == Var("z"))

  //test("Sum of two Numbers reduces to Number with their sum") {
  assert(machine.reduce(Sum(Number(3), Number(2)), Map("x" -> Number(1), "y" -> Number(3))) == Number(5))
  //test("Sum of Number and Bool does not reduce") {
  assert(machine.reduce(Sum(Number(3), Bool(true)), Map("x" -> Number(1), "y" -> Number(3))) == Sum(Number(3), Bool(true)))
  //test("Sum of Bool and Number does not reduce") {
  assert(machine.reduce(Sum(Bool(true), Number(3)), Map("x" -> Number(1), "y" -> Number(3))) == Sum(Bool(true), Number(3)))
  //test("left Sum operand reduces if it is reducible and right is left unchanged") {
  assert(machine.reduce(Sum(Var("x"), Number(3)), Map("x" -> Number(1), "y" -> Number(3))) == Number(4))
  assert(machine.reduce(Sum(Prod(Var("x"), Var("y")), Number(3)), Map("x" -> Number(1), "y" -> Number(3))) == Number(6))
  //test("otherwise right Sum operand reduces") {
  assert(machine.reduce(Sum(Number(3), Prod(Var("x"), Var("y"))), Map("x" -> Number(1), "y" -> Number(3))) == Number(6))

  //test("Prod of two Numbers reduces to Number with their product") {
  assert(machine.reduce(Prod(Number(3), Number(2)), Map("x" -> Number(1), "y" -> Number(3))) == Number(6))
  //test("Prod of Number and Bool does not reduce") {
  //assert(machine.reduce(Prod(Number(3), Bool(true)), Map("x" -> Number(1), "y" -> Number(3))) == Prod(Number(3), Bool(true)))
  //test("Prod of Bool and Number does not reduce") {
  //assert(machine.reduce(Prod(Bool(true), Number(3)), Map("x" -> Number(1), "y" -> Number(3))) == Prod(Bool(true), Number(3)))
  //test("left Prod operand reduces if it is reducible and right is left unchanged") {
  assert(machine.reduce(Prod(Prod(Var("x"), Var("y")), Number(3)), Map("x" -> Number(1), "y" -> Number(3))) == Number(9))
  //test("otherwise right Prod operand reduces") {
  assert(machine.reduce(Prod(Number(3), Prod(Var("x"), Var("y"))), Map("x" -> Number(1), "y" -> Number(3))) == Number(9))

  //// Less
  //test("Less of two Numbers reduces to Bool indicating whether first number is less than the second") {
  assert(machine.reduce(Less(Number(3), Number(2)), Map("x" -> Number(1), "y" -> Number(3))) == Bool(false))
  //test("Less of Number and Bool does not reduce") {
  assert(machine.reduce(Less(Bool(true), Number(2)), Map("x" -> Number(1), "y" -> Number(3))) == Less(Bool(true), Number(2)))
  //test("Less of Bool and Number does not reduce") {
  assert(machine.reduce(Less(Number(2), Bool(true)), Map("x" -> Number(1), "y" -> Number(3))) == Less(Number(2), Bool(true)))
  //test("left Less operand reduces if it is reducible and right is left unchanged") {
  assert(machine.reduce(Less(Prod(Var("x"), Var("y")), Number(3)), Map("x" -> Number(1), "y" -> Number(3))) == Bool(false))
  //test("otherwise right Less operand reduces") {
  assert(machine.reduce(Less(Number(3), Prod(Var("x"), Var("y"))), Map("x" -> Number(1), "y" -> Number(3))) == Bool(false))

  //// IfElse
  //test("IfElse reduces to thenExpr for Bool(true) condition") {
  assert(machine.reduce(IfElse(Bool(true), Prod(Number(3), Prod(Var("x"), Var("y"))), Number(2)), Map("x" -> Number(1), "y" -> Number(3))) ==
    Number(9))
  //test("IfElse reduces to elseExpr for Bool(false) condition") {
  assert(machine.reduce(IfElse(Bool(false), Number(2), Sum(Number(3), Prod(Var("x"), Var("y")))), Map("x" -> Number(1), "y" -> Number(3))) ==
    Number(6))
  //test("IfElse for Number condition does not reduce") {
  assert(machine.reduce(IfElse(Number(0), Number(2), Sum(Number(3), Prod(Var("x"), Var("y")))), Map("x" -> Number(1), "y" -> Number(3))) ==
    IfElse(Number(0), Number(2), Sum(Number(3), Prod(Var("x"), Var("y")))))
  //test("IfElse for reducible condition reduces its condition") {
  assert(machine.reduce(IfElse(Less(Number(3), Prod(Var("x"), Var("y"))), Number(2), Sum(Number(3), Prod(Var("x"), Var("y")))),
    Map("x" -> Number(1), "y" -> Number(3))) == Number(6))

  //// DoNothing
  //test("DoNothing does not alter environment") {
  assert(machine.run(DoNothing(), Map("x" -> Number(3))) == Map("x" -> Number(3)))
  //// Assign
  //test("Assign adds new variable for number expression") {
  assert(machine.run(Assign("b2", Prod(Number(3), Number(2))), Map("x" -> Number(3))) ==
    Map("x" -> Number(3), "b2" -> Number(6)))
  //test("Assign adds new variable for boolean expression") {
  assert(machine.run(Assign("b2", Less(Number(3), Number(2))), Map("x" -> Number(3))) ==
    Map("x" -> Number(3), "b2" -> Bool(false)))
  //test("Assign updates existing variable for number expression") {
  assert(machine.run(Assign("b2", Prod(Number(4), Number(2))), Map("x" -> Number(3), "b2" -> Number(6))) ==
    Map("x" -> Number(3), "b2" -> Number(8)))
  //test("Assign updates existing variable for boolean expression") {
  assert(machine.run(Assign("b2", Less(Number(1), Number(2))), Map("x" -> Number(3), "b2" -> Bool(false))) ==
    Map("x" -> Number(3), "b2" -> Bool(true)))
  //test("Assign updates existing variable for expression with the same variable") {
  assert(machine.run(Assign("x", Sum(Number(1), Var("x"))), Map("x" -> Number(3), "b2" -> Number(6))) ==
    Map("x" -> Number(4), "b2" -> Number(6)))
  //test("Assign does not occur for erroneous expression") {
  assert(machine.run(Assign("b2", Sum(Bool(true), Number(3))), Map("x" -> Number(3), "b2" -> Number(6))) ==
    Map("x" -> Number(3), "b2" -> Number(6), "__error" -> Sum(Bool(true), Number(3))))
  assert(machine.run(Assign("y", Var("z")), Map()) == Map("__error" -> Var("z")))
  assert(machine.run(Assign("y", Var("z")), Map("z" -> Number(3))) == Map("z" -> Number(3), "y" -> Number(3)))
  assert(machine.run(Assign("x", Sum(Var("x"), Number(1))), Map("x" -> Number(2))) == Map("x" -> Number(3)))
  //// If
  //test("'If' runs thenStat if condition is Bool(true)") {
  assert(machine.run(If(Less(Number(2), Number(4)), Assign("y", Number(3)), Assign("y", Number(4))), Map()) ==
    Map("y" -> Number(3)))
  //test("'If' runs elseStat if condition is Bool(false)") {
  assert(machine.run(If(Less(Number(5), Number(4)), Assign("y", Number(3)), Assign("y", Number(4))), Map()) ==
    Map("y" -> Number(4)))
  //test("'If' statement fails for erroneous condition") {
  assert(machine.run(If(Less(Number(5), Bool(true)), Assign("y", Number(3)), Assign("y", Number(4))), Map()) ==
    Map())
  //test("'If' statement fails for condition expression that reduces to Number") {
  assert(machine.run(If(Sum(Number(5), Number(4)), Assign("y", Number(3)), Assign("y", Number(4))), Map()) ==
    Map())
  //// Seq
  //test("'Seq' does nothing if empty") {
  assert(machine.run(Seq(List()), Map()) == Map())
  //test("'Seq' executes one its statement if contains only one") {
  assert(machine.run(Seq(List(Assign("y", Number(9)))),
    Map("x" -> Number(6), "y" -> Number(1), "b" -> Bool(true))) ==
    Map("x" -> Number(6), "y" -> Number(9), "b" -> Bool(true)))
  //test("'Seq' executes its statements one by one") {
  assert(machine.run(Seq(List(Assign("x", Sum(Var("x"), Number(1))), Assign("y", Number(9)))),
    Map("x" -> Number(6), "y" -> Number(1), "b" -> Bool(true))) ==
    Map("x" -> Number(7), "y" -> Number(9), "b" -> Bool(true)))
  //test("'Seq' does not execute remained statements after first failure") {
  assert(machine.run(Seq(List(Assign("x", Sum(Var("x"), Number(1))), Assign("y", Var("z")))),
  Map("x" -> Number(6), "y" -> Number(1), "b" -> Bool(true))) ==
    Map("x" -> Number(7), "y" -> Number(1), "b" -> Bool(true), "__error" -> Var("z")))
  //
  //// While
  //test("'While' executes thenStat multiple times while condition reduces to Bool(true)") {
  assert(machine.run(While(Less(Var("x"), Number(4)), Assign("x", Sum(Var("x"), Number(1)))),
    Map("x" -> Number(1))) == Map("x" -> Number(4)))
  //test("'While' does not execute thenStat if condition reduces to Bool(false) from the start") {
  assert(machine.run(While(Less(Var("x"), Number(4)), Assign("y", Var("x"))), Map("x" -> Number(8))) ==
    Map("x" -> Number(8)))
  //test("'While' statement fails for erroneous condition") {
  assert(machine.run(While(Less(Var("x"), Bool(false)), Assign("y", Var("x"))), Map("x" -> Number(8))) ==
    Map("x" -> Number(8)))
  //test("'While' statement fails for condition expression that reduces to Number") {
  assert(machine.run(While(Sum(Var("x"), Number(3)), Assign("y", Var("x"))), Map("x" -> Number(8))) ==
    Map("x" -> Number(8)))
  //test("'While' statement fails if thenStat statement fails") {
  assert(machine.run(While(Less(Var("x"), Number(10)),
    Seq(List(Assign("x", Sum(Var("x"), Number(1))), Assign("y", Var("z"))))),
    Map("x" -> Number(6), "y" -> Number(1), "b" -> Bool(true))) ==
    Map("x" -> Number(7), "y" -> Number(1), "b" -> Bool(true), "__error" -> Var("z")))

}