//task1
type Set = Int => Boolean

def contains(s:Set, elem:Int ) : Boolean = s(elem)

def singletonSet(elem: Int): Set = set => set == elem

def union(s:Set, t:Set): Set = elem => contains(s, elem) || contains(t, elem)
def intersect(s:Set, t:Set): Set = elem => contains(s, elem) && contains(t, elem)
def diff(s:Set, t:Set): Set = elem => contains(s, elem) && !contains(t, elem)
def filter(s:Set, p:Int => Boolean ): Set = elem => s(elem) && p(elem)

//task2

val interval = 1000

def forall(s:Set, p:Int => Boolean ): Boolean = {
  @annotation.tailrec
  def iter(a:Int) : Boolean = {
    if (a > interval) true
    else if (contains(s, a) && !p(a)) false
    else iter (a + 1)
  }
  iter(-interval)
}

def exists(s:Set ,p:Int => Boolean ) : Boolean = !forall(s, elem => !p(elem))

def map(s:Set, f:Int => Int) : Set = {
  @annotation.tailrec
  def iter(newSet: Set, a: Int): Set = {
    if (a > interval) newSet
    else if (contains(s, a)) iter(union(newSet, singletonSet(f(a))), a+1)
    else iter(newSet, a + 1)
  }
  iter(elem => false, -interval)
}

//more elegant solution
def mapOneLine(s:Set, f:Int => Int) : Set = elem => exists(s, el=>elem ==f(el))

////////////////////////////////////////////////////////////////////////////////
//tests

def pred(elem:Int):Boolean = elem%4 == 0

val set2: Set = elem => elem%2 == 0
assert(set2(4))
assert(!set2(5))
assert(set2(6))

val s1 = singletonSet(4)
val s2 = singletonSet(5)

assert(contains(s1, 4))

assert(!s1(5))
assert(s2(5))


val s3 = union(s1, s2)
assert(s3(4))
assert(s3(5))
assert(!s3(6))

val s4 = intersect(set2, s1)
assert(!s4(3))
assert(s4(4))
assert(!s4(5))

val s5 = diff(set2, s1)
assert(s5(2))
assert(!s5(4))
assert(s5(6))

val s6 = filter(set2, pred)
assert(!s6(2))
assert(s6(4))
assert(!s6(5))
assert(s6(8))


def pred2():Set = elem => elem%2 == 0
def posit(): Set = elem => elem > 0
def ss= filter(posit(), pred2())
assert(!ss(1))
assert(ss(2))
assert(!ss(-1))
assert(!ss(0))
assert(!ss(-2))
assert(ss(4))

assert(forall(s6, elem => (elem%2==0)))
assert(!forall(s6, elem => (elem%13==0)))

assert(exists(s6, elem => (elem%5 == 0)))
assert(!exists(s6, elem => (elem == 13)))

val s7 = mapOneLine(s6, elem=>(elem+1))
assert(!s7(4))
assert(s7(5))
assert(!s7(8))
assert(s7(9))
