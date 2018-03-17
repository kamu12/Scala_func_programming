trait  IntSet{
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
  def union(that: IntSet): IntSet
  def map(f: Int => Int): IntSet
}

case object Empty extends IntSet{
  override def contains(x: Int): Boolean =
    false

  override def include(x: Int): IntSet =
    NonEmpty(x, Empty, Empty)

  override def union(that: IntSet): IntSet = that

  def map(f: Int => Int): IntSet = this

  override def toString: String = "."
}

case class NonEmpty(el: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean =
    if(x < el) left contains x
    else if(x > el) right contains x
    else true

  override def include(x: Int): IntSet =
    if (x < el) NonEmpty(el, left include x, right)
    else if (x > el) NonEmpty(el, left, right include x)
    else this

  override def union(that: IntSet): IntSet =
    ((left union that) union right) include el

  def map(f: Int => Int): IntSet =
    (left map f) union (right map f) include f(el)

  override def toString: String = "{" + left + el + right + "}"
}

val set1 = Empty include 7 include 5 include 12 include 9 include 15

val set2 = Empty include 8 include 13

set1 union set2

set2 union set1

set1 map (x => -x)