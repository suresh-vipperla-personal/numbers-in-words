package learn
import org.apache.logging.log4j.LogManager

object Tree {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t:Tree[A]):Int  = t match{
    case Leaf(_) => 1
    case Branch(left,right) => size(left)+size(right)+1
  }
  def max(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => max(l) max max(r)
  }
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
  def map[A,B] (t:Tree[A]) (f: A => B) :Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l) (f),map(r) (f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l,r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }

  def map2[A,B] (t:Tree[A]) (f: A => B) :Tree[B] = fold(t) (a => Leaf(f(a)): Tree[B]) ( (a,b) => Branch(a,b) )

  def size2[A](t:Tree[A]):Int  = fold(t) (_=>1) (1 + _ + _)
  def max2(t:Tree[Int]):Int  = fold(t) (n=>n) (_ max _)

  def depth2[A](t: Tree[A]): Int = fold(t) (_ =>0) ((d1,d2) => 1 + (d1 max d2))


  def main(args: Array[String]) {
    val log = LogManager.getLogger("com.starter.Main")
    log.info("Testing Trees!")
    val newTree= Branch(Branch(Leaf(6),Branch(Leaf(7),Leaf(9))),Leaf(5))
    log.info("size  of tree via fold is: "+Tree.size2(newTree))
    log.info("max of tree is: "+Tree.max(newTree))
    log.info("max of tree via fold: "+Tree.max2(newTree))
    log.info("max depth of tree is: "+Tree.depth(newTree))
    log.info("max depth of tree via fold is: "+Tree.depth2(newTree))
  }
}
