package learn

import org.apache.logging.log4j.LogManager

import scala.annotation.tailrec


object FunDS {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](l: List[A], n: Int): List[A] = {
      if (n > 0) l match {
        case Nil => sys.error("tail of empty list")
        case Cons(_, t) => tail(t, n - 1)
      }
      else l
    }

    def setHead[A](l: List[A], newHead: A): List[A] = l match {
      case Nil => sys.error("attempt to set head  of empty list")
      case Cons(_, t) => Cons(newHead, t)
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def length[A](list: List[A]): Int = list match {
      case Nil => 0
      case Cons(_, xs) => 1 + length(xs)
    }

    def reverse[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(h, t) => append(reverse(t), Cons(h, Nil))
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      // case Nil => sys.error ("Attempting to remove elements from empty list!")
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

    /* def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil =>  z
        case Cons(x, xs) =>   foldLeft(xs,f(z,x)) (f)
      }
    }*/
    /*def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
     as match {
       case Nil => z
       case Cons(x, xs) => f(x, foldRight(xs, z)(f))
     }*/
    def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((h, t) => Cons(h, t))


    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("attempt to init empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
      case Cons(head, tail) => Cons(head, init(tail))
    }


    def sum2(ns: List[Int]) =
      foldRight(ns, 0)(_ + _)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    def length2[A](as: List[A]): Int =
      foldRight(as, 0)((_, b: Int) => 1 + b)

    @tailrec
    def sumTailRec(ints: List[Int], z: Int): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => sumTailRec(xs, x + z)
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }

    def productTailRec(ns: List[Double]) =
      foldLeft(ns, 1.0)(_ * _)

    def reverse2[A](list: List[A]): List[A] =
      foldRight(list, List[A]())((acc, h) => Cons(acc, h))

    def concat[A](l: List[List[A]]): List[A] =
      foldRight(l, Nil: List[A])(append)

    def addOne(list: List[Int]): List[Int] =
      foldRight(list, Nil: List[Int])((h, t) => Cons(h + 1, t))

    def listDouble2String(list: List[Double]): List[String] =
      foldRight(list, Nil: List[String])((d, t) => Cons(d.toString, t))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

    def map_2[A, B](l: List[A])(f: A => B): List[B] = {
      val buf = new collection.mutable.ListBuffer[B]

      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h, t) => buf += f(h); go(t)
      }

      go(l)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }


    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h, t) => if (f(h)) buf += h; go(t)
      }

      go(as)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))


    def filter_With_flatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

    //def filter_With_Map[A](as: List[A])(f: A => Boolean): List[A] =  map(as) (a=> if (f(a)) a else AnyRef)
    def addList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addList(t1, t2))
    }

    def zipWith[A, B, C](l1: List[A], l2: List[B]) (f: (A,B)=>C) : List[C] = (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2) (f))
    }

    def addList_withZip (l1: List[Int], l2: List[Int]): List[Int] =  zipWith(l1,l2) (_+_)
  }



  def main(args: Array[String]) {
    val log = LogManager.getLogger("com.starter.Main")
    log.info("Hello from scala-gradle-starter! 🚀")
    log.info(List.product(List (1,2,3)))
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    log.info("pattern match test1:"+x)

    //log.info("New Head1 "+List.setHead(List(1,2,3,4,5),8))
    //log.info("New Head2 "+List.setHead(List(1),8))
    //log.info("New Head2 "+List.setHead(List(),8))

    //log.info("tail test 1: "+List.tail(List(1,2,3,4,5),2))

    log.info("Drop While Test1: "+List.dropWhile (List(2, 4, 6, 8, 10)) (x => (x%2)!=0))
    log.info("Drop While Test2: "+List.dropWhile (List(2, 4, 6, 8, 10,11)) (x => (x%2)==0))
    log.info("Init test1: "+List.init(List(1,2,3,4,5)))
    log.info("Init test2: "+List.init(List(1,2,3)))
    log.info("Init test3: "+List.init(List(1,2)))
    log.info("Init test4: "+List.init(List(1)))
    //log.info("Init test5: "+List.init(List()))
    log.info("Length using foldRight:"+List.length2(List(1,2,3)))
    log.info("Length using Product Tail Rec:"+List.productTailRec(List(1,2,3)))
    log.info("Reverse : "+List.reverse(List(2, 4, 6, 8, 10)))
    log.info("Append: "+List.append2(List(1,2,3),List(5,6)))
    log.info("addOne: "+List.addOne(List(1,2,3)))
    log.info("listDouble2String: "+List.listDouble2String(List(1.0,5.9/6.7,5.87)))
    log.info("Map: "+List.map(List(1,2,3,4,5)) (a=>a*2))
    log.info("FlatMap: "+List.flatMap(List(1,2,3))(i => List(i,i)))
    log.info("AddList:"+List.addList(List(1,2,3) , List(4,5,6)))
    log.info("AddList_withZip:"+List.addList_withZip(List(1,2,3) , List(4,5,6)))
  }
}
