package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]) : Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: List[Double]) : Double =
    ds match {
      case Nil => 0
      case Cons(0.0, _) => 0
      case Cons(x, xs) => x * product(xs)
    }

  def apply[A](as: A*) : List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2: Implement tail
  def tail[A](l: List[A]) : List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }
  // scala> List.tail(List(1,2,3,4))
  // res1: List[Int] = Cons(2,Cons(3,Cons(4,Nil)))

  // Exercise 3.3: Implement setHead
  def setHead[A](l: List[A], e: A) : List[A] =
    l match {
      case Nil => Cons(e, Nil)
      case Cons(h, t) => Cons(e, t)
    }
  // scala> List.setHead(List(1,2,3,4), 11)
  // res2: List[Int] = Cons(11,Cons(2,Cons(3,Cons(4,Nil))))

  // Exercise 3.4: Implement drop
  @tailrec
  def drop[A](l: List[A], n: Int) : List[A] =
    if(n <= 0) l
    else drop(tail(l), n-1)
  // scala> List.drop(List(1,2,3,4), 2)
  // res3: List[Int] = Cons(3,Cons(4,Nil))

  // Exercise 3.5: Implement dropWhile
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean) : List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => if(!f(h)) l else dropWhile(t, f)
    }
  // scala> List.dropWhile(List(1,2,3,4), (x: Int) => x <= 3)
  // res8: List[Int] = Cons(4,Nil)

  def append[A](a1: List[A], a2: List[A]) : List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // Exercise 3.6: Implement init which returns a list of all elements but the last
  def init[A](l: List[A]) : List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  // scala> init(List(1,2,3,4,5))
  // res9: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil))))

  def dropWhile2[A](as: List[A])(f: A => Boolean) : List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // Exercise 3.7: Can execution be halted under cerain condition?

  // Exercise 3.8: Relationship between foldRight and data constructors of List
  // scala> List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  // res0: List[Int] = Cons(1,Cons(2,Cons(3,Nil)))

  // Exercise 3.9: Implement length using foldRight
  def length[A](l: List[A]) : Int = foldRight(l, 0)((x,y) => y + 1);
  // scala> List.length(List(1,2,3,4,5,6,7,8))
  // res4: Int = 8
  //
  // scala> List.length(List(11,12,13,14,15,16,17,18))
  // res5: Int = 8
  //
  // scala> List.length(List(0,11,12,13,14,15,16,17,18,0))
  // res6: Int = 10

  // Exercise 3.10: Implement tail-recursive foldLeft
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B) : B =
   as match {
     case Nil => z
     case Cons(h, t) => foldLeft(t, f(z,h))(f)
   }

  // Exercise 3.11: Implement sum, product and length using foldLeft
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length3[A](l: List[A]) : Int = foldLeft(l, 0)((x,y) => x + 1);
  // scala> List.sum3(List(1,2,3,4))
  // res1: Int = 10
  //
  // scala> List.product3(List(1,2,3,4))
  // res2: Double = 24.0
  //
  // scala> List.length(List(0,11,12,13,14,0))
  // res3: Int = 6

  // Exercise 3.12: Implement a function that returns the reverse of a list
  def reverse[A](l: List[A]) : List[A] = foldLeft(l, Nil:List[A])((t,h) => Cons(h,t))
  // scala> List.reverse(List(1,2,3))
  // res4: List[Int] = Cons(3,Cons(2,Cons(1,Nil)))

  // Exercise 3.13: Write foldLeft in terms of foldRight and viceversa
  def foldLeft2[A,B](as: List[A], z: B)(f: (B,A) => B) : B = ???
  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B) : B = ???

  // Exercise 3.14: Implement append in terms of either foldLeft or foldRight
  def append2[A](a1: List[A], a2: List[A]) : List[A] = foldLeft(reverse(a1), a2)((t,h) => Cons(h,t))
  // scala> List.append2(List(1,2), List(3,4))
  // res0: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil))))
  def append3[A](a1: List[A], a2: List[A]) : List[A] = foldRight(a1, a2)(Cons(_,_))
  // scala> List.append3(List(1,2), List(3,4))
  // res1: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil))))

  // Exercise 3.15: Write a function that concatenates a list of lists into a single list
  def concat[A](l: List[List[A]]) : List[A] = foldLeft(l, Nil:List[A])(append(_,_))
  // scala> List.concat(List(List(1,2),List(3,4)))
  // res5: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil))))concat(List(List(1,2),List(3,4)))

  // Exercise 3.16: Write a function that adds 1 to each element of a list of ints
  def plus1(l: List[Int]) : List[Int] = foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))
  // scala> List.plus1(List(1,2,3))
  // res7: List[Int] = Cons(2,Cons(3,Cons(4,Nil)))

  // Exercise 3.17: Write a function that turns each value of a list of doubles into a string
  def asString(l: List[Double]) : List[String] = foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))
  // scala> List.asString(List(1.2,3.4,5.6))
  // res9: List[String] = Cons(1.2,Cons(3.4,Cons(5.6,Nil)))

  // Exercise 3.18: Implement map
  def map[A,B](as: List[A])(f: A => B) : List[B] = foldRight(as, Nil:List[B])((h,t) => Cons(f(h),t))
  // scala> List.map(List(1,2,3))(_.toString+"...")
  // res10: List[String] = Cons(1...,Cons(2...,Cons(3...,Nil)))

  // Exercise 3.19: Implement filter
  def filter[A](as: List[A])(f: A => Boolean) : List[A] = foldRight(as, Nil:List[A])((h,t) => if(f(h)) Cons(h,t) else t)
  // scala> List.filter(List(1,2,3,4))(_ % 2 == 0)
  // res0: List[Int] = Cons(2,Cons(4,Nil))

  // Exercise 3.20: Implement flatMap
  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = foldRight(as, Nil:List[B])((h,t) => append(f(h),t))
  // scala> List.flatMap(List(1,2,3,4))(i => List(i,i))
  // res1: List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Cons(4,Cons(4,Nil))))))))

  // Exercise 3.21: Implement filter in terms of flatMap
  def filter2[A](as: List[A])(f: A => Boolean) : List[A] = flatMap(as)(a => if(f(a)) List(a) else Nil)
  // scala> List.filter2(List(1,2,3,4))(_ % 2 == 0)
  // res2: List[Int] = Cons(2,Cons(4,Nil))

  // Exercise 3.22: Write a function that adds 2 lists of integers
  def addLists[A](a1: List[A], a2: List[A]) : List[A] = ???
  // scala> List.addLists(List(1,2,3), List(4,5,6))

  // Exercise 3.23: Implement zipWith as a generalization of the previous function
  def zipWith[A,B,C](a1: List[A], a2: List[B])(f: (A,B) => C) = ???
  // scala> List.zipWith(List(1,2,3), List(4,5,6))(_ + _)

  // Exercise 3.24: Implement hasSubsequence
  def hasSubsequence[A](sup: List[A], sub: List[A]) : Boolean = ???
  // scala> List.hasSubsequence(List(1,2,3,4), List(2,3))
  // scala> List.hasSubsequence(List(1,2,3,4), List(2,4))

}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25: Write a function that counts the number of nodes
  def size[A](t: Tree[A]) : Int =
    t match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  // scala> Tree.size(Leaf(0))
  // res0: Int = 1
  //
  // scala> Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))
  // res1: Int = 5

  // Exercise 3.26: Write a function that return the maximun value of the tree
  def maximum(t: Tree[Int]) : Int =
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  // scala> Tree.maximum(Leaf(5))
  // res1: Int = 5
  //
  // scala> Tree.maximum(Branch(Branch(Leaf(10), Leaf(2)), Leaf(3)))
  // res2: Int = 10
  //
  // scala> Tree.maximum(Branch(Branch(Leaf(10), Leaf(2)), Leaf(13)))
  // res3: Int = 13
  //
  // scala> Tree.maximum(Branch(Branch(Leaf(10), Leaf(20)), Leaf(13)))
  // res4: Int = 20

  // Exercise 3.27: Implement depth
  def depth[A](t: Tree[A]) : Int =
    t match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  // scala> Tree.depth(Leaf(0))
  // res5: Int = 1
  //
  // scala> Tree.depth(Branch(Leaf(0),Leaf(1)))
  // res6: Int = 2
  //
  // scala> Tree.depth(Branch(Leaf(0), Branch(Leaf(1),Leaf(2))))
  // res7: Int = 3
  //
  // scala> Tree.depth(Branch(Leaf(0), Branch(Branch(Leaf(1),Leaf(3)),Leaf(2))))
  // res8: Int = 4

  // Exercise 3.28: Implement map
  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  // scala> Tree.map(Leaf(1))(_ + 1)
  // res9: Tree[Int] = Leaf(2)
  //
  // scala> Tree.map(Branch(Leaf(0), Branch(Branch(Leaf(1),Leaf(3)),Leaf(2))))(_ * 2)
  // res10: Tree[Int] = Branch(Leaf(0),Branch(Branch(Leaf(2),Leaf(6)),Leaf(4)))

  // Exercise 3.29: Implement fold and reimplement all the previous functions in terms of fold
  def fold[A,B](t: Tree[A])(map: A => B)(f: (B,B) => B) : B =
    t match {
      case Leaf(v) => map(v)
      case Branch(l, r) => f(fold(l)(map)(f), fold(r)(map)(f))
    }
  def size2[A](t: Tree[A]) : Int = fold(t)(v => 1)((l,r) => 1 + l + r)
  // scala> Tree.size2(Leaf(0))
  // res0: Int = 1
  //
  // scala> Tree.size2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))
  // res1: Int = 5
  def maximum2(t: Tree[Int]) : Int = fold(t)(v => v)((l,r) => l max r)
  // scala> Tree.maximum2(Leaf(5))
  // res2: Int = 5
  //
  // scala> Tree.maximum2(Branch(Branch(Leaf(10), Leaf(2)), Leaf(3)))
  // res3: Int = 10
  //
  // scala> Tree.maximum2(Branch(Branch(Leaf(10), Leaf(2)), Leaf(13)))
  // res4: Int = 13
  //
  // scala> Tree.maximum2(Branch(Branch(Leaf(10), Leaf(20)), Leaf(13)))
  // res5: Int = 20
  def depth2[A](t: Tree[A]) : Int = fold(t)(v => 1)((l,r) => 1 + (l max r))
  // scala> Tree.depth2(Leaf(0))
  // res6: Int = 1
  //
  // scala> Tree.depth2(Branch(Leaf(0),Leaf(1)))
  // res7: Int = 2
  //
  // scala> Tree.depth2(Branch(Leaf(0), Branch(Leaf(1),Leaf(2))))
  // res8: Int = 3
  //
  // scala> Tree.depth2(Branch(Leaf(0), Branch(Branch(Leaf(1),Leaf(3)),Leaf(2))))
  // res9: Int = 4
  def map2[A,B](t: Tree[A])(f: A => B) : Tree[B] = fold(t)(v => Leaf(f(v)):Tree[B])((l,r) => Branch(l,r))
  // scala> Tree.map2(Leaf(1))(_ + 1)
  // res10: Tree[Int] = Leaf(2)
  //
  // scala> Tree.map2(Branch(Leaf(0), Branch(Branch(Leaf(1),Leaf(3)),Leaf(2))))(_ * 2)
  // res11: Tree[Int] = Branch(Leaf(0),Branch(Branch(Leaf(2),Leaf(6)),Leaf(4)))

}
