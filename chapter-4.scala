package fpinscala.datastructures

import scala.math

sealed trait Option[+A] {
  // Exercise 4.1: Implement the following functions
  def map[B](f: A => B) : Option[B]
  def flatMap[B](f: A => Option[B]) : Option[B]
  def getOrElse[B >: A](default: => B) : B
  def orElse[B >: A](ob: => Option[B]) : Option[B]
  def filter(f: A => Boolean) : Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  def map[B](f: A => B) : Option[B] = Some(f(get))
  def flatMap[B](f: A => Option[B]) : Option[B] = f(get)
  def getOrElse[B >: A](default: => B) : B = get
  def orElse[B >: A](ob: => Option[B]) : Option[B] = this
  def filter(f: A => Boolean) : Option[A] = if(f(get)) this else None
}
// scala> Some(1).map(_ * 3)
// res0: Option[Int] = Some(3)
//
// scala> Some(2).flatMap(v => Some(v+5))
// res1: Option[Int] = Some(7)
//
// scala> Some(1).getOrElse(Some(2))
// res2: Any = 1
//
// scala> Some(1).orElse(Some(4))
// res3: Option[Int] = Some(1)
//
// scala> Some(1).filter(_ % 2 == 0)
// res4: Option[Int] = None
//
// scala> Some(2).filter(_ % 2 == 0)
// res5: Option[Int] = Some(2)

case object None extends Option[Nothing] {
  def map[B](f: Nothing => B) : Option[B] = this
  def flatMap[B](f: Nothing => Option[B]) : Option[B] = this
  def getOrElse[B >: Nothing](default: => B) : B = default
  def orElse[B >: Nothing](ob: => Option[B]) : Option[B] = ob
  def filter(f: Nothing => Boolean) : Option[Nothing] = this
}
// scala> None.map(v => 1)
// res10: Option[Int] = None
//
// scala> None.flatMap(v => Some(1))
// res11: Option[Int] = None
//
// scala> None.getOrElse(1)
// res12: Int = 1
//
// scala> None.orElse(Some(1))
// res13: Option[Int] = Some(1)
//
// scala> None.filter(v => false)
// res14: Option[Nothing] = None

def mean(xs: Seq[Double]) : Option[Double] =
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)

// Exercise 4.2: Implement variance using flatMap
def variance(xs: Seq[Double]) : Option[Double] = mean(xs).flatMap(m => mean(xs.map(d => math.pow(d-m,2))))
// scala> variance(Nil)
// res16: Option[Double] = None
//
// scala> mean(List(1,2,3))
// res17: Option[Double] = Some(2.0)
//
// scala> variance(List(1,2,3))
// res18: Option[Double] = Some(0.6666666666666666)

def lift[A,B](f: A => B) : Option[A] => Option[B] = _ map f

def Try[A](a: => A) : Option[A] =
  try {
    Some(a)
  } catch {
    case e: Exception => None
  }

// Exercise 4.3: Write a map2 function to lift functions with 2 parameters
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C) : Option[C] =
  a.flatMap(va => b.flatMap(vb => Some(f(va,vb))))
// scala> map2(None,None)((x,y)=>1)
// res21: Option[Int] = None
//
// scala> map2(Some(1),None)((x,y)=>1)
// res22: Option[Int] = None
//
// scala> map2(Some(1),Some(2))((x,y)=>x+y)
// res23: Option[Int] = Some(3)

// Exercise 4.4: Implement sequence
def sequence[A](a: List[Option[A]]) : Option[List[A]] =
  a.foldRight(Some(Nil):Option[List[A]])(map2(_,_)(_::_))
def parseInts(a: List[String]): Option[List[Int]] =
  sequence(a map (i => Try(i.toInt)))
// scala> parseInts(List("3","2","1"))
// res9: Option[List[Int]] = Some(List(3, 2, 1))
//
// scala> parseInts(List("hello","2","1"))
// res10: Option[List[Int]] = None
//
// scala> parseInts(List("3","2","world"))
// res11: Option[List[Int]] = None

// Exercise 4.5: Implement traverse that iterates over the list only once and rewrite sequence in terms of traverse
def traverse[A,B](a: List[Option[A]])(f: A => Option[B]) : Option[List[B]] =
  a.foldRight(Some(Nil):Option[List[B]])((a,b) => map2(a.flatMap(f),b)(_::_))
def sequence2[A](a: List[Option[A]]) : Option[List[A]] = traverse(a)(v => Some(v))
def parseInts2(a: List[String]): Option[List[Int]] =
  sequence2(a map (i => Try(i.toInt)))
// scala> parseInts2(List("3","2","1"))
// res12: Option[List[Int]] = Some(List(3, 2, 1))
//
// scala> parseInts2(List("hello","2","1"))
// res13: Option[List[Int]] = None
//
// scala> parseInts2(List("3","2","world"))
// res14: Option[List[Int]] = None


sealed trait Either[+E, +A] {
  // Exercise 4.6: Implement these functions
  def map[B](f: A => B) : Either[E, B] =
    this match {
      case l: Left[E] => l
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE,B]) : Either[EE,B] =
    this match {
      case l: Left[E] => l
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE,B]) : Either[EE,B] =
    this match {
      case l: Left[E] => b
      case r => r
    }

  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C) : Either[EE,C] =
    this match {
      case l: Left[E] => l
      case Right(a) => b.map(b => f(a,b))
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def meanEither(xs: Seq[Double]) : Either[String,Double] =
  if(xs.isEmpty) Left("mean of empty list")
  else Right(xs.sum / xs.length)

def TryEither[A](a: => A) : Either[Exception,A] =
  try {
    Right(a)
  } catch {
    case e: Exception => Left(e)
  }

// Exercise 4.7: Implement sequence and traverse using Either type
def traverseEither[E,A,B](as: List[Either[E,A]])(f: A => Either[E,B]) : Either[E,List[B]] =
  as.foldRight(Right(Nil):Either[E,List[B]])((a,r) => a.flatMap(f).map2(r)(_::_))
def sequenceEither[E,A](es: List[Either[E,A]]) : Either[E,List[A]] = traverseEither(es)(a => Right(a))
// scala> sequenceEither(List(Right(1),Right(2),Right(3)))
// res16: Either[Nothing,List[Int]] = Right(List(1, 2, 3))
//
// scala> sequenceEither(List(Left("wrong"),Right(2),Right(3)))
// res17: Either[String,List[Int]] = Left(wrong)
//
// scala> sequenceEither(List(Right(1),Right(2),Left("bad")))
// res18: Either[String,List[Int]] = Left(bad)
//
// scala> sequenceEither(List(Left("wrong"),Right(2),Left("bad")))
// res19: Either[String,List[Int]] = Left(wrong)

// Exercise 4.8: How can multiple errors be reported?
