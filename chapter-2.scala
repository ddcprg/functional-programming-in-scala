import scala.annotation.tailrec

/**
 * Exercise 2.1: Write a tail-recursive function to get the nth Fibonacci number
 */
def fib(n: Int): Int = {
  @tailrec
  def go(n: Int, fa: Int, fb: Int): Int = {
    if (n == 0) fa
    else go(n-1, fb, fa+fb)
  }
  go(n, 0, 1)
}

// http://www.miniwebtool.com/list-of-fibonacci-numbers/?number=100
assert(fib(0) == 0)
assert(fib(1) == 1)
assert(fib(2) == 1)
assert(fib(3) == 2)
assert(fib(4) == 3)
assert(fib(5) == 5)
assert(fib(6) == 8)
assert(fib(7) == 13)
assert(fib(15) == 610)
assert(fib(40) == 102334155)


/**
 * Exercise 2.2: Implement isSorted
 */
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @tailrec
  def go(i: Int): Boolean = {
    if (i == as.length-1) true
    else if (ordered(as(i), as(i+1))) go(i+1)
    else false
  }
  go(0)
}

assert(isSorted(Array(0,1,2,3,4,4,5), (x:Int, y:Int) => x <= y))
assert(!isSorted(Array(0,1,2,1,3,4,5), (x:Int, y:Int) => x <= y))
assert(isSorted(Array(5,5,4,3,2,1,0), (x:Int, y:Int) => x >= y))
assert(!isSorted(Array(4,5,4,3,2,1,0), (x:Int, y:Int) => x >= y))


/**
 * Exercise 2.3: Implement curry
 */
def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
  a: A => b: B => f(a, b)
}


/**
 * Exercise 2.4: Implement uncurry
 */
def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
  (a: A, b: B) => f(a)(b)
}


/**
 * Exercise 2.5: Implement compose
 */
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  a: A => f(g(a))
}
