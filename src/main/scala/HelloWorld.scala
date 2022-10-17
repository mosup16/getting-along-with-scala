import scala.annotation.tailrec

object HelloWorld {
  def concat(s1: String): String => String = (s2: String) => s1 + s2

  def factorial(n: Int): Int = if (n <= 0) 1 else n * factorial(n - 1)

  def factorialTailrec(n: Int): Int = {
    @tailrec
    def go(n: Int, factOfPrev: Int): Int = if (n <= 0) factOfPrev else go(n - 1, factOfPrev * n)

    go(n, 1)
  }

  def fibonacci(n: Long): Long = if (n == 1 || n == 0) n else fibonacci(n - 1) + fibonacci(n - 2)

  val fib: Int => Int = n => if (n == 1 || n == 0) n else fib(n - 1) + fib(n - 2)

  def fibonacciTailrec(n: Long): Long = {
    @tailrec
    def go(n: Long, prev: Long, prevPerv: Long): Long = if (n == 0 || n == 1) prev else go(n - 1, prev + prevPerv, prev)

    go(n, 1, 0)
  }

  def binarySearch[T](data: Array[T], key: T, greaterThan: (T, T) => Boolean): Option[T] = {
    def go(data: Array[T], key: T, gt: (T, T) => Boolean): (Int, Int) => Option[T] = {
      @tailrec
      def f(start: Int, end: Int): Option[T] = {
        if (start > end) Option.empty[T] else {
          val mid = (end - start) / 2
          if (data(mid).equals(key)) Option(key) else if (gt(data(mid), key)) f(start, mid) else f(start, mid)
        }
      }

      f //returns a function that dose the search
    }

    go(data, key, greaterThan)(0, data.length)
  }

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def partial2[A, B, C]: (A, (A, B) => C) => B => C = (a, f) => b => f(a, b)

  def partial3[A, B, C](a: A, f: (A, B) => C): B => C = {
    def f2(b: B): C = f(a, b)

    f2
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def unCurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))

  def higherOrderFunc(f: Int => Int): Int => Int = f

  trait Animal

  case class Dog(name: String) extends Animal

  case class Cat(name: String) extends Animal

  def patternMatch(animal: Animal): String = {
    animal match {
      case Dog(name) => s"process dog $name"
      case Cat(name) => s"process cat $name"
      case _ => "unknown"
    }
  }
  def main(args: Array[String]): Unit = {
    //    this concat "hello" apply "world"
    println(concat("hello ")("world"))
    println(factorial(5))
    println(fibonacciTailrec(115) + " tail recursion fibonacci")
    println(fib(5))
    println(higherOrderFunc(factorial)(5))
    println(factorialTailrec(5))
    new Hello().sayHello()
    val a = Array[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
    val found = binarySearch[Int](a, 5, (n1, n2) => n1 > n2).getOrElse(throw new RuntimeException)
    println(found)
    println(patternMatch(Dog("do")))

  }
}