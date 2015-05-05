package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]) : Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def products(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * products(xs)
  }
  
  def tail[A](list: List[A]) : List[A] = list match {
    case Nil => throw new Exception("No tail found")
    case Cons(x, xs) => xs
  }

  def drop[A](list: List[A], n: Int): List[A] = 
    if(n == 0) list
    else list match {
      case Nil => throw new Exception("Nothing to drop")
      case Cons(x, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean) : List[A] = l match {
      case Nil => throw new Exception("Nothing to drop")
      case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else l
    }
  
  
  def setHead[A](head: A, list: List[A]) : List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(x, xs) => Cons(head, xs)
  }

  def init[A](l : List[A]) : List[A] = 

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

sealed trait Try[T] {
  def map[S](f: T => S) : Try[S] = this match {
    case Success(e) => Try(f(e))
    case failure@Failure(t) => failure
  }
}
case class Success[T](elem: T) extends Try[T]
case class Failure[T](t: Throwable) extends Try[T]


object Try {
  def apply[T](r: => T): Try[T] = {
    try {
      Success(r)
    }
    catch:q
    { case t => Failure(t) }
  }
}
