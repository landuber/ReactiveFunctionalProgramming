object MyModule {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int) : Int = 
      if(n > 1) go(b, a + b, n - 1)
      else a
    go(0, 1, n)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): (B => C) = {
    (b: B) =>
      f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = 
    (a: A) => (b: B) => f(a, b) 

  def uncurry[A, B, C](f: A => B => C) : (A, B) => C = 
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B) : A => C =
    (a: A) => f(g(a))

  def main(input: Array[String]) : Unit = {
    val r = new Rectangular {

    }
  }
}

class Point(val x: Int, val y: Int)
trait Recatangular {
  def topLeft: Point
  def bottomRight: Point

  def left = topLeft.x
  def right = bottomRight.x
  def top = topLeft.y
  def bottom = bottomRight.y
}
