package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin") = forAll{ (h: H) => 
    val m = if(isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("deleteMin") = forAll{ (h: H) => 
    val m = if(isEmpty(h)) 0 else findMin(h)
    val x = deleteMin(insert(m, h))
    if(isEmpty(x)) true
    else findMin(x) >= m
  }

  property("deleteMin2") = forAll{ a: Int => 
    val h = insert(2, insert(3, insert(1, empty)))
    findMin(deleteMin(h)) == 2
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val m1 = if(isEmpty(h1)) 0 else findMin(h1)
    val m2 = if(isEmpty(h2)) 0 else findMin(h2)
    
    val h3 = insert(m1, h1)
    val h4 = insert(m2, h2)
    val m = if(m1 <= m2) m1 else m2
    findMin(meld(insert(m1, h1), insert(m2, h2))) == m
  }


  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
