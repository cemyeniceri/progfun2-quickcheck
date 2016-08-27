package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("smallest one") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == a.min(b)
  }

  property("isEmpty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    isEmpty(h1)
  }

  property("smallest of meldiengs") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    findMin(h3) == findMin(h1).min(findMin(h2))
  }

  property("isNotEmpty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    !isEmpty(h)
  }

  property("inserting_sequence") = forAll { (l: List[A]) =>
    val h = l.foldRight(empty)(insert)
    asList(h) == l.sorted
  }
  property("asList_should_be_sorted") = forAll { (h: H) =>
    asList(h).sorted == asList(h)
  }

  def asList(h: H): List[A] = // the sorted list resulting from extracting all elements of h
    if (isEmpty(h)) List() else findMin(h) :: asList(deleteMin(h))
}
