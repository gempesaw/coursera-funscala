package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val leafy = Leaf('d', 6)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)

      val t3 = makeCodeTree(t1, t2)
      assert(weight(t3) === 14)
    }
  }

  test("weight of a leaf") {
    new TestTrees {
      assert(weight(leafy) === 6)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of a leaf") {
    new TestTrees {
      assert(chars(leafy) === List('d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times example") {
    assert(times(List('a')) === List(('a', 1)))
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    assert(times(List('b', 'b', 'a')) === List(('b', 2), ('a', 1)))
    assert(times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')) === List(('l',3), ('o',2), (' ',1), (',',1), ('d',1), ('e',1), ('h',1), ('r',1), ('w',1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2 ), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
    assert(makeOrderedLeafList(List(('a', 2), ('b', 1))) === List(Leaf('b', 1), Leaf('a', 2)))
    assert(makeOrderedLeafList(List(('b', 2), ('a', 1))) === List(Leaf('a', 1), Leaf('b', 2)))
    assert(makeOrderedLeafList(times(string2Chars("hello, world"))) === List(Leaf(' ',1), Leaf(',',1), Leaf('d',1), Leaf('e',1), Leaf('h',1), Leaf('r',1), Leaf('w',1), Leaf('o',2), Leaf('l',3)))
  }

  test("singleton") {
    assert(singleton(List(Leaf('b', 1))) === true)
    assert(singleton(List(Leaf('a',2) )) === true )
    assert(singleton(List(Fork(Leaf('a',2), Leaf('b',3), List('a'), 5))) === true )
    assert(singleton(List(Leaf('a',2) , Leaf('x',2) )) === false)
  }

  // test("combine of some leaf list") {
  //   val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
  //   assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  // }

  // test("decode and encode a very short text should be identity") {
  //   new TestTrees {
  //     assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
  //   }
  // }
}
