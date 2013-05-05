package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
    assert(sentenceOccurrences(List("Brandy", "Alexander")) === List(('a',3), ('b',1), ('d',2), ('e',2), ('l',1), ('n',2), ('r',2), ('x',1), ('y',1)))
  }

  test("dictionaryByOccurrences.get") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
    assert(dictionaryByOccurrences.get(List(('a', 1), ('b', 1), ('t', 1))).map(_.toSet) === Some(Set("bat", "tab")))
    assert(dictionaryByOccurrences.get(List(('a', 2))).map(_.toSet) === None)
  }

  test("word anagrams") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
    assert(wordAnagrams("asynchronously").toSet === Set("asynchronously"))
    assert(wordAnagrams("bat").toSet === Set("bat", "tab"))
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }


  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    // assert(subtract(lard, r) === lad)

    val larrrd = List(('a', 1), ('d', 1), ('l', 1), ('r', 3))
    val rr = List(('r', 2))
    // assert(subtract(larrrd, rr) === lard)

    val laarrrd = List(('a', 2), ('d', 1), ('l', 1), ('r', 3))
    val laard = List(('a', 2), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(laarrrd, rr) === laard)
  }

  // test("sentence anagrams: []") {
  //   val sentence = List()
  //   assert(sentenceAnagrams(sentence) === List(Nil))
  // }

  // val sentenceOccur = sentenceOccurrences(List("Linux", "rulez"))
  // val occur = combinations(sentenceOccur)
  // sentenceOccur foreach println
  // println("---------")
  // println(occur.tail.head)
  // subtract(sentenceOccur, (occur.tail.head)) foreach println

  //   for {
  //     occur <- combinations(sentOccur)
  //     if (dictionaryByOccurrences.getOrElse(occur, Nil) != Nil && dictionaryByOccurrences.getOrElse(subtract(sentOccur, occur), Nil) != Nil)
  // } yield { println(dictionaryByOccurrences(occur))}


  // test("sentence anagrams: Linux rulez") {
  //   val sentence = List("Linux", "rulez")
  //   val anas = List(
  //     List("Rex", "Lin", "Zulu"),
  //     List("nil", "Zulu", "Rex"),
  //     List("Rex", "nil", "Zulu"),
  //     List("Zulu", "Rex", "Lin"),
  //     List("null", "Uzi", "Rex"),
  //     List("Rex", "Zulu", "Lin"),
  //     List("Uzi", "null", "Rex"),
  //     List("Rex", "null", "Uzi"),
  //     List("null", "Rex", "Uzi"),
  //     List("Lin", "Rex", "Zulu"),
  //     List("nil", "Rex", "Zulu"),
  //     List("Rex", "Uzi", "null"),
  //     List("Rex", "Zulu", "nil"),
  //     List("Zulu", "Rex", "nil"),
  //     List("Zulu", "Lin", "Rex"),
  //     List("Lin", "Zulu", "Rex"),
  //     List("Uzi", "Rex", "null"),
  //     List("Zulu", "nil", "Rex"),
  //     List("rulez", "Linux"),
  //     List("Linux", "rulez")
  //   )
  //   assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  // }

}
