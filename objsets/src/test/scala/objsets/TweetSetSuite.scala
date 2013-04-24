package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import TweetReader._

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    val leastLeft = new Tweet("a", "a tweet", 1)
    val leastRight = new Tweet("z", "z tweet", 1)
    val mostMiddle = new Tweet("m", "m tweet", 100)
    val leastMiddle = new Tweet("m", "m tweet", 1)
    val mostRight = new Tweet("z", "z tweet", 100)
    val setMostMiddle = new NonEmpty(mostMiddle, new NonEmpty(leastLeft, new Empty, new Empty), new NonEmpty(leastRight, new Empty, new Empty))
    val setMostLeft = new NonEmpty(leastMiddle, new NonEmpty(c, new Empty, new Empty), new NonEmpty(leastRight, new Empty, new Empty))
    val setMostRight = new NonEmpty(leastMiddle, new NonEmpty(c, new Empty, new Empty), new NonEmpty(mostRight, new Empty, new Empty))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: various") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user != "a")) === 3)
      assert(size(set5.filter(tw => true)) === 4)
      assert(size(set5.filter(tw => tw.retweets > 0)) === 4)
      assert(size(set5.filter(tw => tw.retweets < 0)) === 0)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("union: chains!") {
    new TestSets{
      assert(size(set2 union set3 union set4c union set4d union set5) === 4)
    }
  }

  test("union: duplicates") {
    new TestSets{
      assert(size(set1 union set1) === size(set1))
      assert(size(set5 union set5) === size(set5))
    }
  }

  test("most retweets: head of tree") {
    new TestSets{
      val tweet = setMostMiddle.mostRetweeted
      assert(tweet === mostMiddle)
    }
  }

  test("most retweets: left branch") {
    new TestSets{
      val tweet = setMostLeft.mostRetweeted
      assert(tweet === c)
    }
  }

  test("most retweets: right branch") {
    new TestSets{
      val tweet = setMostRight.mostRetweeted
      assert(tweet === mostRight)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet

      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}
