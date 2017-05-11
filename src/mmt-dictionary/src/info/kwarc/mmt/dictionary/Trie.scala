/**
  * Based on https://github.com/mauricio/scala-sandbox/blob/master/src/main/scala/trie/Trie.scala
  */
package info.kwarc.mmt.dictionary

import info.kwarc.mmt.api.{Error, GlobalName, Path}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class DictionaryTrieLanguageFromNotFound(text : String) extends Error(text)
case class DictionaryTrieLanguageToNotFound(text : String) extends Error(text)


sealed trait Trie[A] extends Traversable[String] {

  def append(key : String, data: A)
  def findByPrefix(prefix: String): Option[A]
  def contains(word: String): Boolean

}

class TrieNode[A](val char : Option[Char] = None,
                             var word: Option[String] = None,
                             var data: Option[A] = None) extends Trie[A] {

  val children: mutable.Map[Char, TrieNode[A]] = new java.util.TreeMap[Char, TrieNode[A]]().asScala

  override def append(key: String, data: A) = {
    println(key)
    @tailrec def appendHelper(node: TrieNode[A], currentIndex: Int): Unit = {
      if (currentIndex == key.length) {
        node.word = Some(key)
        node.data = Some(data)
      } else {
        val char = key.charAt(currentIndex).toLower
        val result = node.children.getOrElseUpdate(char, {
          new TrieNode[A](Some(char))
        })

        appendHelper(result, currentIndex + 1)
      }
    }

    appendHelper(this, 0)
  }

  override def foreach[U](f: String => U): Unit = {

    @tailrec def foreachHelper(nodes: TrieNode[A]*): Unit = {
      if (nodes.nonEmpty) {
        nodes.foreach(node => node.word.foreach(f))
        foreachHelper(nodes.flatMap(node => node.children.values): _*)
      }
    }

    foreachHelper(this)
  }

  override def findByPrefix(prefix: String): Option[A] = {
    @tailrec def helper(currentIndex: Int, node: TrieNode[A]) : Option[A] = {
      if (currentIndex == prefix.length) {
        data
      } else {
        node.children.get(prefix.charAt(currentIndex).toLower) match {
          case Some(child) => helper(currentIndex + 1, child)
          case None => None
        }
      }
    }

    helper(0, this)
  }

  def findByPrefix(prefix: String, max: Int): Iterable[A] = {
    var maxSize = max
    var maxNodes = 100
    var found = false
    def helper(currentIndex: Int, node: TrieNode[A], f: Function[A,Unit]) : Unit= {
      maxNodes = maxNodes - 1
//      println("c:" + node.char.getOrElse("#$") + ", i:" + currentIndex.toString)

      if (maxSize <= 0 || maxNodes <= 0 || found) {
      } else if (currentIndex < prefix.length) {
        if (currentIndex == 0 || (node.char.isDefined && node.char.get == prefix.charAt(currentIndex - 1))) {
          if (currentIndex != 0) println(node.char.get.toString + " " + prefix.charAt(currentIndex - 1))
          node.children.get(prefix.charAt(currentIndex).toLower) match {
            case Some(child) => helper(currentIndex + 1, child, f)
          }
        }
      } else if (currentIndex == prefix.length) {
        node.data match {
          case Some(x: A) =>
            found = true
            maxSize = maxSize - 1
            f(x)
          case None =>
            for ((c, n) <- node.children) {
              helper(currentIndex + 1, n, f)
            }
        }
      } else if (currentIndex > prefix.length) {
         println("cur:" + node.char.get.toString)
         node.data match {
          case Some(x:A) =>
            f(x)
            children foreach {
              case (c, n: TrieNode[A]) => helper(currentIndex + 1, n, f)
            }
          case None =>
            for ((c, n) <- node.children) {
              helper(currentIndex + 1, n, f)
            }
        }
      }
    }

    var items = new ListBuffer[A]()
    helper(0, this, x => items.append(x))
    items
  }

  override def contains(word: String): Boolean = {

    @tailrec def helper(currentIndex: Int, node: TrieNode[A]): Boolean = {
      if (currentIndex == word.length) {
        node.word.isDefined
      } else {
        node.children.get(word.charAt(currentIndex).toLower) match {
          case Some(child) => helper(currentIndex + 1, child)
          case None => false
        }
      }
    }

    helper(0, this)
  }

  def pathTo( word : String ) : Option[ListBuffer[TrieNode[A]]] = {

    def helper(buffer : ListBuffer[TrieNode[A]], currentIndex : Int, node : TrieNode[A]) : Option[ListBuffer[TrieNode[A]]] = {
      if ( currentIndex == word.length) {
        node.word.map( word => buffer += node )
      } else {
        node.children.get(word.charAt(currentIndex).toLower) match {
          case Some(found) => {
            buffer += node
            helper(buffer, currentIndex + 1, found)
          }
          case None => None
        }
      }
    }

    helper(new ListBuffer[TrieNode[A]](), 0, this)
  }

  override def toString() : String = s"Trie(char=${char},word=${word})"

}

import collection._

class PrefixMap[T]
  extends mutable.Map[String, T]
    with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get (s(0)) flatMap (_.get(s substring 1))

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T) =
    withPrefix(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) { val prev = value; value = None; prev }
    else suffixes get (s(0)) flatMap (_.remove(s substring 1))

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))

  def += (kv: (String, T)): this.type = { update(kv._1, kv._2); this }

  def -= (s: String): this.type  = { remove(s); this }

  override def empty = new PrefixMap[T]
}

class DictionaryTrie[A] {
  val children: mutable.Map[String, mutable.Map[String, PrefixMap[A]]] = new java.util.TreeMap[String, mutable.Map[String, PrefixMap[A]]]().asScala

  def append(from: String, to: String, key: String, data: A): Unit = {
    this.children.getOrElseUpdate(from, {
      new java.util.TreeMap[String, PrefixMap[A]]().asScala
    }).getOrElseUpdate(to, {
      new PrefixMap[A]()
    }).update(key, data)
  }

  def findByPrefix(from: String, to: String, prefix: String): Iterable[A] = {
    this.children.get(from) match {
      case Some(m) => m.get(to) match {
        case Some(t) => t.withPrefix(prefix).values
        case None => throw DictionaryTrieLanguageToNotFound(to)
      }
      case None => throw DictionaryTrieLanguageFromNotFound(from)
    }
  }

  def findTranslation(from: String, to: String, prefix: String): Option[A] = {
    this.children.get(from) match {
      case Some(m) => m.get(to) match {
        case Some(t) => t.get(prefix)
        case None => throw DictionaryTrieLanguageToNotFound(to)
      }
      case None => throw DictionaryTrieLanguageFromNotFound(from)
    }
  }

  def getAvailableLanguages: Iterable[String] = {
    this.children.keySet.toList
  }
}