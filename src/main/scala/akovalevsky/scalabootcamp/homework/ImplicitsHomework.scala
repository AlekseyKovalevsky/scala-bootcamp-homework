package akovalevsky.scalabootcamp.homework

import akovalevsky.scalabootcamp.homework.ImplicitsHomework.MyTwitter._
import akovalevsky.scalabootcamp.homework.ImplicitsHomework.SuperVipCollections4s.PackedMultiMap
import akovalevsky.scalabootcamp.homework.ImplicitsHomework.SuperVipCollections4s.syntax.GetSizeScoreOps

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {

  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object GetSizeScore {
      def apply[T: GetSizeScore]: GetSizeScore[T] = implicitly
    }

    object syntax {

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = GetSizeScore[T].apply(inner)
      }

    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {

      import instances._

      //with this you can use .sizeScore syntax on keys and values

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private var map = mutable.LinkedHashMap.empty[K, V]
      private val emptyMapSizeScore = mutable.LinkedHashMap.empty[K, V].sizeScore

      private def sizeScore() = map.sizeScore - emptyMapSizeScore


      @tailrec
      def put(key: K, value: V): Unit = {
        (maxSizeScore - sizeScore, key.sizeScore + value.sizeScore) match {
          case (0, _) => ()
          case (sizeLeft, itemSize) if sizeLeft < itemSize => {
            map = map.tail
            put(key, value)
          }
          case _ => map.addOne(key, value)

        }
      }


      def get(key: K): Option[V] = map.get(key)
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    object Iterate {
      def apply[F[_] : Iterate]: Iterate[F] = implicitly
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]

      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object Iterate2 {
      def apply[F[_, _] : Iterate2]: Iterate2[F] = implicitly
    }

    object instances {

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!

      implicit val iterate2ForMap: Iterate2[collection.Map] = new Iterate2[collection.Map] {
        override def iterator1[K, V](f: collection.Map[K, V]): Iterator[K] = f.keysIterator

        override def iterator2[K, V](f: collection.Map[K, V]): Iterator[V] = f.valuesIterator
      }

      implicit val iterate2ForPackedMultiMap: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.iterator.map(_._1)

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.iterator.map(_._2)
      }

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */

      val byteSize = 1
      val charSize = 2
      val intSize = 4
      val longSize = 8
      val objectHeaderSize = 12

      implicit val sizeScoreForByte: GetSizeScore[Byte] = _ => byteSize
      implicit val sizeScoreForInt: GetSizeScore[Int] = _ => intSize
      implicit val sizeScoreForLong: GetSizeScore[Long] = _ => longSize
      implicit val sizeScoreForChar: GetSizeScore[Char] = _ => charSize
      implicit val sizeScoreForString: GetSizeScore[String] = objectHeaderSize + _.length * charSize

      implicit def sizeScoreForAnyIterable[F[_] : Iterate, A: GetSizeScore]: GetSizeScore[F[A]] =
        (iterable: F[A]) => objectHeaderSize + Iterate[F].iterator(iterable).foldLeft(0)(_ + _.sizeScore)

      implicit def sizeScoreForAnyIterable2[F[_, _] : Iterate2, A: GetSizeScore, B: GetSizeScore]: GetSizeScore[F[A, B]] = {
        (iterable2: F[A, B]) =>
          objectHeaderSize +
            Iterate2[F].iterator1(iterable2).foldLeft(0)(_ + _.sizeScore) +
            Iterate2[F].iterator2(iterable2).foldLeft(0)(_ + _.sizeScore)

      }
    }

  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    import SuperVipCollections4s._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit

      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {

      import akovalevsky.scalabootcamp.homework.ImplicitsHomework.SuperVipCollections4s.instances._;

      implicit def sizeScoreForFbiNote: GetSizeScore[FbiNote] = fbiNote =>
        fbiNote.month.sizeScore + fbiNote.favouriteChar.sizeScore + fbiNote.watchedPewDiePieTimes.sizeScore

      implicit def sizeScoreForTwit: GetSizeScore[Twit] = twit =>
        twit.id.sizeScore + twit.userId.sizeScore + twit.hashTags.sizeScore + twit.attributes.sizeScore + twit.fbiNotes.sizeScore

      private val boundedCache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = boundedCache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = boundedCache.get(id)
    }
  }

  def main(args: Array[String]): Unit = {
    val twitCache = createTwitCache(400)
    val twit1 = new Twit(
      1001,
      100,
      Vector[String]("Hello", "World"),
      PackedMultiMap(("Hello", "World")),
      List(FbiNote("September", '$', 100000))
    )
    val twit2 = new Twit(
      1002,
      100,
      Vector[String]("Hello", "World"),
      PackedMultiMap(("Hello", "World")),
      List(FbiNote("September", '$', 100000))
    )
    val twit3 = new Twit(
      1003,
      100,
      Vector[String]("Hello", "World"),
      PackedMultiMap(("Hello", "World")),
      List(FbiNote("September", '$', 100000))
    )

    twitCache.put(twit1)
    twitCache.put(twit2)
    twitCache.put(twit3)
  }

}
