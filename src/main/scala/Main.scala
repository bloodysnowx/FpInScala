import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Main extends App {
  import Stream._

  val a = Stream.apply(1, 2, 3)
  println(a.toList)

  var b = ArrayBuffer.empty[Int]
  for(x <- List(1, 2, 3, 4)) {
    val y = x + 10
    if(y % 2 == 0) {
      b.append(y * 3)
    }
  }

  for {
    x <- List(1, 2, 3, 4)
    y = x + 10
    if y % 2 == 0
  } yield
    y * 3

  List(1, 2, 3, 4).collect {
    case x if (x + 10) % 2 == 0 => (x + 10) * 3
  }

  println(ones.take(5).toList)
  println(ones.exists(_ % 2 != 0))
  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(ones.takeWhile(_ == 1))
  println(ones.forAll(_ != 1))

  println(unfold(1)(s => Some((s + 2, s + 2))).take(5).toList)
}

trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Cons(a, tail) => a() +: tail().toList
    case Empty => Nil
  }

  def toListTailrec: List[A] = {
    @tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(a, tail) => go(tail(), a() +: l)
      case Empty => l
    }

    go(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case _ if n <= 0 => Empty
    case Cons(a, tail) => cons[A](a(), tail().take(n - 1))
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(a, tail) if n > 0 => tail().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(a, tail) if p(a()) => cons(a(), tail().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)(
    (a, b) => if(p(a)) cons(a, b) else Empty
  )

  def headOption2: Option[A] = foldRight[Option[A]](None)(
    (a, b) => Some(a)
  )

  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty)((a, b) =>
    cons(f(a), b)
  )

  def filter(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, b) =>
    if(p(a)) cons(a, b) else b
  )

  def append[B >: A](other: Stream[B]): Stream[B] = foldRight[Stream[B]](other)((a, b) =>
    cons(a, b)
  )

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((a, b) =>
    f(a).append(b)
  )

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def take2(n: Int): Stream[A] = unfold((this, n)) {
    case (Empty, _) => None
    case (_, n) if n <= 0 => None
    case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
  }

  def takeWhile3(p: A => Boolean): Stream[A] = ???

  def zipWith[B](other: Stream[B]): Stream[(A, B)] = ???

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = ???

  def startsWith[A](s: Stream[A]): Boolean = ???

  def tails: Stream[Stream[A]] = ???

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(m: Int = 0, n: Int = 1): Stream[Int] = cons(m, fibs(n, m + n))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}