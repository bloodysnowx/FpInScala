import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop._

object MainSpecification extends Properties("Stream") {
  import Stream._

  implicit def arbStream[T](implicit a: Arbitrary[T]): Arbitrary[Stream[T]] = Arbitrary {
    Arbitrary.arbitrary[List[T]].map(ls => Stream.apply(ls: _*))
  }

  property("toList") = forAll { l: List[Int] =>
    Stream.apply(l:_*).toList == l
  }

  property("headOption") = forAll { l: Stream[Int] =>
    l.headOption == l.toList.headOption
  }

  property("toListTailrec") = forAll { l: List[Int] =>
    Stream.apply(l:_*).toListTailrec == l
  }

  property("take") = forAll { (l: Stream[Int], i: Int) =>
    l.take(i).toList == l.toList.take(i)
  }

  property("drop") = forAll { (l: Stream[Int], i: Int) =>
    l.drop(i).toList == l.toList.drop(i)
  }

  def createP(i: Int) = {
    val p = (x: Int) => x > i
    val q = (x: Int) => x < i
    val r = (x: Int) => if(i != 0) x % i == 0 else false
    Seq(p, q, r)
  }

  property("takeWhile") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.takeWhile(p).toList == l.toList.takeWhile(p))
  }

  property("exists") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.exists(p) == l.toList.exists(p))
  }

  property("foldRight") = forAll { (l: Stream[Int], i: Int) =>
    l.foldRight(i)((a, b) => a + b) == l.toList.foldRight(i)((a, b) => a + b) &&
      l.foldRight(i)((a, b) => a * b) == l.toList.foldRight(i)((a, b) => a * b)
  }

  property("exists2") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.exists2(p) == l.toList.exists(p))
  }

  property("forAll") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.forAll(p) == l.toList.forall(p))
  }

  property("takeWhile2") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.takeWhile2(p).toList == l.toList.takeWhile(p))
  }

  property("headOption2") = forAll { l: Stream[Int] =>
    l.headOption2 == l.toList.headOption
  }

  property("map") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.map(p).toList == l.toList.map(p))
  }

  property("filter") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.filter(p).toList == l.toList.filter(p))
  }

  property("append") = forAll { (l: Stream[Int], m: Stream[Int]) =>
    l.append(m).toList == l.toList ++ m.toList
  }

  property("flatMap") = forAll { (l: Stream[Int], a: Int, b: Int) =>
    val f = (x: Int) => Stream.apply(x, x * a, x * b)
    l.flatMap(f).toList == l.toList.flatMap(f andThen (_.toList))
  }

  property("find") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.find(p) == l.toList.find(p))
  }

  type SmallInt = Int
  implicit val smallIntArbitrary: Arbitrary[SmallInt] = Arbitrary(Gen.choose(0, 100))

  property("constant") = forAll { (a: Int, b: SmallInt) =>
    constant(a).take(b).forAll(_ == a)
  }

  property("from") = forAll { (a: Int, b: SmallInt) =>
    from(a).take(b).toList == Range(a, a + b).toList
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n == 0) a
      else go(n - 1, b, a + b)

    go(n, 0, 1)
  }

  property("fibs") = forAll { n: SmallInt =>
    fibs().drop(n).headOption.get == fib(n)
  }

  property("unfold") = forAll { (a: SmallInt, b: SmallInt, n: SmallInt) =>
    unfold(a)(s => Some((s, s + b))).drop(n).headOption.get == a + b * n
  }

  property("unfold_ones") = forAll { (a: SmallInt, b: SmallInt, n: SmallInt) =>
    ones.take(n).toList == unfold(a)(_ => Some((1, a))).take(n).toList
  }

  property("unfold_constant") = forAll { (a: SmallInt, b: SmallInt, n: SmallInt) =>
    constant(a).take(n).toList == unfold(b)(_ => Some(a, b)).take(n).toList
  }

  property("unfold_from") = forAll { (a: SmallInt, b: SmallInt, n: SmallInt) =>
    from(a).take(n).toList == unfold(a)(x => Some((x, x + 1))).take(n).toList
  }

  property("unfold_fibs") = forAll { (a: SmallInt, b: SmallInt, n: SmallInt) =>
    fibs().take(n).toList == unfold((0, 1))({ case (x, y) => Some(x, (y, x + y)) }).take(n).toList
  }

  property("map2") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.map2(p).toList == l.toList.map(p))
  }

  property("take2") = forAll { (l: Stream[Int], i: Int) =>
    l.take2(i).toList == l.toList.take(i)
  }

  property("takeWhile3") = forAll { (l: Stream[Int], i: Int) =>
    createP(i).forall(p => l.takeWhile3(p).toList == l.toList.takeWhile(p))
  }

  property("zipWith") = forAll { (l: Stream[Int], m: Stream[Int]) =>
    l.zipWith(m).toList == l.toList.zip(m.toList)
  }

  property("zipAll") = forAll { (l: Stream[Int], m: Stream[Int]) =>
    l.zipAll(m).toList == l.toList.map(Some.apply).zipAll(m.toList.map(Some.apply), None, None)
  }

  property("startsWith") = forAll { (l: Stream[Int], m: Stream[Int], i: Int) =>
    l.startsWith(l) == l.toList.startsWith(l.toList) &&
      l.startsWith(m) == l.toList.startsWith(m.toList) &&
      m.startsWith(l) == m.toList.startsWith(l.toList) &&
      m.startsWith(m) == m.toList.startsWith(m.toList) &&
      l.startsWith(l.take(i)) == true &&
      m.startsWith(m.take(i)) == true
  }

  property("tails") = forAll { l: Stream[Int] =>
    l.tails.map(_.toList).toList == l.toList.tails.toList
  }

  property("scanRight") = forAll { l: Stream[Int] =>
    l.scanRight(0)(_ + _).toList == l.toList.scanRight(0)(_ + _)
  }

  property("scanRight2") = forAll { l: Stream[Int] =>
    l.scanRight2(0)(_ + _).toList == l.toList.scanRight(0)(_ + _)
  }
}
