// Scala標準ライブラリで定義されているListの少し単純化したもの

// 型パラメータの + 記号は共変(covariant)であることを示す. Dog が Animal の部分型であれば, List[Dog] は List[Animal] の部分型.
sealed trait List[+A] // データ型の定義. Aという型でパラメータ化されたListデータ型. 一般的にデータ型は trait を使って定義する. sealed はこのトレイトの実装が全てこのファイルで宣言されなければならないことを示す.
// データコンストラクタ. case 句を使って定義されている.
case object Nil extends List[Nothing] // 空のリストを表すデータコンストラクタ. Nil オブジェクトは空のリストを定義する.
case class Cons[+A](head: A, tail: List[A]) extends List[A] // 空でないリストを表すデータコンストラクタ

//
// 変位について
//
// trail List[+A] 宣言において, 型パラメータAの前にある+は変位アノテーション(variance annotation)であり,
// Aが共変パラメータであることを指定する
// Dog in Animal => List[Dog] in List[Animal]
// +を省略した場合はListはその型パラメータに対して不変(invariant)になる.
// Nil は List[Nothing]を拡張している. Nothing は全ての型の部分型であるので, 変位アノテーションの組み合わせにより,
// Nil を List[Int] や List[Double] とみなせることになる.

// ※コンパニオンオブジェクト
// 同じファイルで同じネストレベルにあるクラスと同名のオブジェクトを定義したとき, そのオブジェクトを指して,
// 「そのクラスのコンパニオンオブジェクトである」という.
// オブジェクトのファクトリとして使われたり, 暗黙パラメータのデフォルトのインスタンスを格納するのに使われたりする.

object List { // List のコンパニオンオブジェクト.

  // sum も product も再帰定義. 再帰データ型を操作するための関数を記述するときによく使われる.
  def sum(ints: List[Int]): Int = ints match { // 整数のリストを足し合わせる関数
    case Nil => 0 // 空のリストの合計は0
    case Cons(x, xs) => x + sum(xs) // x から始まるリストの合計は, xにリストの残りの合計を足したものになる.
  }

  def product(ds: List[Double]): Double = ds match {  // ds を被検査体 or ターゲットと呼ばれる.
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // sumとproductは扱っている型がList[Int]とList[Double]という違いを除けば、リストが空である場合に返す値と, 結果を結合
  // するための演算が異なるだけ
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double])=
    foldRight(ns, 1.0)(_ * _)

  // 引数が可変長個の関数.
  // A型の引数を0個以上受け取る.
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // EXERCISE 3.2
  // Listの最初の要素を削除するtail関数を実装する.
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  // EXERCISE 3.3
  // Listの最初の要素を別の値と置き換えるsetHead関数を実装する.
  def setHead[A](list: List[A], h: A): List[A] = list match {
    case Nil => sys.error("sehHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  // EXERCISE 3.4
  // リストの先頭からn個の要素を削除するdrop関数
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  // EXERCISE 3.5
  // 述語とマッチする場合に限り, Listからその要素までを削除するdropWhile
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  // 上記の実装だと, fに無名関数を使った呼び出しの場合, その引数の型を指定する必要がある
  // val e = List.dropWhile(list, (x: Int) => x <= 3)
  // xの型がIntであることを示すのは冗長
  // dropWhileの引数リストを二つのグループにまとめると, このことをScalaが推測できるようになる(カリー化する)

  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => as
    }
  }

  // リスト 3-2
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // EXERCISE 3.6
  // Listの末尾をのぞく全ての要素で構成されたListを返す init 関数を実装する
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // EXERCISE 3.9
  // foldRightを使ってリストの長さを計算する
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  // EXERCISE 3.10
  // 上記のfoldRightの実装は末尾再帰ではなく, リストが大きい場合はStackOverflowErrorになる
  // これをスタックセーフでないという. リスト再帰の総称関数 foldLeft を記述する
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // EXERCISE 3.11
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, h) => acc + 1)

  // EXERCISE 3.12
  // 要素が逆に並んだリストを返す関数
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))
}

val list: List[Int] = List(1, 2, 3, 4, 5)
List.setHead(list, 9)
List.tail(list)
List.drop(list, 3)
List.dropWhile(list, (x: Int) => x <= 3)
List.dropWhile2(list)(x => x <= 3)

val list2: List[Int] = List(7, 8, 9)
List.append(list, list2)
List.init(list)
List.sum2(list)
List.product2(List(3.2, 1.1, 4.4, 2.431))



val x = List(1, 2, 3, 4) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))


// EXERCISE 3.7
// foldRightを使って実装されたproductは 0.0 を検出した場合に, 直ちに再帰を中止して, 0.0を返せるか？
// 大きなListでfoldRightを呼び出した場合の短絡の仕組みについて検討せよ.
// ------------
/*
No, this is not possible! The reason is because _before_ we ever call our function,
`f`, we evaluate its argument, which in the case of `foldRight` means traversing the list
all the way to the end. We need _non-strict_ evaluation to support early termination---we discuss this in chapter 5.

いいえ、そんなことはできません。
理由は、関数 `f` を呼び出す前に、その引数を評価するからです。
早期終了をサポートするためには、非厳格な評価が必要です。
 */

/*
EXERCISE 3.8 List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))のように, Nil 及び Cons 自体をfoldRightに
渡した場合はどうなるのか. これが foldRight とList のデータコンストラクタ都音関係について何を表していると思うか

We get back the original list! Why is that? As we mentioned earlier,
one way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor of the list with the `z` argument,
and it replaces the `Cons` constructor with the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`,
then we get back the input list.

元のリストを取り戻す！これはなぜでしょうか? 先ほども述べたように、`foldRight`が「何をするか」を考える一つの方法は、
リストのコンストラクタ `Nil` を `z` 引数に置き換え、コンストラクタ `Cons` を与えられた関数 `f` に置き換えることです。
z` には `Nil` を、`f` には `Cons` を与えるだけで、入力リストが得られます。

foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
Cons(1, Cons(2, Cons(3, Nil)))

 */

List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

List.foldLeft(List(1, 2, 3), 0)(_ + _)
List.sum3(List(1, 2, 3))
List.product3(List(1.1, 1.2, 1.3))
List.reverse(List(1, 2, 3))
