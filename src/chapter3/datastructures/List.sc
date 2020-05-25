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
}

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
