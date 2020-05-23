// Scala標準ライブラリで定義されているListの少し単純化したもの

// 型パラメータの + 記号は共変(covariant)であることを示す. Dog が Animal の部分型であれば, List[Dog] は List[Animal] の部分型.
sealed trait List[+A] // データ型の定義. Aという型でパラメータ化されたListデータ型. 一般的にデータ型は trait を使って定義する. sealed はこのトレイトの実装が全てこのファイルで宣言されなければならないことを示す.
// データコンストラクタ. case 句を使って定義されている.
case object Nil extends List[Nothing] // 空のリストを表すデータコンストラクタ. Nil オブジェクトは空のリストを定義する.
case class Cons[+A](head: A, tail: List[A]) extends List[A] // 空でないリストを表すデータコンストラクタ

// ※コンパニオンオブジェクト
// 同じファイルで同じネストレベルにあるクラスと同名のオブジェクトを定義したとき, そのオブジェクトを指して,
// 「そのクラスのコンパニオンオブジェクトである」という.
// オブジェクトのファクトリとして使われたり, 暗黙パラメータのデフォルトのインスタンスを格納するのに使われたりする.

object List { // List のコンパニオンオブジェクト.
  def sum(ints: List[Int]): Int = ints match { // 整数のリストを足し合わせる関数
    case Nil => 0 // 空のリストの合計は0
    case Cons(x, xs) => x + sum(xs) // x から始まるリストの合計は, xにリストの残りの合計を足したものになる.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val x = List(1, 2, 3, 4) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}