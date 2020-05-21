//
// 多層関数：型の抽象化
//

// 単相関数(monomorphic function): 1つのデータ型だけを操作する関数のこと
// abs(x: Int), factorial(n: Int) は引数はInt型のものに限定されており、
// 高階関数であるformatResult(name: String, n: Int, f: Int => Int) もInt型の引数を受け取る関数だった
// どのような型を渡されても、動作するコードが書きたい場合 -> 多相関数(polymorphic function)

// 単相関数から多相関数へ

// 単相関数の例
def findFirst(ss: Array[String], key: String): Int = {
  @annotation.tailrec
  def loop(n: Int): Int =
    if (n >= ss.length) -1
    else if (ss(n) == key) n
    else loop(n + 1)

  loop(0)
}

// 多層関数にする
def findFirst[A](as: Array[A], p: A => Boolean): Int = { // String をハードコーディングにする代わりに、型Aをパラメータとして受け取れるようにする
  // 配列の各要素を評価する関数 p を受け取るようにする
  @annotation.tailrec
  def loop(n: Int): Int =
    if (n >= as.length) -1
    else if (p(as(n))) n
    else loop(n + 1)

  loop(0)
}

// EXERCISE 2.2
// 指定された比較関数に従って Array[A] がソートされているかどうか調べる isSorted を実装する

// 単相関数で考えてみる
def isSortedInt(as: Array[Int], ordered: (Int, Int) => Boolean) = {
  @annotation.tailrec
  def loop(n: Int): Boolean =
    if (n == 0) true
    else ordered(as(n-1), as(n)) && loop(n-1)

  loop(as.length-1)
}

isSortedInt(Array(1, 2, 3), (a: Int, b: Int) => a <= b)


def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) = {
  @annotation.tailrec
  def loop(n: Int): Boolean =
    if (n == 0) true
    else ordered(as(n-1), as(n)) && loop(n-1)

  loop(as.length - 1)
}

isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b)
isSorted(Array(1.3, 1.5, 1.1), (a: Double, b: Double) => a > b)