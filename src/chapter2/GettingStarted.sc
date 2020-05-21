object MyModule { // このようなオブジェクトをモジュールと呼ぶ
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}

// MyModule が名前空間
MyModule.main(Array("hoge"))
MyModule.abs(-33)

// オブジェクトメンバーにアクセスする場合は「.」とメンバーの名前を指定する

// Scalaには演算子の概念はない
// 2 + 1 の「+」はオブジェクト2のメンバ「+」である
// 2 + 1 <=> 2.+(1)
2 + 1
2.+(1)

// どのようなメソッド名も、「単一の引数」で呼び出す場合は、MyModule abs -33 のようにして同じ結果になる
MyModule abs -33


//
// 2.4 高階関数: 関数に関数を渡す
//

// 階乗, 関数型ループの例
def factorial(n: Int): Int = {
  // 再帰のヘルパー関数, go とか loop という名前をつけるのが慣例
  @annotation.tailrec // 末尾呼び出しでないメソッドの場合にコンパイルエラーにするアノテーション
  def go(n: Int, acc: Int): Int = // 残余値 n, 累積階乗 acc
    if (n <= 0) acc
    else go(n-1, n*acc) // 例. 1 + go(n-1, n*acc) は末尾呼び出しではない

  go(n, 1)
}

factorial(10)

// メソッドが末尾呼び出しであるとは、呼び出し元が再帰呼び出しの値を返す以外に何もしないことを意味する
// 関数が実行する再起呼び出しが全て末尾呼び出しである場合、再起は繰り返しのたびにコールスタックフレームを
// を消費しないループとして、自動的にコンパイルされる。

// コールスタック (Call Stack)は、プログラムで実行中のサブルーチンに関する情報を格納するスタックである。実行中のサブルーチンとは、呼び出されたが処理を完了していないサブルーチンを意味する。
// https://www.wikiwand.com/ja/%E3%82%B3%E3%83%BC%E3%83%AB%E3%82%B9%E3%82%BF%E3%83%83%E3%82%AF

// 繰り返しのたびに、コールスタックフレームを消費すると、入力が大きい場合に StackOverflowError が発生する


// EXERCISE 2.1
// フィボナッチ数
// 末尾再帰で書く書き方分からなかった
// https://qiita.com/takuya0301/items/39121e0988750878e0f7
def fib(n: Int): Long = {
  @annotation.tailrec
  def go(n: Int, a: Long, b: Long): Long =
    if (n == 0) a
    else go(n-1, b, a+b)

  go(n, 0, 1)
}

// 末尾再帰じゃない書き方だと、fib(100)なんて計算できないが、末尾再帰なら一瞬
fib(100)

object MyModule2 { // このようなオブジェクトをモジュールと呼ぶ
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }
  // formatAbs と formatFactorial メソッドはぼぼ同じ => formatResultにまとめよう！
  def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // 上のformatAbs と formatFactorialをまとめた高階関数
  def formatResult(name: String, n: Int, f: Int => Int) = { // 引数 f は Intを引数にとり、Intを返す関数
    val msg = "The %sk of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}

MyModule2.main(Array(""))