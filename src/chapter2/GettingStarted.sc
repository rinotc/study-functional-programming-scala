// 階乗
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
