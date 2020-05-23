//
// 2.6 型に従う実装
//

// 部分適用(partial application)と呼ばれるものを実行するための高階関数
def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b) // B型のb値を受け取る関数を返す, C型を返すためには, 引数 f に A と B を渡す.

// A, B, C 3つの型パラメータが定義されている
// この関数は、引数を2つ(a, f)を受け取る
// 関数引数 f は型 A, B の二つを受け取る関数であり, C型の値を返す.
// partial1から返される値も, B => C 型の関数

// 実際に使ってみる.
// partial1は引数 b: Double をとる関数を返す.
// 最後に引数 b: Double に当たる 1.4 を渡している.
partial1[Int, Double, String](10, (x, y) => s"$x + $y = ${x + y}です")(1.4)

// EXERCISE 2.3
// カリー化では, 引数2つの関数 f が, f を部分適用する引数1つの関数に変換される.
// この場合もコンパイルできる実装は1つだけである. これを記述する
def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

curry[Int, Double, String]((x, y) => s"$x * $y = ${x * y} です")(20)(5.5)

// EXERCISE 2.4
// curry による変換の逆変換 uncurry を実装する.
def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

uncurry[Int, Double, String](x => y => s"$x - $y = ${x - y} です.")(123, 22.4)

// EXERCISE 2.5
// 2つの関数を合成する
def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

compose[Int, Int, Int](x => x + 1, y => y * y)(11)

// 関数合成はよく使われる手法のため, Scalaの標準ライブラリでは, compose が Function1 のメソッドとして定義されている.
// f compose g <=> g andThen f
val f = (x: Double) => math.Pi / 2 - x
val cos = f andThen math.sin
cos(90)
