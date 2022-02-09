package chapter2

/**
 * 多層関数：型の抽象化
 *
 * 単相関数(monomorphic function): 1つのデータ型だけを操作する関数のこと
 * [[GettingStarted.abs abs(n: Int)]],
 * [[GettingStarted.factorial factorial(n: Int)]] は引数は [[Int]] 型のものに限定されており、
 * 高階関数である`formatResult(name: String, n: Int, f: Int => Int)` もInt型の引数を受け取る関数だった
 * どのような型を渡されても、動作するコードが書きたい場合 -> 多相関数(polymorphic function)
 */
object MultiLayerFunc {}
