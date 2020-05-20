// 階乗
def factorial(n: Int): Int = {
  // 再帰のヘルパー関数, go とか loop という名前をつけるのが慣例
  def go(n: Int, acc: Int): Int = // 残余値 n, 累積階乗 acc
    if (n <= 0) acc
    else go(n-1, n*acc)

  go(n, 1)
}

factorial(10)
