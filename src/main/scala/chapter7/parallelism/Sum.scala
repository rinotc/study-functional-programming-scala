package chapter7.parallelism

object Sum {

  /**
   * 逐次的な畳み込みではなく、分割統治アルゴリズムを使ってリストを合計する方法
   *
   * この方式は並列化が可能。
   *
   * @param ints
   *   [[IndexedSeq]] は 標準ライブラリ Vector
   *   などのランダムアクセスシーケンスのスーパークラス。特定のインデックスでシーケンスを二つに分割する
   *   [[IndexedSeq#splitAt splitAt]] が用意されている
   */
  def sum1(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum1(l) + sum1(r)
    }
  }

  /**
   * カスタムデータ型を使った `sum`
   */
  def sum2(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL   = Par.unit(sum2(l))
      val sumR   = Par.unit(sum2(r))
      Par.run(sumL) + Par.run(sumR)
    }
  }

  /**
   * 並列計算の結合
   */
  def sum3(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum3(l), sum3(r))(_ + _)
    }
  }

  /**
   * [[sum3]] まででは、計算をメインスレッドからフォーク（分岐）させるタイミングが
   * 非常に曖昧。フォークを明示的にしてみる。[[Par.fork]] という関数を新たに作成
   */
  def sum4(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum4(l)), Par.fork(sum4(r)))(_ + _)
    }
  }
}
