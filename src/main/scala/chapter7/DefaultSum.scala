package chapter7

object DefaultSum {

  /**
   * 整数リストの合計。通常の左畳み込みの実装。
   */
  // noinspection SimplifiableFoldOrReduce
  def sum(ints: Seq[Int]): Int = ints.foldLeft(0) { (a, b) => a + b }
}
