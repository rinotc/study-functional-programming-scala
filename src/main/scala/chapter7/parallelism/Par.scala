package chapter7.parallelism
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => UnitFuture[A]

  scala.concurrent.Future

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean                                 = true
    override def get(timeout: Long, unit: TimeUnit): A           = get
    override def isCancelled: Boolean                            = false
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  /**
   * 評価されていない `A` を受け取り、それを別のスレッドで評価するための計算を返す。 `unit`
   * という名前は、1つの値をラッピングするだけという1単位の並列化を実現することを意味する
   *
   * @note 直ちに `A` が得られる計算を作成
   * @note 定数値を並列計算に昇格させる
   */
  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /**
   * @note ２つの並列計算の結果を2項関数で結合
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  /**
   * メインスレッドから計算を分岐する
   *
   * @note [[run]] による並列評価の対象としてマーク。この評価は[[run]]によって
   *       強制されるまで実際には発生しない。
   */
  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(new Callable[A] {
      def call = a(es).get
    })

  /**
   * [[unit]] の非正格バージョン
   *
   * @note 式`a`を`run`による並列計算のためにラッピングし、並列評価の対象としてマークする。
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * 並列化を実装する場所
   *
   * @note 与えられた [[Par]] を完全に評価し、[[fork]] によって要求される並列計算
   *       を生成し、結果の値を取得。
   * @note 実際に計算を行うことで、[[Par]] から値を取得する
   */
  def run[A](s: ExecutorService)(a: Par[A]): A = ???
}

trait Par[A] {}
