package Chainsaw.Architectures

trait TreeBuilder[T] {

  val input: Seq[T]
  val depth: Int
  val router: (Seq[T], Int) => Seq[T]
  val operator: (Seq[T], Int) => Seq[T]

  def implement(): Seq[T] = {
    def build(input: Seq[T], currentDepth: Int): Seq[T] = {
      if (currentDepth == depth) operator(input, currentDepth)
      else build(router(operator(input, currentDepth), currentDepth), currentDepth + 1)
    }

    build(input, 0)
  }
}
