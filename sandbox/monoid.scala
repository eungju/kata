case class Max[T](val v: T)(implicit ordering:Ordering[T]) {
    def +(that: Max[T]) = if (ordering.compare(this.v, that.v) == 1) this else that
}

println((Max(10) + Max(30) + Max(20)))
