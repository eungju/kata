sealed trait Book
case object First extends Book
case object Second extends Book
case object Third extends Book
case object Forth extends Book
case object Fifth extends Book

case class Bundle(books: Set[Book]) {
  val UNIT_PRICE = 8
  val DISCOUNT_RATES = Array(0, 0, 0.05, 0.1, 0.2, 0.25)
  def contains(book: Book): Boolean = books.contains(book)
  def add(book: Book): Bundle = Bundle(books + book)
  val price: Double = books.size * UNIT_PRICE * (1 - DISCOUNT_RATES(books.size))
}

case class Order(bundles: List[Bundle]) {
  val price: Double = bundles.foldLeft(0.0) { _ + _.price }
  def pack(book: Book): List[Order] = {
    bundles.filter(!_.contains(book)).map((bundle) => {
        val (left, right) = bundles.span(_ != bundle)
        Order(left ++ right.drop(1) :+ bundle.add(book))
      }) :+ Order(bundles :+ Bundle(Set(book)))
  }
  def packAll(books: List[Book]): List[Order] = {
    books.foldLeft(List(this)) { (orders, book) => orders.flatMap { _.pack(book) } }
  }
}

case object Order {
  def price(books: List[Book]): Double = Order(List()).packAll(books).map(_.price).min
}

object HarryPotter {
  def main(args: Array[String]) = {
    val bundle = Bundle(Set(First, Second))
    val order = Order(List(Bundle(Set(First))))

    assert(Bundle(Set(First)).contains(First))
    assert(!Bundle(Set(First)).contains(Second))

    assert(Bundle(Set(First)).price == 8)
    assert(Bundle(Set(First, Second)).price == 8 * 2 * 0.95)
    assert(Bundle(Set(First, Second, Third)).price == 8 * 3 * 0.90)
    assert(Bundle(Set(First, Second, Third, Forth)).price == 8 * 4 * 0.80)
    assert(Bundle(Set(First, Second, Third, Forth, Fifth)).price == 8 * 5 * 0.75)

    assert(Order(List(Bundle(Set(First)), Bundle(Set(First)))).price == 8.0 * 2)

    assert(Order(List()).pack(First) == List(Order(List(Bundle(Set(First))))))
    assert(Order(List(Bundle(Set(First)))).pack(First) == List(Order(List(Bundle(Set(First)), Bundle(Set(First))))))
    assert(Order(List(Bundle(Set(First)))).pack(Second) == List(Order(List(Bundle(Set(First, Second)))),
                                                                Order(List(Bundle(Set(First)), Bundle(Set(Second))))))
    assert(Order(List(Bundle(Set(First)), Bundle(Set(Second)))).pack(First) == List(Order(List(Bundle(Set(First)), Bundle(Set(First, Second)))),
                                                                                    Order(List(Bundle(Set(First)), Bundle(Set(Second)), Bundle(Set(First))))))

    assert(Order(List()).packAll(List(First, Second)) == List(Order(List(Bundle(Set(First, Second)))),
                                                              Order(List(Bundle(Set(First)), Bundle(Set(Second))))))

    //example
    assert(Order.price(List(First, First, Second, Second, Third, Third, Forth, Fifth)) == 51.20)

    //several discounts
    assert(Order.price(List(First, First, Second)) == 8 + (8 * 2 * 0.95))
    assert(Order.price(List(First, First, Second, Second)) == 2 * (8 * 2 * 0.95))
    assert(Order.price(List(First, First, Second, Third, Third, Forth)) == (8 * 4 * 0.8) + (8 * 2 * 0.95))
    assert(Order.price(List(First, Second, Second, Third, Forth, Fifth)) == 8 + (8 * 5 * 0.75))

    //edge cases
    assert(Order.price(List(First, First, Second, Second, Third, Third, Forth, Fifth)) == 2 * (8 * 4 * 0.8))
    if (false) {
    assert(Order.price(List(First, First, First, First, First,
                            Second, Second, Second, Second, Second,
                            Third, Third, Third, Third,
                            Forth, Forth, Forth, Forth, Forth,
                            Fifth, Fifth, Fifth, Fifth)) == 3 * (8 * 5 * 0.75) + 2 * (8 * 4 * 0.8))
    }
  }
}
