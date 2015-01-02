package harrypotter

sealed trait Book
case object First extends Book
case object Second extends Book
case object Third extends Book
case object Forth extends Book
case object Fifth extends Book

case class Bundle(books: Set[Book]) {
  def contains(book: Book): Boolean = books.contains(book)
  val size = books.size
  def add(book: Book): Bundle = Bundle(books + book)
  val discountRate = Array(0, 0, 0.05, 0.1, 0.2, 0.25)(books.size)
  val price: Double = books.size * 8 * (1 - discountRate)
}

case object Bundle {
  def apply(books: Book*): Bundle = Bundle(books.toSet)
}

case class Bundling(bundles: List[Bundle]) {
  val price: Double = bundles.foldLeft(0.0) { _ + _.price }
  def pack(book: Book): Set[Bundling] = {
    bundles.toSet.filter(!_.contains(book)).map((bundle) => {
      val (left, right) = bundles.span(_ != bundle)
      Bundling(left ++ right.drop(1) :+ bundle.add(book))
    }) + Bundling(bundles :+ Bundle(Set(book)))
  }
  def packAll(books: Book*): Set[Bundling] = {
    books.foldLeft(Set(this)) { (orders, book) =>
      val ub = orders.map(_.price).min + Bundle(book).price
      orders.flatMap(_.pack(book).filter(ub >= _.price))
    }
  }
}

case object Bundling {
  def apply(bundles: Bundle*): Bundling = Bundling(bundles.toList)
}

case class Order(books: Book*) {
  def price(): Double = Bundling().packAll(books:_*).map(_.price).min
}

object HarryPotter {
  def main(args: Array[String]) = {
    val bundle = Bundle(First, Second)
    val bundling = Bundling(Bundle(First))

    assert(Bundle(First).contains(First))
    assert(!Bundle(First).contains(Second))

    assert(Bundle(First).price == 8)
    assert(Bundle(First, Second).price == 8 * 2 * 0.95)
    assert(Bundle(First, Second, Third).price == 8 * 3 * 0.90)
    assert(Bundle(First, Second, Third, Forth).price == 8 * 4 * 0.80)
    assert(Bundle(First, Second, Third, Forth, Fifth).price == 8 * 5 * 0.75)

    assert(Bundling(Bundle(First), Bundle(First)).price == 8.0 * 2)

    assert(Bundling().pack(First) == Set(Bundling(Bundle(First))))
    assert(Bundling(Bundle(First)).pack(First) == Set(Bundling(Bundle(First), Bundle(First))))
    assert(Bundling(Bundle(First)).pack(Second) == Set(Bundling(Bundle(First, Second)),
                                                       Bundling(Bundle(First), Bundle(Second))))
    assert(Bundling(Bundle(First), Bundle(First)).pack(Second) == Set(Bundling(Bundle(First), Bundle(First, Second)),
                                                                      Bundling(Bundle(First), Bundle(First), Bundle(Second))))

    assert(Bundling().packAll(First, Second) == Set(Bundling(Bundle(First, Second)),
                                                    Bundling(Bundle(First), Bundle(Second))))

    //example
    assert(Order(First, First, Second, Second, Third, Third, Forth, Fifth).price == 51.20)

    //several discounts
    assert(Order(First, First, Second).price == 8 + (8 * 2 * 0.95))
    assert(Order(First, First, Second, Second).price == 2 * (8 * 2 * 0.95))
    assert(Order(First, First, Second, Third, Third, Forth).price == (8 * 4 * 0.8) + (8 * 2 * 0.95))
    assert(Order(First, Second, Second, Third, Forth, Fifth).price == 8 + (8 * 5 * 0.75))

    //edge cases
    assert(Order(First, First, Second, Second, Third, Third, Forth, Fifth).price == 2 * (8 * 4 * 0.8))
    assert(Order(First, First, First, First, First,
                 Second, Second, Second, Second, Second,
                 Third, Third, Third, Third,
                 Forth, Forth, Forth, Forth, Forth,
                 Fifth, Fifth, Fifth, Fifth).price == 3 * (8 * 5 * 0.75) + 2 * (8 * 4 * 0.8))
  }
}
