import org.scalatest.{FunSuiteLike, Matchers}

class MatchTest extends FunSuiteLike with Matchers {
  import Helper._
  import OrderBook._
  import Transaction._

  test("parse client") {
    val line = "C1\t1000\t90\t10\t2760\t32"
    val client = Client(
      "C1",
      1000,
      Map(Stock("A") -> 90L,
          Stock("B") -> 10L,
          Stock("C") -> 2760L,
          Stock("D") -> 32L)
    )

    parseClient(line) shouldEqual client
  }

  test("parse order") {
    val line = "C8\ts\tAPPL\t14\t88"
    val order = Order("C8", Sell, Stock("APPL"), 14, 88)
    parseOrder(line).copy(timestamp = 0) shouldEqual order.copy(timestamp = 0)
  }

  val clientHolder = ClientHolder(
    Map(
      "C1" -> Client("C1",
                     100,
                     Map(Stock("A") -> 100,
                         Stock("B") -> 0,
                         Stock("C") -> 50,
                         Stock("D") -> 30)),
      "C2" -> Client("C2",
                     200,
                     Map(Stock("A") -> 0,
                         Stock("B") -> 100,
                         Stock("C") -> 50,
                         Stock("D") -> 30)),
      "C3" -> Client(
        "C3",
        1000,
        Map(Stock("A") -> 0, Stock("B") -> 0, Stock("C") -> 0, Stock("D") -> 0))
    )
  )

  val o1 = Order("C3", Buy, Stock("A"), 10, 10)
  val o2 = Order("C2", Sell, Stock("A"), 10, 15)
  val o3 = Order("C1", Sell, Stock("A"), 10, 10)
  val o4 = Order("C3", Buy, Stock("B"), 15, 15)
  val o5 = Order("C2", Sell, Stock("B"), 15, 15)
  val o6 = Order("C1", Sell, Stock("B"), 10, 10)
  val o7 = Order("C3", Buy, Stock("B"), 15, 15)
  val o8 = Order("C3", Buy, Stock("B"), 15, 15)
  val o9 = Order("C3", Sell, Stock("B"), 15, 15)
  val orders = List(o1, o2, o3, o4, o5, o6, o7, o8, o9)

  val freshOrder = OrderBook(List.empty[Order], List.empty[Transaction])
  val orderBook: OrderBook = processOrders(orders.toIterator, freshOrder)
  val transactions: List[Transaction] = orderBook.transactions.reverse

  test("put orders") {
    val ret =
      orders.foldLeft(freshOrder) { (book, order) =>
        put(order).runS(book).value
      }
    ret.orders shouldEqual orders.reverse
  }

  test("gen transactions") {

    transactions.length shouldEqual 2
    transactions.head shouldEqual Transaction(
      List(SellOperation("C1", 10, 10, Stock("A")),
           BuyOperation("C3", 10, 10, Stock("A"))))
    transactions.tail.head shouldEqual Transaction(
      List(SellOperation("C2", 15, 15, Stock("B")),
           BuyOperation("C3", 15, 15, Stock("B"))))
  }

  test("process transactions") {
    val fClientHolder = processTransactions(orderBook, clientHolder)
    fClientHolder shouldEqual ClientHolder(
      Map(
        "C1" -> Client("C1",
                       200,
                       Map(Stock("A") -> 90,
                           Stock("B") -> 0,
                           Stock("C") -> 50,
                           Stock("D") -> 30)),
        "C2" -> Client("C2",
                       425,
                       Map(Stock("A") -> 0,
                           Stock("B") -> 85,
                           Stock("C") -> 50,
                           Stock("D") -> 30)),
        "C3" -> Client("C3",
                       675,
                       Map(Stock("A") -> 10,
                           Stock("B") -> 15,
                           Stock("C") -> 0,
                           Stock("D") -> 0))
      )
    )
  }
}
