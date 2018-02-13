import cats.Eval
import cats.data.{IndexedStateT, State}

case class OrderBook(orders: List[Order], transactions: List[Transaction])

object OrderBook {
  def put(order: Order): State[OrderBook, Unit] = State[OrderBook, Unit] {
    orderBook =>
      (orderBook.copy(orders = order :: orderBook.orders), ())
  }

  val genTransactions: State[OrderBook, Unit] = State[OrderBook, Unit] {
    orderBook =>
      def simpleMatcher(left: Order, right: Order) = {
        (left.stock == right.stock) && (left.price == right.price) && (left.quantity == right.quantity)
        /*&& (left.clientId != right.clientId)*/

      }

      (orderBook.orders.head match {
        case buy @ Order(_, Buy, _, _, _, _) =>
          /*можно SortedSet , но спорно ввиду того, что это сильно от матчинга зависит и вообще зависит от правила матчинга
          (матчится ли при каждой заявке или в рамках какого-то окна собираются заявки, а затем матчатся )
           */
          orderBook.orders.tail.reverse.find { order =>
            order.direction == Sell &&
            simpleMatcher(order, buy)
          }
        case sell @ Order(_, Sell, _, _, _, _) =>
          orderBook.orders.tail.reverse.find { order =>
            order.direction == Buy &&
            simpleMatcher(sell, order)
          }
      }).map(x => MatchedOrders(List(orderBook.orders.head, x))) match {
        case Some(matched) =>
          val ops = matched.orders.map {
            case buy @ Order(_, Buy, _, _, _, _) =>
              import buy._
              BuyOperation(clientId, price, quantity, stock)
            case sell @ Order(_, Sell, _, _, _, _) =>
              import sell._
              SellOperation(clientId, price, quantity, stock)
          }

          (orderBook.copy(
             transactions = Transaction(ops) :: orderBook.transactions,
             orders = orderBook.orders.diff(matched.orders)),
           ())
        case None => (orderBook, ())

      }
  }

  def tic(order: Order): IndexedStateT[Eval, OrderBook, OrderBook, Unit] =
    for {
      _ <- put(order)
      _ <- genTransactions
    } yield ()

  def processOrders(orders: Iterator[Order],
                    freshOrderBook: OrderBook): OrderBook =
    orders.foldLeft(freshOrderBook)((x, y) => tic(y).runS(x).value)
}
