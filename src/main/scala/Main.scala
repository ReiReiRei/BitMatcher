import java.io.File

import scala.io.Source

object Main {

  import Helper._
  import OrderBook._
  import Transaction._

  def main(args: Array[String]): Unit = {

    val orders =
      Source.fromFile(new File("orders.txt")).getLines().map(parseOrder)
    val clients = Source
      .fromFile(new File("clients.txt"))
      .getLines()
      .map(parseClient)
      .map(x => x.id -> x)
      .toMap

    val clientHolder = ClientHolder(clients)
    val freshOrderBook = OrderBook(List.empty[Order], List.empty[Transaction])
    val orderBook = processOrders(orders, freshOrderBook)
    val fClients = processTransactions(orderBook, clientHolder)
    println(fClients)
  }

}
