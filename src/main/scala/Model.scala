sealed trait Direction
case object Buy extends Direction
case object Sell extends Direction
case class Order(clientId: String,
                 direction: Direction,
                 stock: Stock,
                 price: Long,
                 quantity: Long,
                 timestamp: Long = System.currentTimeMillis())

case class Stock(name: String)

case class Client(id: String, initBalance: Long, stocks: Map[Stock, Long])
case class ClientHolder(clients: Map[String, Client]) {
  override def toString: String =
    clients.toList
      .map(_._2)
      .sortBy(_.id)
      .map { client =>
        import client._
        id + "\t" + initBalance + "\t" + stocks.toList
          .sortBy(_._1.name)
          .map(_._2)
          .mkString("\t")
      }
      .mkString("\n")

}
