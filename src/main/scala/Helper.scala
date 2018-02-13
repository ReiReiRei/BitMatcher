object Helper {

  val stockList: List[Stock] = List[String]("A", "B", "C", "D").map(Stock)

  def parseOrder(line: String): Order = {
    line.split("\t").toList match {
      case clientId :: op :: stock :: price :: quantity :: Nil =>
        op match {
          case "s" =>
            Order(clientId, Sell, Stock(stock), price.toLong, quantity.toLong)
          case "b" =>
            Order(clientId, Buy, Stock(stock), price.toLong, quantity.toLong)
        }
      case _ => throw new RuntimeException("")
    }
  }

  def parseClient(line: String): Client = {
    line.split("\t").toList match {
      case clientId :: balance :: stocks =>
        Client(clientId,
               balance.toLong,
               (stockList zip stocks.map(_.toLong)).toMap)
      case _ => throw new RuntimeException("")
    }
  }
}
