import cats.implicits._

sealed trait Operation {
  val clientId: String
  val price: Long
  val quantity: Long
  val stock: Stock

  def op(client: Client): Client
}

case class BuyOperation(clientId: String,
                        price: Long,
                        quantity: Long,
                        stock: Stock)
    extends Operation {
  override def op(client: Client): Client =
    client.copy(initBalance = client.initBalance - price * quantity,
                stocks = client.stocks |+| Map(stock -> quantity))
}

case class SellOperation(clientId: String,
                         price: Long,
                         quantity: Long,
                         stock: Stock)
    extends Operation {
  override def op(client: Client): Client = {
    client.copy(initBalance = client.initBalance + price * quantity,
                stocks = client.stocks |+| Map(stock -> -quantity))
  }

}
