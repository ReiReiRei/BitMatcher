import scala.annotation.tailrec

case class Transaction(operations: List[Operation])

object Transaction {

  implicit class TransactionHelper(h: ClientHolder) {

    def doTransaction(transaction: Transaction): ClientHolder = {
      @tailrec
      def dTr(c: ClientHolder, ops: List[Operation]): ClientHolder = ops match {
        case Nil =>
          println("")
          c
        case x :: t =>
          val client = c.clients(x.clientId)
          val processedClient =
            c.copy(clients = c.clients + (x.clientId -> x.op(client)))
          /*
          println(
            x.getClass.getName + s"  ${client.id} stock: ${x.stock.name} price:${x.price} quantity:${x.quantity}\n" +
              s"balance: ${client.initBalance} -----> ${processedClient.clients(client.id).initBalance}\n" +
              s"stock balance: ${client
                .stocks(x.stock)} -----> ${processedClient.clients(x.clientId).stocks(x.stock)}")*/

          dTr(processedClient, t)

      }
      // println("")
      dTr(h, transaction.operations)

    }

  }

  def processTransactions(orderBook: OrderBook,
                          clientHolder: ClientHolder): ClientHolder =
    orderBook.transactions.foldRight(clientHolder) { (h, t) =>
      t.doTransaction(h)
    }
}
