package model

import java.time.LocalDateTime


case class Currency(sym: String) extends AnyVal {
  def show = sym
}
object Currency {

  implicit val currencyOrdering: Ordering[Currency] = new Ordering[Currency]{
    override def compare(x: Currency, y: Currency) = x.sym.compareTo(y.sym)
  }

}

case class AvPriceHolding(holding: Holding, price: Price) {
  def show: String = price.toString
}

case class Position(currency: Currency, amount:Double, tradeHistory: List[Trade])

sealed trait Direction
case object Buy extends Direction
case object Sell extends Direction

case class Trade(datetime:String, direction: Direction, base: Currency, baseamount: Double, quote: Currency, quoteprice: Double, quotefee: Double)//, quoteInAUD: Double)

/** Value of a position in  currency **/
case class Value(current: Double) extends AnyVal
/** Profit or Loss of a position in a Currency **/
case class PandL(current: Double) extends AnyVal

/**
  * Class to manage state with an API
  *
  * The api works on an immutable model of the current position is described by trades.
  * If a cross trade was made eg buy XRP with BTC then we lookup the price at that trade time in AUD so we can correctly value the trade in AUD
  * so we can calculate an acurate average trade price for the asset.
  *
  * NB it is up to the caller to set (or filter) the correct trade list and hstoric prices.
  * if a historic price is missing then that trades contribution to average price is simply skipped!!!!
  *
  * @param trades List of previous trades
  * @param historicPrices PriceTable of Previously looked up historic prices, used for valueing cross trades in some fiat currency eg AUD
  */
case class Positions(trades: List[Trade], historicPrices: PriceTable = Map()) {
  lazy val amounts: Map[Currency, Position] = _amounts



  private def _amounts: Map[Currency, Position] = {
    import scala.collection.mutable.{Map => MMap}
    val ret = trades.foldLeft(MMap[Currency, Position]()) { (acc, t) =>
      t.direction match {
        case Buy =>
          acc +=
            (t.base -> Position(t.base, acc.getOrElse(t.base, zeroPos).amount + t.baseamount, t::acc.getOrElse(t.base, zeroPos).tradeHistory )) +=
            // minus the quote currency ammount as we ae paying forbase with it, but aso tak away the fee!. use quote price = 1 in ave price because no conversion when you are the quote currency
            // TODO do I need to set a av price for quote currency? shouldnt it alway be 1?
            (t.quote -> Position(t.quote, acc.getOrElse(t.quote, zeroPos).amount - (t.baseamount * t.quoteprice) - t.quotefee, Trade(t.datetime, Sell,t.quote,(t.baseamount * t.quoteprice) - t.quotefee,t.base, 1/t.quoteprice, t.quotefee * (1/t.quoteprice))  ::acc.getOrElse(t.quote, zeroPos).tradeHistory ))


        case Sell =>
          acc +=
            (t.base -> Position(t.base, acc.getOrElse(t.base, zeroPos).amount - t.baseamount, t::acc.getOrElse(t.base, zeroPos).tradeHistory)) +=
            (t.quote -> Position(t.quote, acc.getOrElse(t.quote, zeroPos).amount + (t.baseamount * t.quoteprice) - t.quotefee, Trade(t.datetime, Buy,t.quote,(t.baseamount * t.quoteprice) - t.quotefee,t.base, 1/t.quoteprice, t.quotefee * (1/t.quoteprice))::acc.getOrElse(t.quote, zeroPos).tradeHistory))

      }
    }
    ret.toMap
  }



  /** Uses historic prices to lookup equiv price in AUD if the trade was a cross trade
    * @param position
    * @return
    */
  def avePrice(position: Position): AvPriceHolding = {
    import provider.coinspot.CoinspotParser
    //    position.tradeHistory.filter(_.quote == Currency("AUD")).foldLeft(AvPriceHolding(0d, 0d))((acc, t) =>
    position.tradeHistory.foldLeft(AvPriceHolding(0d, 0d))((acc, t) =>

      t.quote match {

        //trade paid for in AUD
        case Currency("AUD") => AvPriceHolding(
          acc.holding + t.baseamount,
          ((acc.holding * acc.price) + (t.baseamount * t.quoteprice)) / (acc.holding + t.baseamount)
        )

        //any other currency
        case c@Currency(curr) => {

          val x = for {
            priceTableforC <- historicPrices.get(c)
            priceInAUD <- priceTableforC.prices.get(LocalDateTime.parse(t.datetime, CoinspotParser.df))
          } yield {

            // use the historic price price of the quote currency to convert price in quot cur to AUD
            AvPriceHolding(
              acc.holding + t.baseamount,
              ((acc.holding * acc.price) + (t.baseamount * (t.quoteprice * priceInAUD))) / (acc.holding + t.baseamount)
            )
          }
          //          println(s"XXXXXXX trade in ave: $x  in trade $t")
          x.getOrElse(acc) // if there is no such thing then return previous accumulator , ie skip it
        }
      }


    )
  }


  // TODO this is worng pass in the PriceTable then use it in avePrice()
  def expressPriceIn(quote: Currency, prices: Map[Currency, Price])(base: Currency, price: Price): Option[Price] = {
    for {
      quotePriceInCommon <- prices.get(quote)
      basePriceInCommon  <- prices.get(base)
    } yield price * basePriceInCommon / quotePriceInCommon
  }

  def priceInAUD(prices: Map[Currency, Price])(trade: Trade) = trade.quote match {
    case Currency("AUD") => trade.quoteprice
    case _ => expressPriceIn(Currency("AUD"), prices)(trade.base, trade.quoteprice).getOrElse(0d)
  }

  //    amounts.get(currency) map (position)
  //    AvPriceHolding( existingAv.holding + addAmount, ((existingAv.holding*existingAv.price) + (addAmount*atPrice))/(existingAv.holding+addAmount) )


  def +(t: Trade) = this.copy(t :: this.trades)

  /**
    * value of the individual positions in a Map
    * @param prices
    * @return
    */
  def values(prices: Map[Currency, Price]): Map[Currency, Double] = {
    prices map { case (c,price) => (c, amounts.getOrElse(c,zeroPos).amount * price) }
  }

  /**
    * Value of all of the current position given these prices
    * @param prices
    * @return
    */
  def value(prices: Map[Currency, Price]) = values(prices).foldLeft(0d) ( _+_._2)

  def percentProfit(prices: Map[Currency, Price]): Map[Currency, Double] = amounts.mapValues(position => {
    val avPriceHolding = avePrice(position)

    {
      prices.get(position.currency) map (price => {
        val totalReturn = price * position.amount
        val cost = avPriceHolding.price * position.amount
        100 * (totalReturn - cost) / cost
      })
    } getOrElse (0d)
  })

  //  def valuePerTrade(prices: Map[Currency, Price]) = {
  //    trades.f
  //  }


}

object Positions {


}

