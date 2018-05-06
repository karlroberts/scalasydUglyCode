package model

import java.net.URL
import java.time.LocalDateTime

import scala.io.BufferedSource

case class HistoricPrice(localDateTime: LocalDateTime, base:Currency, quotePrice: Double)

case class PricedAsset(base:Currency, quotePrices: List[(Currency,Double)])

/**
  * Prices of currencies at a specific time
  * @param prices
  */
case class PriceTableAtDate(datetime: LocalDateTime, prices: Map[Currency,Double]) {

  /**
    * Will overwrite if already have a price for this currency
    * @param historicPrice
    * @return
    */
  def +=(historicPrice: HistoricPrice) = {
    val hp = (historicPrice.base, historicPrice.quotePrice)
    this.copy(prices = prices + hp )
  }

  /**
    * add stuff only if same date
    * @param other
    * @return
    */
  def ++=(other:PriceTableAtDate) = if(other.datetime == datetime) this.copy(prices = prices ++ other.prices) else this
}

case class PriceTableForCurrency(currency: Currency, prices: Map[LocalDateTime,Double]) {
  /**
    * Will overwrite if we alredy have a price at this time
    * @param historicPrice
    * @return
    */
  def +=(historicPrice: HistoricPrice) = {
    val hp = (historicPrice.localDateTime, historicPrice.quotePrice)
    this.copy(prices = prices + hp)
  }
  def ++=(other:PriceTableForCurrency) = if(other.currency == currency) this.copy(prices = prices ++ other.prices) else this

  def asHistoricPrices: List[HistoricPrice] = {
    import scala.collection.mutable.ListBuffer
    val lb = ListBuffer[HistoricPrice]()
    prices.foreach(tup => {
      lb += HistoricPrice(tup._1,currency,tup._2)
    })
    lb.toList
  }
}

