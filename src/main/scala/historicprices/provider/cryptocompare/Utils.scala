package provider.cryptocompare

import java.time.LocalDateTime
import java.util.Date

object Utils {

  import model.{Currency, PricedAsset}


  //convert Dates and LocalDateTime to unix date.
  def dateTounixtime(date: Date): Long = date.getTime.asInstanceOf[Long] / 1000

  def localdateTimeTounixtime(time: LocalDateTime): Long = {
    import java.time.ZoneId
    val zoneId = ZoneId.systemDefault(); // or: ZoneId.of("Europe/Oslo");

    time.atZone(zoneId).toEpochSecond();
  }

  /**
    * Walk the JSON with a cursor to extract values, can't use a codec as JSON is not structured well, each query will have diff structure
    * eg {"DODGE":...} or {"XRP": ...} so i need to walk it looking for the expected values.
    *
    * We parse {"DOGE":{"AUD":0.008835,"XRP":0.004988}} to a PricedAsset
    *
    * @param rawJson
    * @param fromcurrency
    * @param toCurrencies
    * @return
    */
  def parseHistoricPriceJSON(rawJson: String, fromcurrency: Currency, toCurrencies: List[Currency]): Either[String, PricedAsset] = {
    import argonaut._, Argonaut._
    import monocle.macros.syntax.lens._
    val parsed: Either[String, Json] = Parse.parse(rawJson)
    val myTocurrs = Currency("XRP") :: toCurrencies

    parsed.right.map(outer => {
      val cursor = outer.cursor
      val ps = for {
        toC <- myTocurrs
        prices <- cursor.downField(fromcurrency.sym)
        price <- prices.downField(toC.sym)
        thep <- price.focus.number
      } yield (toC, thep.toDouble.get)

      PricedAsset(fromcurrency, ps)
    })

  }
}
