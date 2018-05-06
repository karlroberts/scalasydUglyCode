package provider.cryptocompare

// functions that call on the cryptocompare api and map to my model
object Cryptocompare {

  import java.time.LocalDateTime
  import java.util.Date

  import model.{Currency, PriceTableForCurrency}
  import provider.api.get


  // get historic prices from cryptoCompareHistoricPrices
  /**
    * TODO move to a providers API like CoinspotParser.scala
    *
    * @param date
    * @param fromcurrency
    * @param tocurrencies
    * @return Map of datetime to a map of quuote currencies to the Double price
    */
  def cryptoCompareHistoricPrices(date: LocalDateTime,
                                  fromcurrency: List[Currency],
                                  tocurrencies: List[Currency] = List(Currency("AUD")))
  : PriceTableForCurrency = {

    //    https://min-api.cryptocompare.com/data/pricehistorical?fsym=ETH&tsyms=BTC,USD,EUR&ts=1452680400&extraParams=your_app_name
    import Utils.{localdateTimeTounixtime, parseHistoricPriceJSON}


    val url = "https://min-api.cryptocompare.com/data/pricehistorical"
    val froms = fromcurrency.map(_.sym).mkString(",")
    val tos = tocurrencies.map(_.sym).mkString(",")
    val unixdate = localdateTimeTounixtime(date)

    val result = get(s"${url}?fsym=${froms}&tsyms=${tos}&ts=$unixdate")
    println(s"THE historic JSON = $result")
    val pricedAsset = parseHistoricPriceJSON(result, fromcurrency.head, tocurrencies)

    pricedAsset match {
      case Left(wtf) => {
        println(s"oops with priceing from internet $wtf")
        PriceTableForCurrency(fromcurrency.head, Map())
      }
      case Right(pa) => {
        //HACK only want AUD  should filter this before hand or allow all info back!!!
        val filtered = pa.quotePrices.filter(p => p._1 == Currency("AUD"))
        filtered.map(p => PriceTableForCurrency(fromcurrency.head, Map(date -> p._2))).head // lots of dodgy head shit need Non empty list or a single value
      }
    }
  }




}
