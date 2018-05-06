package provider

import model.Currency
import scalaz.concurrent.Task

object api {

  import java.time.LocalDateTime

  import model.{HistoricPrice, PriceTable, PriceTableForCurrency, Trade}


  /**
    * For each xtrade, see if we have the known Historic price if not look it up and add it to the Maps
    *
    * @param knownPrices Currently known historic prices
    * @param xtrades total list of cross trades
    * @return a Tuple of the Historic prices for all cross trades and a list of PriceTablefor Currencies for newly looked up prices
    *         which can be used to update the knownPrices store.
    *         TODO should I put an api on PriceTable to let me partition it by newly discovered and from already knownPrices?
    */
  //re-write with a more idiomatic map in map traversal/builder
  def mapifyHistoricPrices(knownPrices: List[HistoricPrice], xtrades: List[Trade] ): (PriceTable, List[PriceTableForCurrency]) = {
    import model.{PriceTableAtDate, PriceTableForCurrency}
    import provider.coinspot.CoinspotParser
    import scala.collection.mutable.ListBuffer

    // VARIABLES
    var pt4Cs: PriceTable = Map();
    var ptatDs: Map[LocalDateTime, PriceTableAtDate] = Map();

    // VALUES
    val lb: ListBuffer[PriceTableForCurrency] = ListBuffer();

    // walk the currently known prices
    for(kp <- knownPrices) {
      if(pt4Cs.contains(kp.base)) {
        val psAtDatetime = pt4Cs.get(kp.base).get
        if(psAtDatetime.prices.contains(kp.localDateTime)) {
          //ok we have it do nothing
          //val price = psAtDatetime.prices.get(kp.localDateTime).get
        }
        else {
          val psAtDt = psAtDatetime.prices + (kp.localDateTime -> kp.quotePrice)
          pt4Cs = pt4Cs + (kp.base -> PriceTableForCurrency(kp.base, psAtDt))
        }
      }
      else {
        pt4Cs = pt4Cs + (kp.base -> PriceTableForCurrency(kp.base, Map(kp.localDateTime -> kp.quotePrice)) )
      }
    }

    //todo re-write to map to a List of Futures of prices
    // Walk the cross trades
    // if we cant find a price in our known history then go to the internet and add it to the maps
    xtrades.foreach(xt => {
      //getthe price at time for the base currency
      if(pt4Cs.contains(xt.base)) {
        //NB we now the key exists so can safly get dont need option

        val psAtDatetime = pt4Cs.get(xt.base).get
        if(!psAtDatetime.prices.contains(LocalDateTime.parse(xt.datetime,CoinspotParser.df))) {
          val historic = historicPriceInAUDForCoin(xt.base, LocalDateTime.parse(xt.datetime,CoinspotParser.df))

          //update the list of newly discovered prices
          lb += historic

          // update the pt4Cs that we are Building up
          pt4Cs = pt4Cs + (xt.base -> historic)

        }
        else {
          //do nothing we have it.
        }
      } else {
        // we dont have it so add a new key and value for the PriceTable map

        //          val psAtDt = psAtDatetime.prices + (LocalDateTime.parse(xt.datetime,CoinspotParser.df) -> xt.quoteprice)
        val historic = historicPriceInAUDForCoin(xt.base, LocalDateTime.parse(xt.datetime,CoinspotParser.df))

        //update the list of newly discovered prices
        lb += historic

        // update the pt4Cs that we are Building up
        pt4Cs = pt4Cs + (xt.base -> historic)
      }

    })

    (pt4Cs, lb.toList)
  }




  /**
    * Wrap the fetch from website in a scalaz Task
    * @param coin
    * @param atdatetime
    * @return
    */
  def getHistoricTask(coin: Currency, atdatetime: LocalDateTime): Task[PriceTableForCurrency] = {
    Task {
      historicPriceInAUDForCoin(coin, atdatetime) }
  }



  /**
    * wraps the provider specific call in a simple usable interface
    * @param coin
    * @param date
    * @return
    */
  def historicPriceInAUDForCoin(coin: Currency, date: LocalDateTime) = {
    import provider.cryptocompare.Cryptocompare._
    import model.Utils._
    //calls cryptocompare rest API need to convert currency symbols from coinspot to crypto compare where necessary
    cryptoCompareHistoricPrices(date, List(coin).map(symbolMapFromCoinspotToCrytoCOmpare))
  }






  /**
    * Returns the text (content) from a REST URL as a String.
    * Inspired by http://matthewkwong.blogspot.com/2009/09/scala-scalaiosource-fromurl-blockshangs.html
    * and http://alvinalexander.com/blog/post/java/how-open-url-read-contents-httpurl-connection-java
    *
    * The `connectTimeout` and `readTimeout` comes from the Java URLConnection
    * class Javadoc.
    * @param url The full URL to connect to.
    * @param connectTimeout Sets a specified timeout value, in milliseconds,
    * to be used when opening a communications link to the resource referenced
    * by this URLConnection. If the timeout expires before the connection can
    * be established, a java.net.SocketTimeoutException
    * is raised. A timeout of zero is interpreted as an infinite timeout.
    * Defaults to 5000 ms.
    * @param readTimeout If the timeout expires before there is data available
    * for read, a java.net.SocketTimeoutException is raised. A timeout of zero
    * is interpreted as an infinite timeout. Defaults to 5000 ms.
    * @param requestMethod Defaults to "GET". (Other methods have not been tested.)
    *
    * @example get("http://www.example.com/getInfo")
    * @example get("http://www.example.com/getInfo", 5000)
    * @example get("http://www.example.com/getInfo", 5000, 5000)
    */
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  def get(url: String,
          connectTimeout: Int = 10000,
          readTimeout: Int = 5000,
          requestMethod: String = "GET") =
  {
    import java.net.{URL, HttpURLConnection}
    val connection = (new URL(url)).openConnection.asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    connection.setRequestMethod(requestMethod)
    val inputStream = connection.getInputStream
    val content = io.Source.fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close
    content
  }


}
