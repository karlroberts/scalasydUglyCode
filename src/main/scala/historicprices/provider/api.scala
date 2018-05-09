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

    // walk the currently known prices
    val knownPT4CMap = knownPrices groupBy(_.base) mapValues(hps => {
      hps.map { hp => (hp.localDateTime -> hp.quotePrice)} toMap
    }) map (tup => {tup._1 -> PriceTableForCurrency(tup._1, tup._2)})

    //filter out crosstrades for which we already know the price at a date using knownPT4CMap to discriminate
    // n.b. use Option.isEmpty to negate the positive logic
    //then Map the remaining trades into Tasks to fetch the missing price from the internet.
    val historicTasks: List[Task[PriceTableForCurrency]] = xtrades.filter(xt => {
      for {
        pt4c <- knownPT4CMap.get(xt.base)
        x <- pt4c.prices.get(LocalDateTime.parse(xt.datetime,CoinspotParser.df))
      } yield x
    }.isEmpty
    ).map(xt => getHistoricTask(xt.base, LocalDateTime.parse(xt.datetime,CoinspotParser.df)))

    //run all the tasks in parralel
    val gatheredHistoricPrices: List[PriceTableForCurrency] = Task.gatherUnordered(historicTasks).run

    // fold down the
    val ret: Map[Currency, PriceTableForCurrency] = gatheredHistoricPrices.foldLeft(knownPT4CMap) ((acc,next) => {
      acc + (next.currency -> next)
    })

    (ret, gatheredHistoricPrices)
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
    * Wrap the fetch from website in a scalaz Task
    * @param coin
    * @param atdatetime
    * @return
    */
  def getHistoricTask(coin: Currency, atdatetime: LocalDateTime): Task[PriceTableForCurrency] = {
    Task {
      historicPriceInAUDForCoin(coin, atdatetime) }
  }









}
