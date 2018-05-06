object Main extends App {

  def usage = "oops"

  import model.Positions
  if (args.length == 0) println(s"$usage: using defaults")
  val arglist = args.toList

  type OptionMap = Map[Symbol, String]
  def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
    def isSwitch(s : String) = (s(0) == '-')
    list match {
      case Nil => map
//      case "--trades" :: value :: tail =>
//        nextOption(map ++ Map('tradeFile -> value), tail)
      case "--prices" :: value :: tail =>
        nextOption(map ++ Map('pricesFile -> value), tail)

      case "--historic" :: value :: tail =>
        nextOption(map ++ Map('historicpricesFile -> value), tail)

      case _@wtf => println(s"Unknown option $wtf")
        sys.exit()
    }
  }
  val options = nextOption(Map(),arglist)

  import java.time.LocalDateTime
  import java.time.format.DateTimeFormatter

  import model._
  import provider.coinspot.CoinspotParser._

  import scala.collection.mutable
  import provider.api.mapifyHistoricPrices

  val sep = sys.props.getOrElse("file.separator","/")
  val homedir = sys.props.get("user.home")
  val projectdir = "uglycode"

  val tradePath = homedir map (h => s"${h}${sep}${projectdir}${sep}coinspotpos${sep}trades.csv") getOrElse s".${sep}config${sep}trades.csv"
  val currenciesPath = homedir map (h => s"${h}${sep}${projectdir}${sep}coinspotpos${sep}coinlist.txt") getOrElse s".${sep}config${sep}coinlist.txt"
  val historicpricesPath = homedir map (h => s"${h}${sep}${projectdir}${sep}coinspotpos${sep}historicPrices.csv") getOrElse s".${sep}config${sep}historicPrices.csv"

  val trades: List[Trade] = readFile( options.get('tradeFile).fold(tradePath) ( identity(_) ))(tradesFromCSV)
  val prices2: List[Currency] = readFile( options.get('pricesFile).fold(currenciesPath) ( identity(_) ))(currenciesFromList)
  val historicprices: List[HistoricPrice] = readFile( options.get('historicpricesFile).fold(historicpricesPath) ( identity(_) ))(historicPricesFromCSV)


  val crosstrades = trades.filter(_.quote != Currency("AUD"))
  val (priceTable, updateTheseInHistoricFile)= mapifyHistoricPrices(historicprices, crosstrades )

  //fix up the historic file
  val hpf = options.get('historicpricesFile)
  hpf.foreach(f => {
    println(s"WRITING ><><><><><><>< to file ${f.toString}")

    val int:Int = writeFile(f.toString, true)(saveHistoricPrices(updateTheseInHistoricFile.flatMap(_.asHistoricPrices)))
  })


}
