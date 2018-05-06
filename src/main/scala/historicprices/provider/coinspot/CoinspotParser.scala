package provider.coinspot

object CoinspotParser {
  import java.io.FileWriter
  import java.time.format.DateTimeFormatter

  import model._

  val pairpattern = """(.*)/(.*)""".r

  val df: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy hh:mm a")

  def readFile[T](filenmae: String)(handler: io.BufferedSource => T): T = {
    val bufferedSource = io.Source.fromFile(filenmae)
    try {
      handler(bufferedSource)
    } finally {
      bufferedSource.close()
    }
  }

  def writeFile[T](filename: String, append:Boolean = false)(handler: FileWriter => T): T = {
    val fw = new FileWriter(filename,append)
    try {
      handler(fw)
    } finally {
      fw.flush();
      fw.close();
    }
  }


  //TODO make csv a Buffered SOurce or something or Stream
  def tradesFromCSV(bufferedSource: io.BufferedSource): List[Trade] = {
    import scala.collection.mutable.ListBuffer

    val lb = new ListBuffer[Trade]()
    for (line <- bufferedSource.getLines) {
      import model.Currency

      val Array(datetime, direction, pair, baseamount, quotePrice, quoteFee, quoteamount, crap) = line.split(",").map(_.trim)
      //      val datetime2 = LocalDateTime.parse(datetime, df)
      val dir = if(direction == "Buy") Buy else Sell
      val pairpattern(base,quote) = pair
      val trade = Trade(datetime,dir,Currency(base),baseamount.toDouble,Currency(quote), quotePrice.toDouble, quoteFee.dropRight(4).toDouble)

      lb += trade
    }
    lb.toList
  }


  def pricesFromCSV(bufferedSource: io.BufferedSource): Map[Currency,Double] = {
    import scala.collection.mutable.{Map => MMap}

    val m: MMap[Currency,Double] = MMap()
    for (line <- bufferedSource.getLines) {
      import model.Currency

      val Array(curr, priceinAUD) = line.split(",").map(_.trim)

      m += (Currency(curr) -> priceinAUD.toDouble)
    }
    m.toMap
  }

  def currenciesFromList(bufferedSource: io.BufferedSource): List[Currency] = {
    import scala.collection.mutable.ListBuffer

    var lb = new ListBuffer[String]()
    for (line <- bufferedSource.getLines) {
      val l = line.trim
      lb += l
    }
    lb map {Currency(_)} toList
  }


  def historicPricesFromCSV(bufferedSource: io.BufferedSource): List[HistoricPrice] = {
    import java.time.LocalDateTime

    import model.Currency

    import scala.collection.mutable.ListBuffer

    val lb = ListBuffer[HistoricPrice]()

    for (line <- bufferedSource.getLines) {
      val Array(datetime,curr, price) = line.split(",").map(_.trim)
      val ldt = LocalDateTime.parse(datetime, df)
      lb += HistoricPrice(ldt, Currency(curr), price.toDouble)
    }
    lb.toList
  }

  /**
    *
    * @param historicPrices close over this and return a function from FileWriter to Unit that can be used with writeFile
    *                       in a loan pattern.
    * @return
    */
  def saveHistoricPrices(historicPrices: List[HistoricPrice]) = {
    val linesep = sys.props.get("line.separator").getOrElse("\n")
    val ret: (FileWriter) => Int = {fw => {
      historicPrices foreach { hp =>
        println(s"seen:- $hp")
        val ldt = df.format(hp.localDateTime)
        fw.write(s"$ldt,${hp.base.sym},${hp.quotePrice}$linesep")
      }
      historicPrices.size
    }}
    ret
  }

}

