package model

object Utils {

  import model.Currency

  //mappers to convert symbols that dont match between crypto currency price providers
  val symbolMapFromCoinspotToCrytoCOmpareMap = Map("STR" -> "XLM")
  val symbolMapFromCrytoCOmpareToCoinspotMap = Map("XLM" -> "STR")

  def symbolMapFromCoinspotToCrytoCOmpare = (c: Currency) => Currency(symbolMapFromCoinspotToCrytoCOmpareMap.getOrElse(c.sym,c.sym))
  def symbolMapFromCrytoCOmpareToCoinspot = (c: Currency) => Currency(symbolMapFromCrytoCOmpareToCoinspotMap.getOrElse(c.sym,c.sym))

}