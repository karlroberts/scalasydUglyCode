package object model {
  // (NumofCoins, AvePricePerCoin)
  type Holding = Double
  type Price = Double
  val zeroCurrency = Currency("")
  val zeroPos = Position(zeroCurrency ,0d, Nil)
  type PriceTable = Map[Currency, PriceTableForCurrency]
}
