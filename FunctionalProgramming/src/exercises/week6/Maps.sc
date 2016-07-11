object Maps{
  val romanNumerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

  capitalOfCountry("US")
//  capitalOfCountry("Andorra") // Exception
  capitalOfCountry get "Andorra" // Option

  def showCapital(country: String) =
    capitalOfCountry.get(country) match {
      case Some(capital) => capital
      case None => "Missing data"
    }

  showCapital("US")
  showCapital("Andorra")
}