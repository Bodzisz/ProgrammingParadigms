// Kacper Wójcicki

// Zad 1
class Time (private var hourVal : Int):
  if hour < 0 then hourVal = 0

  def hour : Int = hourVal

  def hour_=(newHourVal : Int) : Unit =
    if newHourVal < 0 then hourVal = 0
    else hourVal = newHourVal

// Zad 2
// a)
class Time2(private var hourVal : Int, private var minuteVal : Int):
  require(0 <= hourVal && hourVal < 24)
  require(0 <= minuteVal && minuteVal < 60)

  def hour : Int = hourVal
  def minute : Int = minuteVal

  def hour_=(newHourVal : Int) : Unit =
    require(0 <= newHourVal && newHourVal < 24)
    hourVal = newHourVal

  def minute_=(newMinuteVal : Int) : Unit =
    require(0 <= newMinuteVal && newMinuteVal < 60)
    minuteVal = newMinuteVal

  def before(other : Time2) : Boolean =
    (hourVal < other.hourVal) || (hourVal == other.hourVal && minuteVal < other.minuteVal)

// b)
class Time3(private var hourVal : Int, private var minuteVal : Int):
  require(0 <= hourVal && hourVal < 24)
  require(0 <= minuteVal && minuteVal < 60)

  private var minutesAfterMidnight = hourVal * 60 + minuteVal

  def hour : Int = minutesAfterMidnight / 60
  def minute : Int = minutesAfterMidnight % 60

  def hour_=(newHourVal : Int) : Unit =
    require(0 <= newHourVal && newHourVal < 24)
    minutesAfterMidnight = minute + newHourVal * 60

  def minute_=(newMinuteVal : Int) : Unit =
    require(0 <= newMinuteVal && newMinuteVal < 60)
    minutesAfterMidnight = hour + newMinuteVal

  def before(other : Time3) : Boolean =
    (hourVal < other.hourVal) || (hourVal == other.hourVal && minuteVal < other.minuteVal)

// Zad 3
class Pojazd(val producent : String, val model : String, val rokProdukcji : Int = -1, var numerRejestracyjny : String = ""):
  def this(producent : String, model : String, numerRejestracyjny : String) =
    this(producent, model, -1, numerRejestracyjny)

  override def toString: String = model + " " + rokProdukcji + " " + numerRejestracyjny


object Lista9:
  def main(args : Array[String]) : Unit =
    val time = Time(-12)
    val time2 = Time(10)
    println(time.hour)
    println(time2.hour)

    val time3 = Time2(9,40)
    val time4 = Time2(10, 40)
    println(time3.before(time4))

    val pojazd1 = new Pojazd("Volvo", "XC60", 2016, "DBA1234")
    val pojazd2 = new Pojazd("Volvo", "XC70","DBA1234")
    val pojazd3 = new Pojazd("Volvo", "XC80")
    val pojazd4 = new Pojazd("Volvo", "XC90", 2016)
    println(pojazd1)
    println(pojazd2)
    println(pojazd3)
    println(pojazd4)

