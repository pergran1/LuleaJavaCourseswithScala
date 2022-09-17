package IntroductionProgramming

object FirstTask_PrintTable extends  App{

  val BATTERY: Double = 35.87

  val current_list: List[Int] = List(10, 16, 10, 16, 32)
  val voltage_list: List[Int] = List(230, 230, 400, 400, 400)

  print("Batteri " + BATTERY + "(kwh)\nStröm(A) \tSpänning(V) \tLaddeffekt(kw) \tLaddningstid(h)\n")
  print("=======================================================================\n")


  (0 until current_list.length).foreach { n =>
    val x = current_list(n)
    val y = voltage_list(n)
    print_info(x, y)
  }

  def print_info(current: Int, voltage: Int): Unit = {
    if (voltage == 230)
      val x = current * voltage / 1000.0
      println(s"$current          $voltage             $x")
    else
      val x = (current * voltage / 1000.0) * Math.sqrt(3)
      val rounded: Double = round_to_double(x, 2)
      println(s"$current          $voltage             $rounded")
  }

  def round_to_double(value: Double, nbr_of_decimals: Int): Double =  {
    val scale: Double = Math.pow(10, nbr_of_decimals)
    Math.round(value * scale) / scale
  }





}
