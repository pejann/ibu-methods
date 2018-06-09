package model

sealed trait RagerAddition {
  def calculateRagerAddition(addition: Addition, boilGravity: Double, finalVolume: Int): Float = {
    val alphaAcidDecimal:Float = addition.alphaAcidPercentage.toFloat / 100
    val gramsOfHop:Float = addition.hopMass

    var ga:Double = 0
    if (boilGravity > 1.050) {
      ga = (boilGravity - 1.050) / 0.2
    }

    // Rager Utilization
    val utilization:Double = 18.11 + (13.86 * scala.math.tanh((addition.timeBoiled-31.32) / 18.27))
    val editedUtilization:Double = utilization/100

    val addedAlphaAcid:Float = (alphaAcidDecimal * editedUtilization.toFloat * gramsOfHop * 1000) /
      (finalVolume * (1 + ga.toFloat))

    addedAlphaAcid
  }
}

sealed trait TinsethAddition {
  def calculateTinsethAddition(addition: Addition, boilGravity: Double, finalVolume: Int): Float = {
    val alphaAcidDecimal:Float = addition.alphaAcidPercentage.toFloat / 100
    val gramsOfHop:Float = addition.hopMass

    // Tinseth Utilization
    val bignessFactor = 1.65 * scala.math.pow(0.000125, boilGravity - 1)
    val expTimeBoiled:Double = -0.04 * addition.timeBoiled
    val boilTimeFactor:Double = (1 - scala.math.exp(expTimeBoiled)) / 4.15

    val alphaAcidUtilization:Float = bignessFactor.toFloat * boilTimeFactor.toFloat
    var addedAlphaAcid:Float = (alphaAcidUtilization * alphaAcidDecimal * gramsOfHop * 1000) / finalVolume

    addedAlphaAcid
  }
}

case class Addition (alphaAcidPercentage: Int, hopMass: Float, timeBoiled: Int) extends TinsethAddition with RagerAddition

case class CalculatedIBU(ibu: Float, tinsethIBU: Float, ragerIBU: Float, boilGravity: Double, finalVolume: Int,
                         additions: Option[List[Addition]])
