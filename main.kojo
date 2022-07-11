import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap
import scala.language.postfixOps
import scala.collection.mutable.Map
import scala.collection.mutable.Seq

val HEADER_TEXT_SCALE_FACTOR = 5

var MIGRATION_NUM_VISUAL_STEPS = 5

var ICON_FOLDER_PATH = "/home/dansolombrino/GitHub/Africa-Wildlife/icons/"
var BACKGROUND_FOLDER_PATH = "/home/dansolombrino/GitHub/Africa-Wildlife/background/"

// TODO load from disk
val TEMPERATURE_YEARLY_MULTIPLICATIVE_FACTOR = 1.025

val MAX_daysWithoutSatisfiedNeeds = 2

val DAYS_IN_YEAR = 7
val DELAY_MS = 1500

val MAX_feltTemperature_LOWERBOUND = 20
val MAX_feltTemperature_UPPERBOUND = 40

val MIN_WATER_LOWERBOUND = 1.0
val MIN_WATER_UPPERBOUND = 3.5

val MIN_NUM_OF_ANIMALS = 6
val MAX_NUM_OF_ANIMALS = 10

val ANIMAL_SPECIES = List("Lion", "Elephant", "Zebra")

object LionParams {
  val maxfeltTemperature = 40;
  val minWater = 2
  val iconFilePath = ICON_FOLDER_PATH + "lion_64.png"
}

object ElephantParams {
  val maxfeltTemperature = 45;
  val minWater = 2
  val iconFilePath = ICON_FOLDER_PATH + "elephant_64.png"
}

object ZebraParams {
  val maxfeltTemperature = 45;
  val minWater = 2
  val iconFilePath = ICON_FOLDER_PATH + "zebra_64.png"
}

trait Animal extends Drawable {

    val maxfeltTemperature : Double
    val minWater : Double
    val id : Int
    var daysWithoutSatisfiedNeeds = 0
    var rival = ""
    
    var lifePoints = Seq.fill(DAYS_IN_YEAR)(1.0)
    var drankWater = Seq.fill(DAYS_IN_YEAR)(0.0)
    var feltTemperature = Seq.fill(DAYS_IN_YEAR)(0.0)

    def isAlive(day : Int) : Boolean = {
        return lifePoints(day) > 0
    }

    def drinkWater(day: Int, water: Double) {
        drankWater(day) += water
    }

    def getId() : Int = {
        return id
    }

    def updateLifePoints(day : Int) : Boolean = {
        lifePoints(day) = 50 * drankWater(day) - 1 * feltTemperature(day)

        if (lifePoints(day) < 0) {
            die(day)

            return true
        }

        return false
    }

    def updateLifePoints(day : Int, numEncounteredRivals : Int) : Boolean = {
        lifePoints(day) = 50 * drankWater(day) - 1 * feltTemperature(day) - 10 * numEncounteredRivals

        if (lifePoints(day) < 0) {
            die(day)

            return true
        }

        return false
    }

    def die(day : Int) {
        for (i <- day to DAYS_IN_YEAR - 1) {
            lifePoints(i) = -1
        }

        icon.erase()
    }

    def getDesiredWater() : Double = {
        
        return Random.between(minWater * 0.8, minWater)
        
    }

    def feelTemperature(day : Int, temperatureValue : Double) {
        
        feltTemperature(day) = temperatureValue
        
    }

    def handleNeedsSatisfaction(actual_water : Double, desired_water : Double) {
        
        if (actual_water < desired_water) {
            
            daysWithoutSatisfiedNeeds += 1
        
        }
        
    }

    def evaluateMigration(actual_water : Double, desired_water : Double) : Boolean = {
        handleNeedsSatisfaction(actual_water, desired_water)

        return daysWithoutSatisfiedNeeds >= MAX_daysWithoutSatisfiedNeeds
    }

    def countEncounteredRivals(neighbouringAnimals : ListBuffer[Animal]) : Int = {
        var numEncounteredRivals = 0

        neighbouringAnimals.foreach(
            a => {
                numEncounteredRivals += (if (a.getClass.getSimpleName == this.rival) 1 else 0)
            }
        )

        return numEncounteredRivals
    }

    def migrate(ws : WaterSource) {
        println("CurrentPosition: " + this.icon.position)
        println("TargetPosition: " + ws.position)

        var absDist = (ws.position._1 - this.icon.position.x, ws.position._2 - this.icon.position.y)
        println("absDistance: " + absDist)

        var step_x = absDist._1 / MIGRATION_NUM_VISUAL_STEPS
        var step_y = absDist._2 / MIGRATION_NUM_VISUAL_STEPS

        for (i <- 1 to MIGRATION_NUM_VISUAL_STEPS) {
            
            icon.translate(step_x,step_y)
            Thread.sleep(DELAY_MS)
            
            if (icon.collidesWith(ws.icon)) {
                icon.translate(getRandomShift(), getRandomShift())
            }
            
        }

        daysWithoutSatisfiedNeeds = 0
        
    }

}

class Lion(
    val maxfeltTemperature : Double, 
    val minWater : Double, 
    val id : Int, 
    val icon : Picture, 
    var position: (Int, Int)
) extends Animal {

    rival = "Zebra"

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Lion, maxfeltTemperature: " + maxfeltTemperature + ", lifePoints: " + lifePoints + ", drankWater: " + drankWater + ", feltTemperature: " + feltTemperature + "\n"
    }
}

class Elephant(
    val maxfeltTemperature : Double, 
    val minWater : Double, 
    val id : Int, 
    val icon : Picture,
    var position : (Int, Int)
) extends Animal {

    rival = "Lion"

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Elephant, maxfeltTemperature: " + maxfeltTemperature + ", lifePoints: " + lifePoints + ", drankWater: " + drankWater + ", feltTemperature: " + feltTemperature + "\n"
    }
}

class Zebra(
    val maxfeltTemperature : Double, 
    val minWater : Double, 
    val id : Int, 
    val icon : Picture,
    var position : (Int, Int)
) extends Animal {

    rival = "Lion"

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Zebra, maxfeltTemperature: " + maxfeltTemperature + ", lifePoints: " + lifePoints + ", drankWater: " + drankWater + ", feltTemperature: " + feltTemperature + "\n"
    }
}

class Fauna(val faunaSize : Int) {

    var faunaCount = faunaSize
    
    var fauna: ListBuffer[Animal] = ListBuffer()

    def populateFauna(): ListBuffer[Animal] = {
        
        for (i <- 1 to faunaSize) {
            
            val animalSpecie = ANIMAL_SPECIES(Random.between(0, ANIMAL_SPECIES.length))
        
            animalSpecie match {
        
                case "Lion" => {
                    fauna += new Lion(
                        LionParams.maxfeltTemperature, 
                        LionParams.minWater, 
                        i, 
                        Picture.image(LionParams.iconFilePath), 
                        (0, 0)
                    )
                }
        
                case "Elephant" => {
                    fauna += new Elephant(
                        ElephantParams.maxfeltTemperature, 
                        ElephantParams.minWater, 
                        i, 
                        Picture.image(ElephantParams.iconFilePath),
                        (0, 0)
                    )
                }
        
                case "Zebra" => {
                    fauna += new Zebra(
                        ZebraParams.maxfeltTemperature, 
                        ZebraParams.minWater, 
                        i, 
                        Picture.image(ZebraParams.iconFilePath),
                        (0, 0)
                    )
                }
        
                case default => {
                    fauna += new Zebra(
                        ZebraParams.maxfeltTemperature, 
                        ZebraParams.minWater, 
                        i, 
                        Picture.image(ZebraParams.iconFilePath),
                        (0, 0)
                    )
                }
            }
        }
    
        return fauna
    }

    populateFauna()

    override def toString() : String = {
        var out = ""

        fauna.foreach(
            a => {
                out += a.toString()
            }
        )

        return out
 
    }

    def getAnimal(i: Int) : Animal = {
        return fauna(i)
    }

    def drawInCanvas(checkCollisionAgainst : List[Picture]) {
        //println("drawing animals...")

        fauna.foreach(
            a => {
                a.drawInCanvas(checkCollisionAgainst)
            }
        )
    }
}

val waterColor = color(88, 148, 245)
val WATER_COLOR = color(88, 148, 245)
val BLUE_COLOR  = color(0, 0, 255)
val RED_COLOR   = color(255, 0, 0)
val GREEN_COLOR = color(0, 255, 0)
val RED_COLOR_CHANNEL = 175
val BACKGROUND_COLOR = color(200, 235, 255)

val MIN_NUM_OF_WATER_SOURCES = 3
val MAX_NUM_OF_WATER_SOURCES = 4

val MAX_WATER_SOURCE_LEVEL = 10

val MAX_LEVEL_LOWERBOUND = 3
val MAX_LEVEL_UPPERBOUND = 4

val WATER_SOURCES_TYPES = List("Lake", "River")

def getRandomShift() : Int = {
    return Random.between(25, 50) * (if (Random.between(0, 2) == 1) -1 else 1)
 }

trait Drawable {
    var position : (Int, Int)
    val icon : Picture

    def drawInCanvas(checkCollisionAgainst : List[Picture]) {
        
        icon.setPosition(position._1, position._2)
        
        icon.draw()

        if (!checkCollisionAgainst.isEmpty) {
            
            checkCollisionAgainst.foreach(
                
                cca => {
                    if (cca.collidesWith(icon)) {
                        icon.translate(getRandomShift(), getRandomShift())
                    }
                }
                
            )  
                     
        }

    }
}

trait DrawableShape extends Drawable {
    
    val borderColor : Color
    val innerColor : Color
    val rotation : Int
    val thickness : Int
    
    var opacity = new ValueInRange(1.0, 1.0, 0.0)

    override def drawInCanvas(checkCollisionAgainst : List[Picture]) {

        icon.setPenColor(borderColor)
        icon.setFillColor(innerColor)
        icon.setRotation(rotation)
        icon.setPenThickness(thickness)

        super.drawInCanvas(checkCollisionAgainst)
    }
}

class ValueInRange(var value : Double, val max : Double, val min : Double) {
    
    def trimValueIntoRange() {
        if (value > max) {
            value = max
        }

        if (value < min) {
            value = min
        }
    }
    
    def setValue(newValue : Double) {
        value = newValue

        trimValueIntoRange()
    }
}


trait WaterSource extends DrawableShape {
    val maxLevel : Double
    var currentLevel = Seq.fill(DAYS_IN_YEAR)(maxLevel)
    val id : Int
    val name : String
    
    def removeWater(day: Int, water: Double, handleOpacity : Boolean) : Double = {

        var maxAvailableWater = 0.0

        if (currentLevel(day) - water >= 0) {
            currentLevel(day) -= water


            maxAvailableWater = water
        } else {
            maxAvailableWater = currentLevel(day)
            
            currentLevel(day) = 0
            
        }

        if (handleOpacity) {
            opacity.setValue(1.0 - currentLevel(day) / maxLevel)
            icon.setOpacity(opacity.value)
        }

        if (day + 1 < DAYS_IN_YEAR) {
            removeWater(day + 1, water / 10, false)
        }

        return maxAvailableWater

    }

    def canEqual(a: Any) = a.isInstanceOf[WaterSource]

    override def equals(that: Any): Boolean =
        
        that match {
            
            case that: WaterSource => {
                that.canEqual(this) &&
                this.name == that.name &&
                this.position == that.position &&
                this.rotation == that.rotation &&
                this.maxLevel == that.maxLevel
                
            }
            
            case _ => false
        }

    override def hashCode: Int = {
        name.hashCode + position.hashCode + rotation.hashCode + maxLevel.hashCode
    }

    
}

class Lake(
    val maxLevel : Double, 
    val id : Int, 
    val name : String, 
    val icon : Picture, 
    var position : (Int, Int), 
    val borderColor : Color, 
    val innerColor : Color, 
    val rotation : Int, 
    val thickness : Int
) extends WaterSource {

    override def toString() : String = {
        return "\n\t\tName: " + name + " --> Lake, currentLevel: " + currentLevel + ", maxLevel: " + maxLevel + "\n"
    }
}

class River(
    val maxLevel : Double, 
    val id : Int, 
    val name : String, 
    val icon : Picture, 
    var position : (Int, Int), 
    val borderColor : Color, 
    val innerColor : Color, 
    val rotation : Int, 
    val thickness : Int
) extends WaterSource {

    override def toString() : String = {
        return "\n\t\tName: " + name + " --> River, currentLevel: " + currentLevel + ", maxLevel: " + maxLevel + "\n"
    }
}

class WaterSources(val numOfWaterSources : Int) {

    // TODO load it from disk
    val waterSourcesList = ListBuffer("Chad", "Victoria", "Niger")
    
    var waterSources_new = new scala.collection.mutable.HashMap[String, WaterSource]

    waterSources_new("Chad") = new Lake(
        Random.between(MAX_LEVEL_LOWERBOUND, MAX_LEVEL_UPPERBOUND), 
        1, 
        "Chad", 
        Picture.ellipse(35, 20),
        (580, 760), 
        WATER_COLOR, 
        WATER_COLOR, 
        -45, 
        10
    )
    
    waterSources_new("Victoria") = new Lake(
        Random.between(MAX_LEVEL_LOWERBOUND, MAX_LEVEL_UPPERBOUND), 
        2, 
        "Victoria", 
        Picture.ellipse(35, 20),
        (850, 535), 
        WATER_COLOR, 
        WATER_COLOR, 
        45, 
        10
    )
    
    waterSources_new("Niger") = new River(
        Random.between(MAX_LEVEL_LOWERBOUND, MAX_LEVEL_UPPERBOUND), 
        3, 
        "Niger", 
        Picture.ellipse(10, 225),
        (365, 815), 
        WATER_COLOR, 
        WATER_COLOR, 
        -230, 
        10
    )
    
    override def toString() : String = {
        var out = ""
    
        waterSources_new.foreach(
            ws => {
                out += ws.toString()
            }
        )
    
        return out

    }

    def getRandomWaterSource() : WaterSource = {
        return waterSources_new(waterSourcesList(Random.between(0, numOfWaterSources)))
    }

    def getRandomWaterSource(exclude : WaterSource) : WaterSource = {
        
        var randomWaterSource = getRandomWaterSource()
        
        do {
            if (randomWaterSource.equals(exclude) == false) {
                return randomWaterSource
            }

            randomWaterSource = getRandomWaterSource()
            
        } while (true)

        return randomWaterSource
    }

    def drawInCanvas() {
       
        waterSources_new.foreach(
            ws => {
                ws._2.drawInCanvas(List())
            }
        )
    }
}

class HeaderElement(text : String, value : Double, position : (Int, Int), color : Color, scale : Double) {
    var element = Picture.text("")

    def update(day : Int, updatedValue : Double, updatedColor : Color) {
        
        if (day > -1) {
            element.erase()
        }

        element = Picture.text(text + ": " + updatedValue)
        element.setPosition(position._1, position._2)
        element.setPenColor(updatedColor)
        element.scale(scale)

        element.draw()
    }
}

class Header(val elements : List[HeaderElement]) {
    
    def update(day : Int, updatedValues : List[Double], updatedColors : List[Color]) {

        if (updatedValues.length != updatedColors.length) {
            throw new RuntimeException("ERROR updatedValues.length must be equal to updatedColors.length!")
        }
        
        for (e <- 0 to updatedValues.length - 1) {
            
            elements(e).update(day, updatedValues(e), updatedColors(e))
            
        }
    }
    
}

class Africa(val faunaSize : Int, val waterSourcesSize : Int, val icon : Picture, var position : (Int, Int)) extends Drawable {

    var fauna = new Fauna(faunaSize)

    var waterSources = new WaterSources(waterSourcesSize)

    var temperatures = Seq.fill(DAYS_IN_YEAR)(30.0)

    var header = new Header(
        List(
            new HeaderElement("Day", 0, (500, 1400), BLUE_COLOR, HEADER_TEXT_SCALE_FACTOR),
            new HeaderElement("Temperature", 0, (250, 1300), RED_COLOR, HEADER_TEXT_SCALE_FACTOR),
            new HeaderElement("Fauna count", 0, (350, 1215), GREEN_COLOR, HEADER_TEXT_SCALE_FACTOR)
        )
    )

    // LinkedHashMap rather than HashMap so as records can be shuffled
    // Shuffling a HashMap record yould require to convert to list first, since in a Set the order does NOT count, hence shuffling does NOT make any sense 
    // int --> day index
    // inner map --> actual water source <----> animal association, for that given day
    var animalsWaterSourcesMapAcrossYears = new LinkedHashMap[
        Int, 
        LinkedHashMap[
            Animal, WaterSource
        ]
    ]
    
    var waterSourcesAnimalsMapAcrossYears = new LinkedHashMap[
        Int, 
        LinkedHashMap[
            WaterSource, 
            ListBuffer[Animal]
        ]
    ]
    
    override def toString() : String = {
        return "*** Africa ***\n\n" + "\t number of animals: " + faunaSize + "\n\t animals: " + fauna.toString() + "\t number of water sources: " + waterSourcesSize + "\n\t water sources: " + waterSources.toString() + "\n\n**************"
    }

    def populateAnimalsWaterSourcesMap() {

        var animalsWaterSourcesMap = new LinkedHashMap[Animal, WaterSource]
        var waterSourcesAnimalsMap = new LinkedHashMap[WaterSource, ListBuffer[Animal]]

        fauna.fauna.foreach(
            a => {

                val ws = waterSources.getRandomWaterSource()
                
                animalsWaterSourcesMap(a) = ws

                    try {
                       
                       waterSourcesAnimalsMap(ws) += a
                    
                    } catch {
                        
                        case e: NoSuchElementException => {
                            
                            waterSourcesAnimalsMap.put(ws, new ListBuffer[Animal])
                            waterSourcesAnimalsMap(ws) += a
                       
                        }
                        
                    }
                
                a.position = (
                    ws.position._1 + getRandomShift(), 
                    ws.position._2 + getRandomShift()
                )
            }
        )
        for (i <- 1 to DAYS_IN_YEAR) {

            animalsWaterSourcesMapAcrossYears(i) = new LinkedHashMap[
                Animal, WaterSource
            ]
            waterSourcesAnimalsMapAcrossYears(i) = new LinkedHashMap[
                WaterSource, ListBuffer[Animal]
            ]
            
            fauna.fauna.foreach(
                a => {

                    animalsWaterSourcesMapAcrossYears(i) += ((a, animalsWaterSourcesMap(a)))

                    try {
                       waterSourcesAnimalsMapAcrossYears(i)(animalsWaterSourcesMap(a)) += a
                    } catch {
                        case e: NoSuchElementException => {
                            waterSourcesAnimalsMapAcrossYears(i).put(animalsWaterSourcesMap(a), new ListBuffer[Animal])
                            waterSourcesAnimalsMapAcrossYears(i)(animalsWaterSourcesMap(a)) += a
                        }   
                    }   
                }
            )
        }
    }

    populateAnimalsWaterSourcesMap()

    def updateAnimalsWaterSourcesMap(dayZeroBased : Int, animal : Animal, wsToMigrateTo : WaterSource) {
        for (i <- dayZeroBased to DAYS_IN_YEAR) {
           //println("\tapplying migration for day: " + i)
           animalsWaterSourcesMapAcrossYears(i)(animal) = wsToMigrateTo
    
           try {
               waterSourcesAnimalsMapAcrossYears(i)(wsToMigrateTo) += animal
            } catch {
                case e: NoSuchElementException => {
                    waterSourcesAnimalsMapAcrossYears(i).put(wsToMigrateTo, new ListBuffer[Animal])
                    waterSourcesAnimalsMapAcrossYears(i)(wsToMigrateTo) += animal
                }
            }                                                                 
        } 
    }

    def getColorChannelsInFunctionOfValue(
            startingValue : Int, 
            multiplier : Int, 
            value : Double, 
            r : Boolean, 
            g : Boolean, 
            b : Boolean
    ) : Color = {
        val color = new ValueInRange(0, 255, 0)

        color.setValue(startingValue + multiplier * value)

        return Color(
            if (r) color.value.toInt else 0,
            if (g) color.value.toInt else 0,
            if (b) color.value.toInt else 0
        )
    }

    def simulation() {
        
        clear()

        setBackground(BACKGROUND_COLOR)

        icon.draw()

        fauna.drawInCanvas(
            waterSources.waterSources_new.map{
                ws => ws._2.icon
            }.toList
        )
        
        waterSources.drawInCanvas()

        animalsWaterSourcesMapAcrossYears.keys.foreach(
            day => {

                val dayZeroBased = day - 1
                val previousDay = dayZeroBased - (if ( dayZeroBased == 0 ) 0 else 1 )

                temperatures(dayZeroBased) = temperatures(
                    previousDay
                ) * TEMPERATURE_YEARLY_MULTIPLICATIVE_FACTOR

                header.update(
                    day, 
                    List(
                        day, 
                        (Math.floor(temperatures(dayZeroBased) * 100) / 100),
                        fauna.faunaCount
                    ), 
                    List(
                        BLUE_COLOR,
                        getColorChannelsInFunctionOfValue(
                            RED_COLOR_CHANNEL, 
                            20,
                            temperatures(dayZeroBased), 
                            true, 
                            false, 
                            false
                        ),
                        getColorChannelsInFunctionOfValue(
                            0, 
                            20, 
                            fauna.faunaCount, 
                            false, 
                            true, 
                            true
                       )
                    )
                )

                Random.shuffle(animalsWaterSourcesMapAcrossYears(day)).foreach(
                    association => {

                         if ( association._1.isAlive(previousDay) ) {

                            val desired_water = association._1.getDesiredWater()
    
                            val actual_water = association._2.removeWater(
                                dayZeroBased, desired_water, true
                            )

                            if ( association._1.evaluateMigration(actual_water, desired_water) ) {
                      
                                val wsToMigrateTo = waterSources.getRandomWaterSource(association._2)
                                
                                association._1.migrate(wsToMigrateTo)

                                updateAnimalsWaterSourcesMap(dayZeroBased, association._1, wsToMigrateTo)

                            }
    
                            association._1.drinkWater(dayZeroBased, actual_water)

                            association._1.feelTemperature(dayZeroBased, temperatures(dayZeroBased))

                            var hasDied = association._1.updateLifePoints(
                                dayZeroBased, 
                                association._1.countEncounteredRivals(
                                    waterSourcesAnimalsMapAcrossYears(day)(association._2)
                                )
                            )

                            if (hasDied) {
                                fauna.faunaCount -= 1
                            }
                        }
                    }
                )
                
                Thread.sleep(DELAY_MS)
     
            }
        )

    }

}

object AfricaParams {
  val numOfAnimals = Random.between(MIN_NUM_OF_ANIMALS, MAX_NUM_OF_ANIMALS)
  val numOfWaterSources = 3 // keep it this way, for testing purposes
  val iconFilePath = BACKGROUND_FOLDER_PATH + "africaClean.png"
  val position = (0, 0)
}

var africa = new Africa(
    AfricaParams.numOfAnimals, 
    AfricaParams.numOfWaterSources, 
    Picture.image(AfricaParams.iconFilePath),
    AfricaParams.position
)

africa.simulation()

println("DONE")















