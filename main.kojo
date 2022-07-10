import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap
import scala.language.postfixOps
import scala.collection.mutable.Map
import scala.collection.mutable.Seq

val YEAR_TEXT_SCALE_FACTOR = 5

var MIGRATION_NUM_VISUAL_STEPS = 5

var ICON_FOLDER_PATH = "/home/dansolombrino/GitHub/Africa-Wildlife/icons/"

// TODO load from disk
val TEMPERATURE_YEARLY_MULTIPLICATIVE_FACTOR = 1.025

val MAX_daysWithoutSatisfiedNeeds = 2

val DAYS_IN_YEAR = 5
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

    def updateLifePoints(day : Int) {
            lifePoints(day) = 50 * drankWater(day) - 1 * feltTemperature(day)
    }

    def updateLifePoints(day : Int, numEncounteredRivals : Int) {
            lifePoints(day) = 50 * drankWater(day) - 1 * feltTemperature(day) - 10 * numEncounteredRivals
    }

    def die(day : Int) {
        for (i <- day to DAYS_IN_YEAR - 1) {
            lifePoints(i) = -1
        }

        icon.erase()
    }

    /*
    def TODO_REMOVE_ME() {
        icon.setPosition(position._1, position._2)
        icon.draw()  
    }
    */

    def handleNeedsSatisfaction(actual_water : Double, desired_water : Double) {
        if (actual_water < desired_water) {
            daysWithoutSatisfiedNeeds += 1
            //println("\tAnimal " + association._1.id + " did NOT get enough water.")
            //println("\tAnimal " + association._1.id + " daysWithoutSatisfiedNeeds: " + association._1.daysWithoutSatisfiedNeeds)
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

        //println(this.getClass.getSimpleName + "encountersRival: " + (sum > 0))
        //println("neighbouringAnimals: " + neighbouringAnimals)

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
            
            println("step: " + i)
            icon.translate(step_x,step_y)
            Thread.sleep(DELAY_MS)
            
            if (icon.collidesWith(ws.icon)) {
                println("COLLISION DETECTED, APPLYING OFFSET!")
                
                icon.translate(getRandomShift(), getRandomShift())
            }
            
        }
        
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

    def drawInCanvas() {
        //println("drawing animals...")

        fauna.foreach(
            a => {
                a.drawInCanvas()
            }
        )
    }
}

val waterColor = color(88, 148, 245)
val WATER_COLOR = color(88, 148, 245)
val DEEP_BLUE_COLOR = color(7, 42, 108)

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

    def drawInCanvas() {
        icon.setPosition(position._1, position._2)
        icon.draw()
    }
}

trait DrawableShape extends Drawable {
    
    val borderColor : Color
    val innerColor : Color
    val rotation : Int
    val thickness : Int

    override def drawInCanvas() {
        
        val tempIcon = trans(
            position._1, position._2
        ) * penColor(
            borderColor
        ) * fillColor(
            innerColor
        ) * rot(
            rotation
        ) * penThickness(
            thickness
        ) -> icon
        
        tempIcon.draw()

        
    }
}


trait WaterSource extends DrawableShape {
    val maxLevel : Double
    var currentLevel = Seq.fill(DAYS_IN_YEAR)(maxLevel)
    val id : Int
    val name : String

    def removeWater(day: Int, water: Double) : Double = {

        if (currentLevel(day) - water >= 0) {
            currentLevel(day) -= water

            return water
        } else {
            val maxAvailableWater = currentLevel(day)
            
            currentLevel(day) = 0

            return maxAvailableWater
        }
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

    /*
    def getRandomWaterSource(exclude : String) : WaterSource = {
        
        var waterSourcesListTempCopy = ListBuffer("Chad", "Victoria", "Niger")
        
        waterSourcesListTempCopy -= exclude
        
        return waterSources_new(waterSourcesListTempCopy(Random.between(0, numOfWaterSources - 1)))
    }
    */

    def drawInCanvas() {
        //println("drawing waterSources...")

        waterSources_new.foreach(
            ws => {
                ws._2.drawInCanvas()
            }
        )
    }
}

class Africa(val faunaSize : Int, val numOfWaterSources : Int, val icon : Picture, var position : (Int, Int)) extends Drawable {

    var fauna = new Fauna(numOfAnimals)

    var waterSources = new WaterSources(numOfWaterSources)

    var temperatures = Seq.fill(DAYS_IN_YEAR)(30.0)

    var dayText = Picture.text(" ")

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
        return "*** Africa ***\n\n" + "\t number of animals: " + faunaSize + "\n\t animals: " + fauna.toString() + "\t number of water sources: " + numOfWaterSources + "\n\t water sources: " + waterSources.toString() + "\n\n**************"
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

    def handleDisplayDayText(day : Int) {
        if (day > 1) {
            dayText.erase()
        }
        
        dayText = Picture.text("Day: " + day)
        dayText.setPosition(500, 1300)
        dayText.setPenColor(DEEP_BLUE_COLOR)
        dayText.scale(YEAR_TEXT_SCALE_FACTOR)
        draw(dayText)
    }

    def simulation() {
        //println("Simulation!")

        icon.draw()

        fauna.drawInCanvas()
        
        waterSources.drawInCanvas()

        animalsWaterSourcesMapAcrossYears.keys.foreach(
            day => {

                handleDisplayDayText(day)

                val dayZeroBased = day - 1

                temperatures(dayZeroBased) = temperatures(
                    dayZeroBased - (if ( dayZeroBased == 0 ) 0 else 1 )
                ) * TEMPERATURE_YEARLY_MULTIPLICATIVE_FACTOR

                //println("Day (zero based): " + dayZeroBased)
                //println("Temperature     : " + temperatures(dayZeroBased))
                //println("Day             : " + day)

                Random.shuffle(animalsWaterSourcesMapAcrossYears(day)).foreach(
                    association => {

                         if ( association._1.isAlive( dayZeroBased - (if ( dayZeroBased == 0 ) 0 else 1 ) ) ) {
                      
                            val desired_water = Random.between(
                                association._1.minWater * 0.8, association._1.minWater
                            )
    
                            val actual_water = association._2.removeWater(
                                dayZeroBased, desired_water
                            )

                            //association._1.handleNeedsSatisfaction(actual_water, desired_water)

                            //association._1.evaluateMigration(actual_water, desired_water)

                            // migration code begin

                            //if (association._1.daysWithoutSatisfiedNeeds >= MAX_daysWithoutSatisfiedNeeds) {
                            if ( association._1.evaluateMigration(actual_water, desired_water) ) {
                                //println("\tAnimal " + association._1.id + " MIGRATING")

                                //println("original water source: " + association._2.name + ", " + association._2.position)
                                //val wsToMigrateTo = waterSources.getRandomWaterSource(association._2.name)
                                val wsToMigrateTo = waterSources.getRandomWaterSource(association._2)
                                //println("new water source: " + wsToMigrateTo.name + ", " + wsToMigrateTo.position)
                                
                                association._1.migrate(wsToMigrateTo)

                                for (i <- dayZeroBased to DAYS_IN_YEAR) {
                                   //println("\tapplying migration for day: " + i)
                                   animalsWaterSourcesMapAcrossYears(i)(association._1) = wsToMigrateTo

                                   try {
                                       waterSourcesAnimalsMapAcrossYears(i)(wsToMigrateTo) += association._1
                                    } catch {
                                        case e: NoSuchElementException => {
                                            waterSourcesAnimalsMapAcrossYears(i).put(wsToMigrateTo, new ListBuffer[Animal])
                                            waterSourcesAnimalsMapAcrossYears(i)(wsToMigrateTo) += association._1
                                        }
                                       
                                    }                                                                 
                                } 
                                association._1.daysWithoutSatisfiedNeeds = 0
                            }
                        
                            // migration code end
    
                            association._1.drinkWater(dayZeroBased, actual_water)
    
                            //association._1.feltTemperature(dayZeroBased) = feltTemperature_for_the_day
                            association._1.feltTemperature(dayZeroBased) = temperatures(dayZeroBased)

                            //println("SUS BEGIN")
                            val numEncounteredRivals = association._1.countEncounteredRivals(waterSourcesAnimalsMapAcrossYears(day)(association._2))
                            //println("SUS END")
                            
                            association._1.updateLifePoints(dayZeroBased, numEncounteredRivals)
                            
   
                            //println("\tAnimal " + association._1.id + " AFTER drinking: ")
                            //println("\t" + association._1)
                            //println("\tWaterSource " + association._2.id + " AFTER drinking: ")
                            //println(association._2)
    
                            //println("\n\n\n\n")

                           
                        } else {
                            //println("\tAnimal " + association._1.id + " DIED :'( ")
                            association._1.die(dayZeroBased - 1)
                        }
                        
                        

                        

                        
                        
                    }
                )
        
                //println("sleeping for " + DELAY_MS + " ms")
                Thread.sleep(DELAY_MS)
                //println("waking up after sleeping for " + DELAY_MS + " ms")
                
            }
        )
        
        
        
    }

}

val numOfAnimals = Random.between(MIN_NUM_OF_ANIMALS, MAX_NUM_OF_ANIMALS)

//val numOfWaterSources = Random.between(MIN_NUM_OF_WATER_SOURCES, MAX_NUM_OF_WATER_SOURCES)
val numOfWaterSources = 3 // keep it this way, for testing purposes

var africa = new Africa(
    numOfAnimals, 
    numOfWaterSources, 
    Picture.image("/home/dansolombrino/GitHub/Africa-Wildlife/africaClean.png"),
    (0, 0)
)

//println(africa)

println("\n\n")
println("********** animalsWaterSourcesMapAcrossYears **********")
println("\n")
//println(africa.animalsWaterSourcesMapAcrossYears)
println("\n")
println("********************************************")

clear()

val bgColor = color(200,235,255)
setBackground(bgColor)

africa.simulation()

println("DONE")

/*
africa.animals.animals.foreach(
    a => {
        println(a)
    }
)

*/















