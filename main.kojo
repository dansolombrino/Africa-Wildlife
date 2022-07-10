import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.collection.mutable.Map

//TODO make animal extend drawable

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
}

object ElephantParams {
  val maxfeltTemperature = 45;
  val minWater = 2
}

object ZebraParams {
  val maxfeltTemperature = 45;
  val minWater = 2
}

trait Animal extends Drawable {

    val maxfeltTemperature : Double
    val minWater : Double
    val id : Int
    //var icon : Picture
    //var position = (0, 0)
    var daysWithoutSatisfiedNeeds = 0
    var rival = ""
    
    var lifePoints = scala.collection.mutable.Seq.fill(DAYS_IN_YEAR)(1.0)
    var drankWater = scala.collection.mutable.Seq.fill(DAYS_IN_YEAR)(0.0)
    var feltTemperature = scala.collection.mutable.Seq.fill(DAYS_IN_YEAR)(0.0)

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

    def TODO_REMOVE_ME() {
        icon.setPosition(position._1, position._2)
        icon.draw()  
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
        var NUM_STEPS = 5
        

        println("CurrentPosition: " + this.icon.position)
        println("TargetPosition: " + ws.position)

        var absDist = (ws.position._1 - this.icon.position.x, ws.position._2 - this.icon.position.y)
        println("absDistance: " + absDist)

        var step_x = absDist._1 / NUM_STEPS
        var step_y = absDist._2 / NUM_STEPS

        for (i <- 1 to NUM_STEPS) {
            println("step: " + i)
            icon.translate(step_x,step_y)
            Thread.sleep(DELAY_MS)
            if (icon.collidesWith(ws.icon)) {
                println("COLLISION DETECTED, APPLYING OFFSET!")

                var randShiftX = Random.between(17, 40) 
                var randShiftY = Random.between(17, 40)

                randShiftX *= (if (Random.between(0, 2) == 1) -1 else 1)
                randShiftY *= (if (Random.between(0, 2) == 1) -1 else 1)
                
                icon.translate(randShiftX, randShiftY)
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

class Animals(val numOfAnimals : Int) {
    
    var animals: ListBuffer[Animal] = ListBuffer()

    def populateAnimals(): ListBuffer[Animal] = {
        
        for (i <- 1 to numOfAnimals) {
            
            val animalSpecie = ANIMAL_SPECIES(Random.between(0, ANIMAL_SPECIES.length))
        
            animalSpecie match {
        
                case "Lion" => {
                    animals += new Lion(
                        LionParams.maxfeltTemperature, 
                        LionParams.minWater, 
                        i, 
                        Picture.image("/home/dansolombrino/GitHub/Africa-Wildlife/icons/lion_64.png"), 
                        (0, 0)
                    )
                }
        
                case "Elephant" => {
                    animals += new Elephant(
                        ElephantParams.maxfeltTemperature, 
                        ElephantParams.minWater, 
                        i, 
                        Picture.image("/home/dansolombrino/GitHub/Africa-Wildlife/icons/elephant_64.png"),
                        (0, 0)
                    )
                }
        
                case "Zebra" => {
                    animals += new Zebra(
                        ZebraParams.maxfeltTemperature, 
                        ZebraParams.minWater, 
                        i, 
                        Picture.image("/home/dansolombrino/GitHub/Africa-Wildlife/icons/zebra_64.png"),
                        (0, 0)
                    )
                }
        
                case default => {
                    animals += new Zebra(
                        ZebraParams.maxfeltTemperature, 
                        ZebraParams.minWater, 
                        i, 
                        Picture.image("/home/dansolombrino/GitHub/Africa-Wildlife/icons/zebra_64.png"),
                        (0, 0)
                    )
                }
            }
        }
    
        return animals
    }

    populateAnimals()

    override def toString() : String = {
        var out = ""

        animals.foreach(
            a => {
                out += a.toString()
            }
        )

        return out
 
    }

    def getAnimal(i: Int) : Animal = {
        return animals(i)
    }

    def draw() {
        println("drawing animals...")

        animals.foreach(
            a => {
                a.draw()
            }
        )
    }
}

val waterColor = color(88, 148, 245)
val WATER_COLOR = color(88, 148, 245)

val MIN_NUM_OF_WATER_SOURCES = 3
val MAX_NUM_OF_WATER_SOURCES = 4

val MAX_WATER_SOURCE_LEVEL = 10

val MAX_LEVEL_LOWERBOUND = 3
val MAX_LEVEL_UPPERBOUND = 4

val WATER_SOURCES_TYPES = List("Lake", "River")

trait Drawable {
    var position : (Int, Int)
    val icon : Picture

    def draw() {
        icon.setPosition(position._1, position._2)
        icon.draw()
    }
}

trait DrawableShape extends Drawable {
    
    val borderColor : Color
    val innerColor : Color
    val rotation : Int
    val thickness : Int

    override def draw() {
        val tempIcon = trans(position._1, position._2) * penColor(borderColor) * fillColor(innerColor) * rot(rotation) * penThickness(thickness) -> icon
        tempIcon.draw()
    }
}


trait WaterSource extends DrawableShape {
    val maxLevel : Double
    var currentLevel = scala.collection.mutable.Seq.fill(DAYS_IN_YEAR)(maxLevel)
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

    def getRandomWaterSource(exclude : String) : WaterSource = {
        
        var waterSourcesListTempCopy = ListBuffer("Chad", "Victoria", "Niger")
        
        waterSourcesListTempCopy -= exclude
        
        return waterSources_new(waterSourcesListTempCopy(Random.between(0, numOfWaterSources - 1)))
    }


    def draw() {
        println("drawing waterSources...")

        waterSources_new.foreach(
            ws => {
                ws._2.draw()
            }
        )
    }
}

class Africa(val numOfAnimals : Int, val numOfWaterSources : Int, val icon : Picture) {

    var animals = new Animals(numOfAnimals)

    var waterSources = new WaterSources(numOfWaterSources)

    var feltTemperatures = scala.collection.mutable.Seq.fill(DAYS_IN_YEAR)(30.0)

    def populatefeltTemperatures() {
        for (i <- 1 to DAYS_IN_YEAR) {
            feltTemperatures(i) = feltTemperatures(i - 1) * 1.0125 
        }
    }

    // LinkedHashMap rather than HashMap so as we can shuffle records
    // Shuffling a HashMap record yould require to convert to list first, since in a Set the order does NOT count, hence shuffling does NOT make any sense 
    var animalsWaterSourcesMap = new scala.collection.mutable.LinkedHashMap[Int, scala.collection.mutable.LinkedHashMap[Animal, WaterSource]]
    var waterSourcesAnimalsMap = new scala.collection.mutable.LinkedHashMap[Int, scala.collection.mutable.LinkedHashMap[WaterSource, ListBuffer[Animal]]]
    
    
    override def toString() : String = {
        return "*** Africa ***\n\n" + "\t number of animals: " + numOfAnimals + "\n\t animals: " + animals.toString() + "\t number of water sources: " + numOfWaterSources + "\n\t water sources: " + waterSources.toString() + "\n\n**************"
    }

    def populateAnimalsWaterSourcesMap() {

        var animalsWaterSourcesMapGlobal = new scala.collection.mutable.LinkedHashMap[Animal, WaterSource]
        var waterSourcesAnimalsMapGlobal = new scala.collection.mutable.LinkedHashMap[WaterSource, ListBuffer[Animal]]

        animals.animals.foreach(
            a => {

                val ws = waterSources.getRandomWaterSource()
                
                animalsWaterSourcesMapGlobal(a) = ws
                //waterSourcesAnimalsMapGlobal(ws) += a

                    try {
                       waterSourcesAnimalsMapGlobal(ws) += a
                    } catch {
                        case e: NoSuchElementException => {
                            waterSourcesAnimalsMapGlobal.put(ws, new ListBuffer[Animal])
                            waterSourcesAnimalsMapGlobal(ws) += a
                        }
                    }
                
                
                var randShiftX = Random.between(-10, 50)
                var randShiftY = Random.between(-10, 50)
                a.position = (ws.position._1 + randShiftX, ws.position._2 + randShiftY)
            }
        )
        for (i <- 1 to DAYS_IN_YEAR) {

            animalsWaterSourcesMap(i) = new scala.collection.mutable.LinkedHashMap[Animal, WaterSource]
            waterSourcesAnimalsMap(i) = new scala.collection.mutable.LinkedHashMap[WaterSource, ListBuffer[Animal]]
            
            animals.animals.foreach(
                a => {
                    /*
                    var ws = waterSources.getRandomWaterSource()
                    
                    animalsWaterSourcesMap(i) += ((a, ws))

                    var randShiftX = Random.between(-10, 50)
                    var randShiftY = Random.between(-10, 50)
                    a.position = (ws.position._1 + randShiftX, ws.position._2 + randShiftY)
                    */

                    animalsWaterSourcesMap(i) += ((a, animalsWaterSourcesMapGlobal(a)))
                    //waterSourcesAnimalsMap(i) += ((animalsWaterSourcesMapGlobal(a), a))
                    //waterSourcesAnimalsMap(i)(animalsWaterSourcesMapGlobal(a)) += a

                    try {
                       waterSourcesAnimalsMap(i)(animalsWaterSourcesMapGlobal(a)) += a
                    } catch {
                        case e: NoSuchElementException => {
                            waterSourcesAnimalsMap(i).put(animalsWaterSourcesMapGlobal(a), new ListBuffer[Animal])
                            waterSourcesAnimalsMap(i)(animalsWaterSourcesMapGlobal(a)) += a
                        }
                            
                            
                        
                    }
                   
                    
                    
                }
            )
           
        }
    }

    populateAnimalsWaterSourcesMap()

    def simulation() {
        //println("Simulation!")

        //africa.
        icon.draw()
        //africa.
        animals.draw()
        //africa.
        waterSources.draw()

        animalsWaterSourcesMap.keys.foreach(
            day => {
                val day_pic = Picture.text("Day: " + day)
                day_pic.setPosition(500, 1150)
                draw(day_pic)

                val dayZeroBased = day - 1

                val r = scala.util.Random

                //val feltTemperature_for_the_day = Random.between(30, 45)
                val feltTemperature_for_the_day = feltTemperatures(dayZeroBased)

                //println("Day (zero based): " + dayZeroBased + ", day: " + day +", feltTemperatureerature: " + feltTemperature_for_the_day)
                //println("Day (zero based): " + dayZeroBased)
                //println("Day             : " + day)
                //println(r.shuffle(animalsWaterSourcesMap(day)))

                r.shuffle(animalsWaterSourcesMap(day)).foreach(
                    association => {

                         if ( association._1.isAlive( dayZeroBased - (if ( dayZeroBased == 0 ) 0 else 1 ) ) ) {
                        //if ( association._1.lifePoints( dayZeroBased - (if ( dayZeroBased == 0 ) 0 else 1 ) ) > 0 ) {

                            //println("\tAnimal " + association._1.id + " is drinking from WaterSource " + association._2.id)
                            //println("\tAnimal " + association._1.id + " BEFORE drinking: ")
                            //println("\t" + association._1)
                            //println("\tWaterSource " + association._2.id + " BEFORE drinking: ")
                            //println(association._2)
    
                            // drink code
                            val desired_water = Random.between(association._1.minWater * 0.8, association._1.minWater)
                            //println("\tdesired_water: " + desired_water)
    
                            val actual_water = association._2.removeWater(dayZeroBased, desired_water)

                            if (actual_water < desired_water) {
                                association._1.daysWithoutSatisfiedNeeds += 1
                                //println("\tAnimal " + association._1.id + " did NOT get enough water.")
                                //println("\tAnimal " + association._1.id + " daysWithoutSatisfiedNeeds: " + association._1.daysWithoutSatisfiedNeeds)
                            }

                            // migration code begin

                            if (association._1.daysWithoutSatisfiedNeeds >= MAX_daysWithoutSatisfiedNeeds) {
                                //println("\tAnimal " + association._1.id + " MIGRATING")

                                //println("original water source: " + association._2.name + ", " + association._2.position)
                                //val wsToMigrateTo = waterSources.getRandomWaterSource(association._2.name)
                                val wsToMigrateTo = waterSources.getRandomWaterSource(association._2)
                                //println("new water source: " + wsToMigrateTo.name + ", " + wsToMigrateTo.position)
                                
                                association._1.migrate(wsToMigrateTo)

                                for (i <- dayZeroBased to DAYS_IN_YEAR) {
                                   //println("\tapplying migration for day: " + i)
                                   animalsWaterSourcesMap(i)(association._1) = wsToMigrateTo

                                   try {
                                       waterSourcesAnimalsMap(i)(wsToMigrateTo) += association._1
                                    } catch {
                                        case e: NoSuchElementException => {
                                            waterSourcesAnimalsMap(i).put(wsToMigrateTo, new ListBuffer[Animal])
                                            waterSourcesAnimalsMap(i)(wsToMigrateTo) += association._1
                                        }
                                            
                                            
                                        
                                    }
                                   
                                                             
                                }
                                

                                /*
                                var randShiftX = Random.between(-10, 50)
                                var randShiftY = Random.between(-10, 50)
                                association._1.icon.setPosition(
                                    association._2.position._1 + randShiftX, 
                                    association._2.position._2 + randShiftY
                                )
                                */
                                
                                association._1.daysWithoutSatisfiedNeeds = 0
                            }
                        
                            // migration code end
    
                            association._1.drinkWater(dayZeroBased, actual_water)
    
                            association._1.feltTemperature(dayZeroBased) = feltTemperature_for_the_day

                            //println("SUS BEGIN")
                            val numEncounteredRivals = association._1.countEncounteredRivals(waterSourcesAnimalsMap(day)(association._2))
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
                day_pic.erase()
            }
        )
        
        
        
    }

}

val numOfAnimals = Random.between(MIN_NUM_OF_ANIMALS, MAX_NUM_OF_ANIMALS)

//val numOfWaterSources = Random.between(MIN_NUM_OF_WATER_SOURCES, MAX_NUM_OF_WATER_SOURCES)
val numOfWaterSources = 3 // keep it this way, for testing purposes

var africa = new Africa(numOfAnimals, numOfWaterSources, Picture.image("/home/dansolombrino/GitHub/Africa-Wildlife/africaClean.png"))

//println(africa)

println("\n\n")
println("********** animalsWaterSourcesMap **********")
println("\n")
//println(africa.animalsWaterSourcesMap)
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















