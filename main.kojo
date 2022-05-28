import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.collection.mutable.Map

val DAYS_IN_YEAR = 3

val MAX_TEMP_LOWERBOUND = 20
val MAX_TEMP_UPPERBOUND = 40

val MIN_WATER_LOWERBOUND = 1.0
val MIN_WATER_UPPERBOUND = 3.5

val MIN_NUM_OF_ANIMALS = 1
val MAX_NUM_OF_ANIMALS = 4

val ANIMAL_SPECIES = List("Lion", "Elephant", "Zebra")

object LionParams {
  val maxTemp = 40
}

object ElephantParams {
  val maxTemp = 45
}

object ZebraParams {
  val maxTemp = 45
}

trait Animal {

    val maxTemp: Double

    var lifePoints = List.fill(DAYS_IN_YEAR)(100)

    def stillAlive(currentTemp: Double)

}

class Lion(val maxTemp : Double, val id : Int) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Lion, maxTemp: " + maxTemp + ", lifePoints: " + lifePoints + "\n"
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Lion" + currentTemp)
    }
}

class Elephant(val maxTemp : Double, val id : Int) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Elephant, maxTemp: " + maxTemp + ", lifePoints: " + lifePoints + "\n"
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Elephant" + currentTemp)
    }
}

class Zebra(val maxTemp : Double, val id : Int) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Zebra, maxTemp: " + maxTemp + ", lifePoints: " + lifePoints + "\n"
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Zebra" + currentTemp)
    }
}

class Animals(val numOfAnimals : Int) {
    
    var animals: ListBuffer[Animal] = ListBuffer()

    def populateAnimals(): ListBuffer[Animal] = {
        
        for (i <- 1 to numOfAnimals) {
            
            val animalSpecie = ANIMAL_SPECIES(Random.between(0, ANIMAL_SPECIES.length))
        
            animalSpecie match {
        
                case "Lion" => {
                    animals += new Lion(LionParams.maxTemp, i)
                }
        
                case "Elephant" => {
                    animals += new Elephant(ElephantParams.maxTemp, i)
                }
        
                case "Zebra" => {
                    animals += new Zebra(ZebraParams.maxTemp, i)
                }
        
                case default => {
                    animals += new Zebra(ZebraParams.maxTemp, i)
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
}

val MIN_NUM_OF_WATER_SOURCES = 2
val MAX_NUM_OF_WATER_SOURCES = 4

val MAX_WATER_SOURCE_LEVEL = 10

val MAX_LEVEL_LOWERBOUND = 2
val MAX_LEVEL_UPPERBOUND = 4

val WATER_SOURCES_TYPES = List("Lake", "River")

trait WaterSource {
    val maxLevel : Double
    var currentLevel = List.fill(DAYS_IN_YEAR)(maxLevel)
    val id : Int
}

class Lake(val maxLevel : Double, val radius : Double, val id : Int) extends WaterSource {


    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Lake, currentLevel: " + currentLevel + ", maxLevel: " + maxLevel + "\n"
    }
}

class River(val maxLevel : Double, val area : Double, val shape : String, val id : Int) extends WaterSource {


    override def toString() : String = {
        return "\n\t\tID: " + id + " --> River, currentLevel: " + currentLevel + ", maxLevel: " + maxLevel + "\n"
    }
}

class WaterSources(val numOfWaterSources : Int) {
    
    var waterSources : ListBuffer[WaterSource] = ListBuffer()

    def populateWaterSources() {
    
        for (i <- 1 to numOfWaterSources) {
            
            val waterSourceType = WATER_SOURCES_TYPES(Random.between(0, WATER_SOURCES_TYPES.length))
            //println("waterSourceType: " + waterSourceType)
            
            val maxLevel = Random.between(MAX_LEVEL_LOWERBOUND, MAX_LEVEL_UPPERBOUND)
            //println("maxLevel: " + maxLevel)
            
            val radiusOrArea = Random.between(0, 1)

            waterSourceType match {
        
                case "Lake" => {
                    waterSources += new Lake(maxLevel, radiusOrArea, i)
                    //println(waterSources)
                }

                case "River" => {
                    waterSources += new River(maxLevel, radiusOrArea, "L", i)
                    //println(waterSources)
                }

                case default => {
                    waterSources += new Lake(maxLevel, radiusOrArea, i)
                    //println(waterSources)
                }

            }
        }
    }

    populateWaterSources()
    
    override def toString() : String = {
        var out = ""
    
        waterSources.foreach(
            ws => {
                out += ws.toString()
            }
        )
    
        return out

    }
}

class Africa(val numOfAnimals : Int, val numOfWaterSources : Int) {

    var animals = new Animals(numOfAnimals)

    var waterSources = new WaterSources(numOfWaterSources)

    var animalsWaterSourcesMap = new HashMap[Animal, WaterSource]

    override def toString() : String = {
        return "*** Africa ***\n\n" + "\t number of animals: " + numOfAnimals + "\n\t animals: " + animals.toString() + "\t number of water sources: " + numOfWaterSources + "\n\t water sources: " + waterSources.toString() + "\n\n**************"
    }
}

val numOfAnimals = Random.between(MIN_NUM_OF_ANIMALS, MAX_NUM_OF_ANIMALS)

//val numOfWaterSources = Random.between(MIN_NUM_OF_WATER_SOURCES, MAX_NUM_OF_WATER_SOURCES)
val numOfWaterSources = 1 // keep it this way, for testing purposes

var africa = new Africa(numOfAnimals, numOfWaterSources)

println(africa)



