import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

val MAX_TEMP_LOWERBOUND = 20
val MAX_TEMP_UPPERBOUND = 40

val MIN_WATER_LOWERBOUND = 1.0
val MIN_WATER_UPPERBOUND = 3.5

val MIN_NUM_OF_ANIMALS = 1
val MAX_NUM_OF_ANIMALS = 4

val MIN_NUM_OF_WATER_SOURCES = 1
val MAX_NUM_OF_WATER_SOURCES = 2

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

    def stillAlive(currentTemp: Double)

}

class Lion(val maxTemp : Double, val id : Int) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Lion, maxTemp: " + maxTemp + "\n"
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Lion" + currentTemp)
    }
}

class Elephant(val maxTemp : Double, val id : Int) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Elephant, maxTemp: " + maxTemp + "\n"
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Elephant" + currentTemp)
    }
}

class Zebra(val maxTemp : Double, val id : Int) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Zebra, maxTemp: " + maxTemp + "\n"
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Zebra" + currentTemp)
    }
}

class WaterSource(val maxLevel : Double, val currentLevel : Double, val id : Int) {
    
}

class Africa(val numOfAnimals : Int, val numOfWaterSources : Int) {
    
    var animals: ListBuffer[Animal] = ListBuffer()

    def populate_animals(): ListBuffer[Animal] = {
        
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

    populate_animals()

    override def toString() : String = {
        return "*** Africa ***\n\n" + "\t number of animals: " + numOfAnimals + 
            "\n\t animals: " + animals.toString() + "\n\n**************"
    }
}

val numOfAnimals = Random.between(MIN_NUM_OF_ANIMALS, MAX_NUM_OF_ANIMALS)
val numOfWaterSources = Random.between(MIN_NUM_OF_WATER_SOURCES, MAX_NUM_OF_WATER_SOURCES)
var africa = new Africa(numOfAnimals, numOfWaterSources)
println(africa)



