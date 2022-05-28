import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

val MAX_TEMP_LOWERBOUND = 20
val MAX_TEMP_UPPERBOUND = 40

val MIN_WATER_LOWERBOUND = 1.0
val MIN_WATER_UPPERBOUND = 3.5

val MIN_NUMBER_OF_ANIMALS = 1
val MAX_NUMBER_OF_ANIMALS = 4

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
        return "ID: " + id + " --> Lion, maxTemp: " + maxTemp
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Lion" + currentTemp)
    }
}

class Elephant(val maxTemp : Double, val id : Int) extends Animal {

    override def toString() : String = {
        return "ID: " + id + " --> Elephant, maxTemp: " + maxTemp
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Elephant" + currentTemp)
    }
}

class Zebra(val maxTemp : Double, val id : Int) extends Animal {

    override def toString() : String = {
        return "ID: " + id + " --> Zebra, maxTemp: " + maxTemp
    }

    def stillAlive(currentTemp: Double) {
        println("stillAlive Zebra" + currentTemp)
    }
}




def populate(numberOfAnimals : Int): ListBuffer[Animal] = {
    
    var animals: ListBuffer[Animal] = ListBuffer()
    
    for (i <- 1 to numberOfAnimals) {
        
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

val numberOfAnimals = Random.between(MIN_NUMBER_OF_ANIMALS, MAX_NUMBER_OF_ANIMALS)
println("numberOfAnimals: " + numberOfAnimals)

var animals: ListBuffer[Animal] = populate(numberOfAnimals)

println(animals)



