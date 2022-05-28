import scala.util.Random
import scala.collection.mutable.ListBuffer

val MAX_TEMP_LOWERBOUND = 20
val MAX_TEMP_UPPERBOUND = 40

val MIN_WATER_LOWERBOUND = 1.0
val MIN_WATER_UPPERBOUND = 3.5

val MIN_NUMBER_OF_ANIMALS = 1
val MAX_NUMBER_OF_ANIMALS = 4

abstract class Animal(maxTemp: Double, minWater: Double){
    
 
}

class Lion(maxTemp: Double, minWater: Double, id: Int) extends Animal(maxTemp, minWater) {
    
    override def toString() : String = {
      
        return "ID: " + id + " --> Lion" + ", maxTemp: " + maxTemp + ", minWater: " + minWater
    }
    
}

class Elephant(maxTemp: Double, minWater: Double, id: Int) extends Animal(maxTemp, minWater) {
    
    override def toString() : String = {
      
        return "ID: " + id + " --> Elephant" + ", maxTemp: " + maxTemp + ", minWater: " + minWater
    }
    
}

class Zebra(maxTemp: Double, minWater: Double, id: Int) extends Animal(maxTemp, minWater) {
    
    override def toString() : String = {
      
        return "ID: " + id + " --> Zebra" + ", maxTemp: " + maxTemp + ", minWater: " + minWater
    }
    
}

class Bull(maxTemp: Double, minWater: Double, id: Int) extends Animal(maxTemp, minWater) {
    
    override def toString() : String = {
      
        return "ID: " + id + " --> Lion" + ", maxTemp: " + maxTemp + ", minWater: " + minWater
    }
    
}

val animalSpecies = List("Lion", "Elephant", "Zebra", "Bull")

val numberOfAnimals = Random.between(MIN_NUMBER_OF_ANIMALS, MAX_NUMBER_OF_ANIMALS)
println("numberOfAnimals: " + numberOfAnimals)

var animals: ListBuffer[Animal] = ListBuffer()

for (i <- 1 to numberOfAnimals) {

    val animalSpecie = animalSpecies(Random.between(0, animalSpecies.length))

    animalSpecie match {
        
        case "Lion" => {

            println("match Lion") 

            animals += new Lion(i, 40, 2)
        }
        case "Elephant" => {
            
            println("match Elephant") 

            animals += new Elephant(i, 40, 2)
        }
        case "Zebra" => {
            
            println("match Zebra") 
            
            animals += new Zebra(i, 40, 2)
        }
        case "Bull" => {
            
            println("match Bull") 

            animals += new Bull(i, 40, 2)
        }
        case default => {
            println("don't know what " + default + " is...")
        }
    }
   
}




























