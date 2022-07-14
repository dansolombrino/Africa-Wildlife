import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.language.postfixOps
import scala.collection.mutable.Map
import scala.collection.mutable.Seq

/**
 * PLEASE NOTE!
 * In order to use relative paths for assets and data, place this file and the 
 * "assets" folder in the same folder where Kojo SDK is installed.
 * Otherwise, replace relative paths with full paths.
 *
 * This is due to internal Kojo dynamics, out of my control.
 * 
 * Thank you, Daniele.
 */

// ---------------- BEGIN PARAMETERS CONFIGURATION ---------------------------//

/** Set of variables controlling simulation behavior */

// Number of years to run the simulation for 
var NUM_YEARS_TO_SIMULATE = 12

// Starting year of the simulation
var STARTING_YEAR = 1950

// Multiplicative factor which will be used in the formula
// to compute the temperature for any given year.
// See Animal class for more information regarding how such
// temperature is actually computed
var TEMPERATURE_YEARLY_MULTIPLICATIVE_FACTOR = 1.015

// Delay in ms between different years, in order to make graphical appearance
// more usable
val BETWEEN_YEARS_DELAY_MS = 1000

/* ************************************************  */


/** Set of variables controlling simulation graphical appearance */

// Relative path of folder containing animal icons, which will be shown
// in the canvas
val ICON_FOLDER_PATH = "./assets/icons/"

// Relative path of folder containing background image, which will be shown
// in the canvas
val BACKGROUND_FOLDER_PATH = "./assets/background/"

// Lower and upper bound for the random shift applied in getRandomShift method.
// See getRandomShift method for more details about how these values are used.
val RANDOM_SHIFT_MIN_VALUE = 25
val RANDOM_SHIFT_MAX_VALUE = 50

// Set of colors used througout the project
val WATER_COLOR = color(88, 148, 245)
val BLUE_COLOR  = color(0, 0, 255)
val RED_COLOR   = color(255, 0, 0)
val GREEN_COLOR = color(0, 255, 0)
val BACKGROUND_COLOR = color(200, 235, 255)

// Stores the value for the red channel of an RGB color.
// Used in the dynamic generation of the red color, when placing the 
// "temperature" information in the canvas.

// See Africa.getColorChannelsInFunctionOfValue method for more information 
// about how this parameter is used 
val RED_COLOR_CHANNEL = 175


/* ************************************************* */

/** Set of variables controlling Animals' characteristics */

// Lower and upper bound for the random number of animals that appear at the 
// beginning of the simulation.

// See AfricaParamas doc for more information on how these values are used.
val MIN_NUM_OF_ANIMALS = 12
val MAX_NUM_OF_ANIMALS = 15

// List of supported animal species.
// See Fauna.populateFauna method for more information about how this list is
// used
val ANIMAL_SPECIES_LIST = List("Lion", "Elephant", "Zebra")

// The following objects store some init configuration parameters for the 
// Animals that will be instanciated at simulation init time.

// This is just one of the possible ways to manage the situation.

// Hardcoding the parameters in class constructors was NOT considered due to the
// unelegance and unmaintainability of the choice (future extensibility is one
// of the requirement asked to students, see report for more info about it)

// This solution represents a good tradeoff, because it adds enough dinamicity 
// for future extensibility.
// For example, one could load this data from a JSON, use it to populate such 
// objects and then call the constructors passing data from these objects

object LionParams {
  val MAX_FELT_TEMPERATURE = 40;
  val MIN_WATER = 2
  val ICON_FILE_PATH = ICON_FOLDER_PATH + "lion_64.png"
}

object ElephantParams {
  val MAX_FELT_TEMPERATURE = 37;
  val MIN_WATER = 2
  val ICON_FILE_PATH = ICON_FOLDER_PATH + "elephant_64.png"
}

object ZebraParams {
  val MAX_FELT_TEMPERATURE = 36;
  val MIN_WATER = 2
  val ICON_FILE_PATH = ICON_FOLDER_PATH + "zebra_64.png"
}

/* ************************************************* */

/** Set of variables controlling WaterSources' characteristics */

// Lower and upper bound for the maximum random amount of water that any water 
// source has, at the beginning of the simulation.

// See WaterSources doc for more information on how these values are used.
val MAX_LEVEL_LOWERBOUND = 3
val MAX_LEVEL_UPPERBOUND = 4

// List of available identifiers for the available Water Sources

// See WaterSources.getRandomWaterSource method for more information about how
// this list is used.
val WATER_SOURCES_LIST = ListBuffer("Chad", "Victoria", "Niger")

/* *********************************************************** */

/** Set of variables controlling Africa characteristics */

object AfricaParams {
  val numOfAnimals = Random.between(MIN_NUM_OF_ANIMALS, MAX_NUM_OF_ANIMALS)
  val numOfWaterSources = 3 // keep it this way, for testing purposes
  val ICON_FILE_PATH = BACKGROUND_FOLDER_PATH + "africaClean.png"
  val position = (0, 0)
}

/** *************************************************** */


// ---------------- END PARAMETERS CONFIGURATION -----------------------------//


/**
 * Utility method to get a tuple (x and y coordinates) of random shifts
 */
def getRandomShift() : Int = {

    // The part after the * gets a random sign, either + or negative, in order
    // to make the shift positive or negative
    return Random.between(
        RANDOM_SHIFT_MIN_VALUE, RANDOM_SHIFT_MAX_VALUE
    ) * (if (Random.between(0, 2) == 1) -1 else 1)
}

/**
 * Stores properties for a drawable object, i.e. an object that can be placed
 * in the canvas.
 */
trait Drawable {
    
    // The position of the object in the canvas
    protected var position : (Int, Int)

    // The icon of the object that will be shown in the canvas
    protected val icon : Picture

    // Getter method for the position parameter
    def getPosition() : (Int, Int) = {
        return position
    }

    // Setter method for the position parameter
    def setPosition(position : (Int, Int)) {
       this.position = position
    }
    
    // Getter method for the icon parameter
    def getIcon() : Picture = {
        return icon
    }

    /**
     * Method to draw the object in the canvas.
     * 
     * Takes an (optional, if given empty) list of other pictures to check
     * collistion against.
     */
    def drawInCanvas(checkCollisionAgainst : List[Picture]) {
        
        // Setting the position of the icon in the canvas, via Kojo SDK function
        icon.setPosition(position._1, position._2)
        
        // Placing the icon in the canvas, via Kojo SDK function
        icon.draw()

        // If the input list is NOT empty, then checking whether the newly 
        // placed icon collides with any of the given Picture objects
        if (!checkCollisionAgainst.isEmpty) {
            
            // Iterating over all given pictures to check collision against
            checkCollisionAgainst.foreach(
                
                cca => {

                    // If collision happens
                    if (cca.collidesWith(icon)) {

                        // Then apply a random shift
                        icon.translate(getRandomShift(), getRandomShift())
                    }
                }
            )       
        }
    }
}

/**
 * Stores properties for a drawable shape, i.e. a shape that can be placed
 * in the canvas.
 * 
 * See Drawable doc for more info about it 
 */
trait DrawableShape extends Drawable {
    
    // Set of properties characterizing the shape to be drawn in the canvas
    protected val borderColor : Color
    protected val innerColor : Color
    protected val rotation : Int
    protected val thickness : Int
    protected var opacity = new ValueInRange(1.0, 1.0, 0.0)

    // Method to draw the object in the canvas.
    // Overrides method in super class, in order to provide a more specialized
    // implementation.
    override def drawInCanvas(checkCollisionAgainst : List[Picture]) {

        // First sets the properties of the shape to be drawn
        icon.setPenColor(borderColor)
        icon.setFillColor(innerColor)
        icon.setRotation(rotation)
        icon.setPenThickness(thickness)

        // Then calls the more general method in the super class
        // Typical OOP pattern used.
        super.drawInCanvas(checkCollisionAgainst)
    }
}

// Class modeling an Animal
trait Animal extends Drawable {

    // Set of parametrs used to determine animal life points.
    // See updateLifePoints method(s) for more info on how these parameters 
    // actually impact life points

    // Maximum temperature the animal can sustain, every year
    protected val maxToleratedTemperature : Double

    // Minimum quantity of water the animal needs, every year
    protected val minWater : Double

    // numeric ID of the animal, used mostly for debugging
    protected val id : Int
    
    // Storing animal's vital parameters for every year that is simulated.
    // See updateLifePoints method(s) for more info about how these params are
    // actually used

    // Animal life points, they model "how much alive" the animal is.
    // Basically, the more events affect the animal, the more the life points 
    // are detracted.
    protected var lifePoints = Seq.fill(NUM_YEARS_TO_SIMULATE)(1.0)

    // Stores animal's drank water, for every year in the simulation
    protected var drankWater = Seq.fill(NUM_YEARS_TO_SIMULATE)(0.0)

    // Stores what temperature the animal felt, in a given year
    protected var feltTemperature = Seq.fill(NUM_YEARS_TO_SIMULATE)(0.0)

    /**
     * Here is where, potentially, in future the project may be expanded even
     * more, by adding additional parameters.
     */

    // Determines whether the animal is alive, in a given year
    def isAlive(year : Int) : Boolean = {

        // Determining animal's aliveness boils down to check whether its life
        // points are greater than zero or not
        return lifePoints(year) > 0
    }

    // Makes the animal drink
    def drinkWater(year: Int, water: Double) {

        drankWater(year) += water
    }

    // Getter method for the ID field
    def getId() : Int = {
        
        return id
    
    }

    /**
     * Updates the life points of the animal, in a given year.
     * Returns true if the animal died, false otherwise
     *
     * This is basically the core of the simulation, being the function 
     * governing whether animals survive or not.
     * 
     * Every other property stored in the class has either a positive or a 
     * negative impact on the life points.
     *
     * This is the BASIC version, in which the update is in function of 2 
     * parameters: drank water and felt temperature
     */
    def updateLifePoints(year : Int) : Boolean = {

        // updating the life points according to drank water and the felt 
        // temperature

        // Drank water has a positive impact

        // felt temperature has a negative impact.
        // The exponential formula ensures:
        //   -high penalty, the more feltTemperature surpasses 
        //    maxToleratedTemperature
        //   -low penalty, the more feltTemperature is distant from 
        //    maxToleratedTemperature 
        lifePoints(year) += 50 * drankWater(year) - 6 * scala.math.pow(
            2, feltTemperature(year) - maxToleratedTemperature
        )

        // if life points reached a negative value
        if (lifePoints(year) < 0) {

            // then die
            die(year)

            // return true because animal died
            return true
        }

        // Animal survived, returning false
        return false
    }

    // Make the animal die
    def die(year : Int) {

        // Set life points to -1 in the years following the given one 
        for (i <- year to NUM_YEARS_TO_SIMULATE - 1) {
            lifePoints(i) = -1
        }

        // Remove animal from canvas
        icon.erase()
    }

    // Randomly decide how much water the animal wants to drink
    def getDesiredWater() : Double = {
        
        return Random.between(minWater * 0.8, minWater)
        
    }

    // Store felt temperature in a given year
    def feelTemperature(year : Int, temperatureValue : Double) {
        
        feltTemperature(year) = temperatureValue
        
    }
}

// Models a Lion.
// This offers future expandability, in case one would like to make the Lion 
// more specialized than the general abstract class
class Lion(
    protected val maxToleratedTemperature : Double, 
    protected val minWater : Double, 
    protected val id : Int, 
    protected val icon : Picture, 
    protected var position: (Int, Int)
) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Lion, maxToleratedTemperature: " + 
            maxToleratedTemperature + ", lifePoints: " + lifePoints + 
            ", drankWater: " + drankWater + ", feltTemperature: " + 
            feltTemperature + "\n"
    }
}

// Models a Elephant.
// This offers future expandability, in case one would like to make the Elephant 
// more specialized than the general abstract class
class Elephant(
    protected val maxToleratedTemperature : Double, 
    protected val minWater : Double, 
    protected val id : Int, 
    protected val icon : Picture,
    protected var position : (Int, Int)
) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Elephant, maxToleratedTemperature: " + 
            maxToleratedTemperature + ", lifePoints: " + lifePoints + 
            ", drankWater: " + drankWater + ", feltTemperature: " + 
            feltTemperature + "\n"
    }
}

// Models a Zebra.
// This offers future expandability, in case one would like to make the Zebra 
// more specialized than the general abstract class
class Zebra(
    protected val maxToleratedTemperature : Double, 
    protected val minWater : Double, 
    protected val id : Int, 
    protected val icon : Picture,
    protected var position : (Int, Int)
) extends Animal {

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Zebra, maxToleratedTemperature: " + 
            maxToleratedTemperature + ", lifePoints: " + lifePoints + 
            ", drankWater: " + drankWater + ", feltTemperature: " + 
            feltTemperature + "\n"
    }
}

// Models African fauna.
class Fauna(val faunaSize : Int) {

    // Number of alive animals
    var faunaCount = faunaSize
    
    // Actual fauna, i.e. a list of Animals
    var fauna: ListBuffer[Animal] = ListBuffer()

    // Populates the fauna
    def populateFauna() {
        
        for (i <- 1 to faunaSize) {

            // Get a random animal specie    
            val animalSpecie = ANIMAL_SPECIES_LIST(
                Random.between(0, ANIMAL_SPECIES_LIST.length)
            )

            // Init the specific object, according to randomly extracted animal
            // specie
            animalSpecie match {
        
                case "Lion" => {
                    fauna += new Lion(
                        LionParams.MAX_FELT_TEMPERATURE, 
                        LionParams.MIN_WATER, 
                        i, 
                        Picture.image(LionParams.ICON_FILE_PATH), 
                        (0, 0)
                    )
                }
        
                case "Elephant" => {
                    fauna += new Elephant(
                        ElephantParams.MAX_FELT_TEMPERATURE, 
                        ElephantParams.MIN_WATER, 
                        i, 
                        Picture.image(ElephantParams.ICON_FILE_PATH),
                        (0, 0)
                    )
                }
        
                case "Zebra" => {
                    fauna += new Zebra(
                        ZebraParams.MAX_FELT_TEMPERATURE, 
                        ZebraParams.MIN_WATER, 
                        i, 
                        Picture.image(ZebraParams.ICON_FILE_PATH),
                        (0, 0)
                    )
                }
        
                case default => {
                    fauna += new Zebra(
                        ZebraParams.MAX_FELT_TEMPERATURE, 
                        ZebraParams.MIN_WATER, 
                        i, 
                        Picture.image(ZebraParams.ICON_FILE_PATH),
                        (0, 0)
                    )
                }
            }
        }
    }

    // Scala way of calling methods inside a constructure... simply invoke the
    // method inside the class
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

    // Draw populated fauna in the canvas
    def drawInCanvas(checkCollisionAgainst : List[Picture]) {

        // Basically, invoke the draw method on every animal in the Fauna
        fauna.foreach(
            a => {
                a.drawInCanvas(checkCollisionAgainst)
            }
        )
    }
}

// Models a value that is constraint to be inside a specified range
class ValueInRange(
    protected var value : Double, 
    protected val max : Double, 
    protected val min : Double
) {
    
    def trimValueIntoRange() {
        if (value > max) {
            value = max
        }

        if (value < min) {
            value = min
        }
    }

    def getValue() : Double = {
        return value
    }
    
    def setValue(newValue : Double) {
        value = newValue

        trimValueIntoRange()
    }
}

// Models a water source
trait WaterSource extends DrawableShape {
    protected val maxLevel : Double
    protected var currentLevel = Seq.fill(NUM_YEARS_TO_SIMULATE)(maxLevel)
    protected val id : Int
    protected val name : String
    
    // Remove water from the water source
    def removeWater(
        year: Int, 
        water: Double, 
        handleOpacity : Boolean
    ) : Double = {

        // Maximum amount of water that the removal has been able to take from 
        // the water source
        var removedWater = 0.0

        // if the current level of the water source is greater the requested one
        if (currentLevel(year) - water >= 0) {
            
            // Then remove the requested amount
            currentLevel(year) -= water
            removedWater = water

        } else {
            
            // Otherwise, remove the current level, because it the maximum 
            // possible
            removedWater = currentLevel(year)
            currentLevel(year) = 0
        }

        // If desired
        if (handleOpacity) {

            // Update opacity of water source canvas drawing, according to 
            // current quantity of water in the water source
            opacity.setValue(1.0 - currentLevel(year) / maxLevel)
            icon.setOpacity(opacity.getValue())
        }

        // Remove a tiny portion from next year as well, to simulate water 
        // scarcity
        if (year + 1 < NUM_YEARS_TO_SIMULATE) {
            removeWater(year + 1, water / 10, false)
        }

        // Returns amount of removed water
        return removedWater

    }

    // Methods needed to compare two WaterSource objects
    def canEqual(a: Any) = a.isInstanceOf[WaterSource]

    override def equals(that: Any): Boolean =
        
        that match {
            
            case that: WaterSource => {

                // Two WaterSources are considered the same if the following
                // equalities hold true
                that.canEqual(this) &&
                this.name == that.name &&
                this.position == that.position &&
                this.rotation == that.rotation &&
                this.maxLevel == that.maxLevel
                
            }
            
            case _ => false
        }

    override def hashCode: Int = {
        name.hashCode + position.hashCode + rotation.hashCode + 
            maxLevel.hashCode
    }
}

// Models a Lake.
// This offers future expandability, in case one would like to make the Lake 
// more specialized than the general abstract class
class Lake(
    protected val maxLevel : Double, 
    protected val id : Int, 
    protected val name : String, 
    protected val icon : Picture, 
    protected var position : (Int, Int), 
    protected val borderColor : Color, 
    protected val innerColor : Color, 
    protected val rotation : Int, 
    protected val thickness : Int
) extends WaterSource {

    override def toString() : String = {
        return "\n\t\tName: " + name + " --> Lake, currentLevel: " + 
            currentLevel + ", maxLevel: " + maxLevel + "\n"
    }
}

// Models a Lake.
// This offers future expandability, in case one would like to make the Lake 
// more specialized than the general abstract class
class River(
    protected val maxLevel : Double, 
    protected val id : Int, 
    protected val name : String, 
    protected val icon : Picture, 
    protected var position : (Int, Int), 
    protected val borderColor : Color, 
    protected val innerColor : Color, 
    protected val rotation : Int, 
    protected val thickness : Int
) extends WaterSource {

    override def toString() : String = {
        return "\n\t\tName: " + name + " --> River, currentLevel: " + 
            currentLevel + ", maxLevel: " + maxLevel + "\n"
    }
}

// Models a collection of Water Sources
class WaterSources(val numOfWaterSources : Int) {
    
    // List of Water Sources stored as hashmap, in order to be able to easily
    // access a water source, according to its string name (which can be seen
    // as an ID, in a way)
    protected var waterSources = new HashMap[String, WaterSource]

    // Getter method for waterSources
    def getWaterSourcers() : HashMap[String, WaterSource] = {
        return waterSources
    }

    // Scala way of calling methods inside a constructure... simply invoke the
    // method inside the class

    // Adding the Chad lake in the water sources collection
    waterSources("Chad") = new Lake(
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
    
    // Adding the Victoria lake in the water sources collection
    waterSources("Victoria") = new Lake(
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
    
    // Adding the Niger lake in the water sources collection
    waterSources("Niger") = new River(
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
    
        waterSources.foreach(
            ws => {
                out += ws.toString()
            }
        )
    
        return out

    }

    // Returns a random water source from waterSources
    def getRandomWaterSource() : WaterSource = {
        return waterSources(
            WATER_SOURCES_LIST(Random.between(0, numOfWaterSources))
        )
    }

    // Returns a random water source from waterSources, excluding the given one
    def getRandomWaterSource(exclude : WaterSource) : WaterSource = {
        
        // Get a random water source
        var randomWaterSource = getRandomWaterSource()
        
        // Continue getting random water sources
        do {

            // Until randomly extracted water source is different from the one
            // that must be excluded from the selection process
            if (randomWaterSource.equals(exclude) == false) {
                return randomWaterSource
            }

            randomWaterSource = getRandomWaterSource()
            
        } while (true)

        return randomWaterSource
    }

    // Draws water sources in the canvas
    def drawInCanvas() {
        
        waterSources.foreach(
            ws => {
                ws._2.drawInCanvas(List())
            }
        )
    }
}

// Main class of the project.
// Models African environment, taking into consideration fauna, water sources
// and how these elements interact together
class Africa(
    protected val faunaSize : Int, 
    protected val waterSourcesSize : Int, 
    protected val icon : Picture, 
    protected var position : (Int, Int)
 ) extends Drawable {

    // African fauna.
    // See Fauna class for more information about this class
    protected var fauna = new Fauna(faunaSize)

    // African WaterSources
    // See WaterSources class for more information about this class
    protected var waterSources = new WaterSources(waterSourcesSize)

    // Stores African temperatures, for every year of the simulation
    protected var temperatures = Seq.fill(NUM_YEARS_TO_SIMULATE)(30.0)

    /**
     * Stores the mapping between Animals and Water Sources i.e. the water 
     * source any animal in the fauna drinks from, at any given year.
     *
     * LinkedHashMap rather than HashMap so as records can be shuffled.
     * All the other kind of maps basically extend Sets, in which the order does
     * NOT count, so shuffling would NOT make sense
     * 
     * Shuffling is needed to ensure non-determinism in the simulation, 
     * similarly to what happens in nature. 
     *
     * Int --> year index
     * LinkedHashMap[Animal, WaterSource] --> Animal-WaterSource association 
     *
     * See simulation() method for more details on how this map is used
     */
    
    protected var animalsWaterSourcesMapAcrossYears = new LinkedHashMap[
        Int, 
        LinkedHashMap[Animal, WaterSource]
    ]
    
    /**
     * Stores the mapping between WaterSources and Animals i.e. the water 
     * source any animal in the fauna drinks from, at any given year.
     * 
     * This may be redundant, but it is actually useful in the simulation
     * See simulation() method for more details on how this map is used
     *
     * Int --> year index
     * LinkedHashMap[WaterSource, ListBuffer[Animal]] --> 
     *      WaterSource-List of animals drinking from that water source 
     *      association 
     */
    protected var waterSourcesAnimalsMapAcrossYears = new LinkedHashMap[
        Int, 
        LinkedHashMap[WaterSource, ListBuffer[Animal]]
    ]
    
    override def toString() : String = {
        return "*** Africa ***\n\n" + "\t number of animals: " + faunaSize + 
            "\n\t animals: " + fauna.toString() + 
            "\t number of water sources: " + waterSourcesSize + 
            "\n\t water sources: " + waterSources.toString() + 
            "\n\n**************"
    }

    def putIfAbsent(
        map : LinkedHashMap[WaterSource, ListBuffer[Animal]],
        key : WaterSource,
        value : Animal
    ) : Boolean = {
        
        // Try to store the value at the given key
        try {
            
            map(key) += value

            return true
        
        } catch {

            // If key needs to be added
            case e: NoSuchElementException => {
                
                // Add the key
                map.put(
                    key, new ListBuffer[Animal]
                )
                
                //Then actually store the waterSource-Animal mapping
                map(key) += value
            
            }
        }

        return false
    }

    // Randomly associates animals in Fauna to available water sources
    def populateAnimalsWaterSourcesMap() {

        // Init Animals-WaterSources mapping at the start of the simulation
        var animalsWaterSourcesMap = new LinkedHashMap[Animal, WaterSource]
        var waterSourcesAnimalsMap = new LinkedHashMap[
            WaterSource, ListBuffer[Animal]
        ]

        // Populate mappings for the year zero.
        // For every animal in the fauna
        fauna.fauna.foreach(
            a => {

                // Get a random water source
                val ws = waterSources.getRandomWaterSource()
                
                // Store the mapping between the animal and the water source
                animalsWaterSourcesMap(a) = ws

                // To store the inverse mapping, between the water source and 
                // the animal
                putIfAbsent(waterSourcesAnimalsMap, ws, a)
                
                // After having assigned a water source to the Animal
                // Set its canvas position to be close-by the position of the 
                // Water Source
                a.setPosition(
                    (
                        ws.getPosition._1 + getRandomShift(), 
                        ws.getPosition._2 + getRandomShift()
                    )
                )
            }
        )

        // After having init'd the year zero, basically replicate the 
        // just populated associations for all the years to simulate.
        // For every year
        for (i <- 1 to NUM_YEARS_TO_SIMULATE) {

            animalsWaterSourcesMapAcrossYears(i) = new LinkedHashMap[
                Animal, WaterSource
            ]
            waterSourcesAnimalsMapAcrossYears(i) = new LinkedHashMap[
                WaterSource, ListBuffer[Animal]
            ]
            
            // For every animal in the fauna
            fauna.fauna.foreach(
                a => {

                    // Populate the animals-WaterSources map
                    animalsWaterSourcesMapAcrossYears(i) += (
                        (a, animalsWaterSourcesMap(a))
                    )

                    // Populate the WaterSources-Animals map
                    putIfAbsent(
                        waterSourcesAnimalsMapAcrossYears(i), 
                        animalsWaterSourcesMap(a), 
                        a
                    )
                }
            )
        }
    }

    // Scala way of calling methods inside a constructure... simply invoke the
    // method inside the class
    populateAnimalsWaterSourcesMap()

    // Get an RGB Color proportional to a given value
    def getColorChannelsInFunctionOfValue(
        startingValue : Int, 
        multiplier : Int, 
        value : Double, 
        r : Boolean, 
        g : Boolean, 
        b : Boolean
    ) : Color = {

        // Get color channel 
        val color = new ValueInRange(0, 255, 0)

        // Set color channel according to given value
        color.setValue(startingValue + multiplier * value)

        // return then the color, populating the channels according to the 
        // requested ones
        return Color(
            if (r) color.getValue.toInt else 0,
            if (g) color.getValue.toInt else 0,
            if (b) color.getValue.toInt else 0
        )
    }

    // Governs the entire simulation
    def simulation() {
        
        // Clears the canvas from the results of previous executions
        clear()

        // Sets the background image
        setBackground(BACKGROUND_COLOR)

        // Places Africa image in the canvas
        icon.draw()

        // Places Fauna animals in the canvas
        fauna.drawInCanvas(
            waterSources.getWaterSourcers().map{
                ws => ws._2.getIcon()
            }.toList
        )
        
        // Places WaterSources in canvas
        waterSources.drawInCanvas()

        // For every year that has to be simulated
        animalsWaterSourcesMapAcrossYears.keys.foreach(
            year => {

                println("Simulating year: " + year)

                // Compute some utility indexes
                val yearZeroBased = year - 1
                val previousYear = yearZeroBased - (
                    if ( yearZeroBased == 0 ) 0 else 1 
                )

                // Get a temperature for the year
                // Temperatures are modeled in an exponential way, starting
                // from the temperature of the very first year.
                temperatures(yearZeroBased) = temperatures(
                    previousYear
                ) * TEMPERATURE_YEARLY_MULTIPLICATIVE_FACTOR

                // For every animal-WaterSource association of the year 
                // currently under simulation

                Random.shuffle(animalsWaterSourcesMapAcrossYears(year)).foreach(

                    // Get a random animal-WaterSource association, in order to
                    // ensure non-determinism
                    association => {
                        
                        // association._1 --> animal
                        // association._2 --> water source

                        // If the animal is still alive
                        if ( association._1.isAlive(previousYear) ) {
                            
                            // get the amount of water that the animal wants to
                            // drink
                            val desiredWater = association._1.getDesiredWater()

                            // Get the quantity of water that the animal 
                            // actually managed to drink
                            val actualWater = association._2.removeWater(
                                yearZeroBased, desiredWater, true
                            )

                            // For the currently under simulation year, 
                            // record what temperature the animal has felt
                            association._1.drinkWater(yearZeroBased, actualWater)

                            // For the currently under simulation year, 
                            // record how much water the animal has managed to
                            // drink
                            association._1.feelTemperature(
                                yearZeroBased, temperatures(yearZeroBased)
                            )

                            // update life points
                            // this is the BASIC version, so the invoked
                            // function takes 2 parameters.
                            // See Animal.updateLifePoints() method(s) for more 
                            // info about its behaviour
                            val hasDied = association._1.updateLifePoints(
                                yearZeroBased
                            )
                            
                            // If the animal has died
                            if (hasDied) {

                                // Then decrease the number of currently alive
                                // animals in the fauna
                                fauna.faunaCount -= 1
                            }
                        }
                    }
                )
                
                // Add a visual delay to make simulation more enjoyable
                Thread.sleep(BETWEEN_YEARS_DELAY_MS)
     
            }
        )
    }

}

// Init main class of the simulation
var africa = new Africa(
    AfricaParams.numOfAnimals, 
    AfricaParams.numOfWaterSources, 
    Picture.image(AfricaParams.ICON_FILE_PATH),
    AfricaParams.position
)

// Starting simulation
africa.simulation()

// Simulation done
println("Simulation done!")
println("Thanks for supporting Africa.")
println("Totsiens! * Goodbye! - до побачення! * Adiós!")