import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.language.postfixOps
import scala.collection.mutable.Map
import scala.collection.mutable.Seq

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

// Number of maximum day without satisfied needs that an animal tolerates,
// before evaluating whether to migrate or not.

// See Animal.evaluateMigration method for more info about the usage of this
// constant
var MAX_DAYS_WITHOUT_SATISFIED_NEEDS = 2

// Delay in ms between different years, in order to make graphical appearance
// more usable
val BETWEEN_YEARS_DELAY_MS = 1000

// Delay in ms between different migration steps, in order to make graphical 
// appearance of migration more comprehensible and actually visible
val MIGRATION_STEP_DELAY_MS = 750


/* ************************************************  */


/** Set of variables controlling simulation graphical appearance */

// Dimension of text printed in canvas
val HEADER_TEXT_SCALE_FACTOR = 5

// Number of steps to show, when an animal migrates from a water source to 
// another one
val MIGRATION_NUM_VISUAL_STEPS = 5

// Relative path of folder containing animal icons, which will be shown
// in the canvas
val ICON_FOLDER_PATH = "/home/dansolombrino/GitHub/Africa-Wildlife/assets/icons/"

// Relative path of folder containing background image, which will be shown
// in the canvas
val BACKGROUND_FOLDER_PATH = "/home/dansolombrino/GitHub/Africa-Wildlife/assets/background/"

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
  val MAX_FELT_TEMPERATURE = 45;
  val MIN_WATER = 2
  val ICON_FILE_PATH = ICON_FOLDER_PATH + "lion_64.png"
}

object ElephantParams {
  val MAX_FELT_TEMPERATURE = 45;
  val MIN_WATER = 2
  val ICON_FILE_PATH = ICON_FOLDER_PATH + "elephant_64.png"
}

object ZebraParams {
  val MAX_FELT_TEMPERATURE = 45;
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

    // Maximum temperature the animal can sustain, every day
    protected val maxFeltTemperature : Double

    // Minimum quantity of water the animal needs, every day
    protected val minWater : Double

    // numeric ID of the animal, used mostly for debugging
    protected val id : Int

    // Current number of consecutive days in which the animal did not manage to 
    // satisfy its needs
    protected var daysWithoutSatisfiedNeeds = 0
    protected var rival = ""
    
    protected var lifePoints = Seq.fill(NUM_YEARS_TO_SIMULATE)(1.0)
    protected var drankWater = Seq.fill(NUM_YEARS_TO_SIMULATE)(0.0)
    protected var feltTemperature = Seq.fill(NUM_YEARS_TO_SIMULATE)(0.0)

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
        lifePoints(day) += 50 * drankWater(day) - 6 * scala.math.pow(2, feltTemperature(day) - maxFeltTemperature)

        if (lifePoints(day) < 0) {
            die(day)

            return true
        }

        return false
    }

    def updateLifePoints(day : Int, numEncounteredRivals : Int) : Boolean = {
        lifePoints(day) += -10 * numEncounteredRivals
        
        return updateLifePoints(day)
    }

    def updateLifePoints(day : Int, numEncounteredRivals : Int, migrationDistance : (Double, Double)) : Boolean = {
        lifePoints(day) += -(migrationDistance._1 + migrationDistance._2) / 25

        return updateLifePoints(day, numEncounteredRivals)
    }

    def die(day : Int) {
        for (i <- day to NUM_YEARS_TO_SIMULATE - 1) {
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

        return daysWithoutSatisfiedNeeds >= MAX_DAYS_WITHOUT_SATISFIED_NEEDS
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

    def migrate(ws : WaterSource) : (Double, Double) = {
        //println("CurrentPosition: " + this.icon.position)
        //println("TargetPosition: " + ws.getPosition())

        var absDist = (
            ws.getPosition()._1 - this.icon.position.x, 
            ws.getPosition()._2 - this.icon.position.y
        )
        //println("absDistance: " + absDist)

        var step_x = absDist._1 / MIGRATION_NUM_VISUAL_STEPS
        var step_y = absDist._2 / MIGRATION_NUM_VISUAL_STEPS

        for (i <- 1 to MIGRATION_NUM_VISUAL_STEPS) {
            
            icon.translate(step_x,step_y)
            Thread.sleep(MIGRATION_STEP_DELAY_MS)
            
            if (icon.collidesWith(ws.getIcon())) {
                icon.translate(getRandomShift(), getRandomShift())
            }
            
        }

        daysWithoutSatisfiedNeeds = 0

        return absDist
        
    }

}

class Lion(
    protected val maxFeltTemperature : Double, 
    protected val minWater : Double, 
    protected val id : Int, 
    protected val icon : Picture, 
    protected var position: (Int, Int)
) extends Animal {

    rival = "Zebra"

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Lion, maxFeltTemperature: " + maxFeltTemperature + ", lifePoints: " + lifePoints + ", drankWater: " + drankWater + ", feltTemperature: " + feltTemperature + "\n"
    }
}

class Elephant(
    protected val maxFeltTemperature : Double, 
    protected val minWater : Double, 
    protected val id : Int, 
    protected val icon : Picture,
    protected var position : (Int, Int)
) extends Animal {

    rival = "Lion"

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Elephant, maxFeltTemperature: " + maxFeltTemperature + ", lifePoints: " + lifePoints + ", drankWater: " + drankWater + ", feltTemperature: " + feltTemperature + "\n"
    }
}

class Zebra(
    protected val maxFeltTemperature : Double, 
    protected val minWater : Double, 
    protected val id : Int, 
    protected val icon : Picture,
    protected var position : (Int, Int)
) extends Animal {

    rival = "Lion"

    override def toString() : String = {
        return "\n\t\tID: " + id + " --> Zebra, maxFeltTemperature: " + maxFeltTemperature + ", lifePoints: " + lifePoints + ", drankWater: " + drankWater + ", feltTemperature: " + feltTemperature + "\n"
    }
}

class Fauna(val faunaSize : Int) {

    var faunaCount = faunaSize
    
    var fauna: ListBuffer[Animal] = ListBuffer()

    def populateFauna(): ListBuffer[Animal] = {
        
        for (i <- 1 to faunaSize) {
            
            val animalSpecie = ANIMAL_SPECIES_LIST(Random.between(0, ANIMAL_SPECIES_LIST.length))
        
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


trait WaterSource extends DrawableShape {
    protected val maxLevel : Double
    protected var currentLevel = Seq.fill(NUM_YEARS_TO_SIMULATE)(maxLevel)
    protected val id : Int
    protected val name : String
    
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
            icon.setOpacity(opacity.getValue())
        }

        if (day + 1 < NUM_YEARS_TO_SIMULATE) {
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
        return "\n\t\tName: " + name + " --> Lake, currentLevel: " + currentLevel + ", maxLevel: " + maxLevel + "\n"
    }
}

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
        return "\n\t\tName: " + name + " --> River, currentLevel: " + currentLevel + ", maxLevel: " + maxLevel + "\n"
    }
}

class WaterSources(val numOfWaterSources : Int) {
    
    protected var waterSources = new HashMap[String, WaterSource]

    def getWaterSourcers() : HashMap[String, WaterSource] = {
        return waterSources
    }

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

    def getRandomWaterSource() : WaterSource = {
        return waterSources(WATER_SOURCES_LIST(Random.between(0, numOfWaterSources)))
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
       
        waterSources.foreach(
            ws => {
                ws._2.drawInCanvas(List())
            }
        )
    }
}

class HeaderElement(
    protected var text : String, 
    protected var value : Double, 
    protected var valueIsInt : Boolean,
    protected var position : (Int, Int), 
    protected var color : Color, 
    protected var scale : Double
 ) {
    protected var element = Picture.text("")

    def update(day : Int, updatedValue : Double, updatedColor : Color) {
        
        if (day > -1) {
            element.erase()
        }

        element = Picture.text(text + ": " + (if (valueIsInt) updatedValue.toInt else updatedValue))
        element.setPosition(position._1, position._2)
        element.setPenColor(updatedColor)
        element.scale(scale)

        element.draw()
    }
}

class Header(protected val elements : List[HeaderElement]) {
    
    def update(day : Int, updatedValues : List[Double], updatedColors : List[Color]) {

        if (updatedValues.length != updatedColors.length) {
            throw new RuntimeException("ERROR updatedValues.length must be equal to updatedColors.length!")
        }
        
        for (e <- 0 to updatedValues.length - 1) {
            
            elements(e).update(day, updatedValues(e), updatedColors(e))
            
        }
    }
    
}

class Africa(
    protected val faunaSize : Int, 
    protected val waterSourcesSize : Int, 
    protected val icon : Picture, 
    protected var position : (Int, Int)
 ) extends Drawable {

    protected var fauna = new Fauna(faunaSize)

    protected var waterSources = new WaterSources(waterSourcesSize)

    protected var temperatures = Seq.fill(NUM_YEARS_TO_SIMULATE)(30.0)

    protected var header = new Header(
        List(
            new HeaderElement("Year", 0, true, (400, 1515), BLUE_COLOR, HEADER_TEXT_SCALE_FACTOR),
            new HeaderElement("Temperature", 0, false, (250, 1415), RED_COLOR, HEADER_TEXT_SCALE_FACTOR),
            new HeaderElement("Fauna current count", 0, true, (200, 1315), GREEN_COLOR, HEADER_TEXT_SCALE_FACTOR),
            new HeaderElement("Fauna original count", fauna.faunaSize, true, (200, 1215), GREEN_COLOR, HEADER_TEXT_SCALE_FACTOR)
        )
    )

    // LinkedHashMap rather than HashMap so as records can be shuffled
    // Shuffling a HashMap record yould require to convert to list first, since in a Set the order does NOT count, hence shuffling does NOT make any sense 
    // int --> day index
    // inner map --> actual water source <----> animal association, for that given day
    protected var animalsWaterSourcesMapAcrossYears = new LinkedHashMap[
        Int, 
        LinkedHashMap[
            Animal, WaterSource
        ]
    ]
    
    protected var waterSourcesAnimalsMapAcrossYears = new LinkedHashMap[
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
                
                a.setPosition(
                    (
                        ws.getPosition._1 + getRandomShift(), 
                        ws.getPosition._2 + getRandomShift()
                    )
                )
            }
        )
        for (i <- 1 to NUM_YEARS_TO_SIMULATE) {

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
        for (i <- dayZeroBased to NUM_YEARS_TO_SIMULATE) {
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
            if (r) color.getValue.toInt else 0,
            if (g) color.getValue.toInt else 0,
            if (b) color.getValue.toInt else 0
        )
    }

    def simulation() {
        
        clear()

        setBackground(BACKGROUND_COLOR)

        icon.draw()

        fauna.drawInCanvas(
            waterSources.getWaterSourcers().map{
                ws => ws._2.getIcon()
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
                        STARTING_YEAR + day, 
                        (Math.floor(temperatures(dayZeroBased) * 100) / 100),
                        fauna.faunaCount,
                        fauna.faunaSize
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
                       ),
                       getColorChannelsInFunctionOfValue(
                            0, 
                            20, 
                            fauna.faunaSize, 
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

                            var migrationDistance = (0.0, 0.0)

                            if ( association._1.evaluateMigration(actual_water, desired_water) ) {
                      
                                val wsToMigrateTo = waterSources.getRandomWaterSource(association._2)
                                
                                migrationDistance = association._1.migrate(wsToMigrateTo)

                                updateAnimalsWaterSourcesMap(dayZeroBased, association._1, wsToMigrateTo)

                            }
    
                            association._1.drinkWater(dayZeroBased, actual_water)

                            association._1.feelTemperature(dayZeroBased, temperatures(dayZeroBased))

                            var hasDied = association._1.updateLifePoints(
                                dayZeroBased, 
                                association._1.countEncounteredRivals(
                                    waterSourcesAnimalsMapAcrossYears(day)(association._2)
                                ),
                                migrationDistance
                            )

                            if (hasDied) {
                                fauna.faunaCount -= 1
                            }
                        }
                    }
                )
                
                Thread.sleep(BETWEEN_YEARS_DELAY_MS)
     
            }
        )

    }

}

object AfricaParams {
  val numOfAnimals = Random.between(MIN_NUM_OF_ANIMALS, MAX_NUM_OF_ANIMALS)
  val numOfWaterSources = 3 // keep it this way, for testing purposes
  val ICON_FILE_PATH = BACKGROUND_FOLDER_PATH + "africaClean.png"
  val position = (0, 0)
}

var africa = new Africa(
    AfricaParams.numOfAnimals, 
    AfricaParams.numOfWaterSources, 
    Picture.image(AfricaParams.ICON_FILE_PATH),
    AfricaParams.position
)

africa.simulation()

println("DONE")















