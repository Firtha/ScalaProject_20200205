package ScalaProject

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date

import scala.io.Source

object Main {
  // This variable is used to display more information.
  // 0 : program execution without dev details
  // 1 : program execution with dev details and explanation
  val devDisplays = 1

  case class Cat(name: String, race: String, age: Int)
  case class Person(firstName: String, lastName: String, salary: Int, numberOfChildren: Int)
  case class Car(brand: String, countryOfBirth: String, maxSpeed: Int, speeds: Int)
  case class Film(mainActors: Seq[String], dateOfRelease: Date)
  case class Actor(name: String, filmsPlayed: Seq[String])


  def findTypeFromIndex(stringList: List[List[String]], index: Int): Array[Int] ={
    val currList = stringList(index)
    if(devDisplays == 1) {
      println(currList.size + " fields in " + currList)
    }

    var intCpt = 0
    var stringCpt = 0
    var dateCpt = 0
    var listCpt = 0

    for(a <- 0 until currList.size){
      if(currList(a).toIntOption.isDefined){
        if(devDisplays == 1) {
          println(a + " - " + currList(a) + " is an Integer !")
        }
        intCpt += 1
      } else {
        if(currList(a).contains(",")){
          if(devDisplays == 1) {
            println(a + " - " + currList(a) + " is a List !")
          }
          listCpt += 1
        } else {
          if(devDisplays == 1) {
            println(a + " - " + currList(a) + " is a String !")
          }

          if(currList(a).matches("[0-9]{2}/[0-9]{2}/[0-9]{4}")){
            if(devDisplays == 1) {
              println(a + " - " + ".... and " + currList(a) + " is also a Date !")
            }
            dateCpt += 1
          } else {
            stringCpt += 1
          }
        }
      }
    }

    if(devDisplays == 1) {
      println("this list contains " + intCpt + " integers, " + stringCpt + " strings, " + dateCpt + " dates and " + listCpt + " lists.")
    }
    val lineAttributes: Array[Int] = Array(stringCpt, intCpt, dateCpt, listCpt)
    return lineAttributes
  }

  def processIndex(size: Int, index: Int): Int ={
    val realIndex = size - 2 - index
    if(devDisplays == 1){
      println("Input " + index + " has been process into the value " + realIndex + " of our List of List.")
    }
    return realIndex
  }

  def createObject(currList: List[String], currAttributes: Array[Int]): String = {
    // These attributes defines the number of types for each class
    // The array contains 4 elements :
    // 0 - Number of String
    // 1 - Number of Integer
    // 2 - Number of Dates
    // 3 - Number of List
    val catAttributes: Array[Int] = Array(2, 1, 0, 0)
    val personAttributes: Array[Int] = Array(2, 2, 0, 0)
    val carAttributes: Array[Int] = Array(2, 2, 0, 0)
    val filmAttributes: Array[Int] = Array(0, 0, 1, 1)
    val actorAttributes: Array[Int] = Array(1, 0, 0, 1)

    if (currAttributes(0) == catAttributes(0) && currAttributes(1) == catAttributes(1) && currAttributes(2) == catAttributes(2) && currAttributes(3) == catAttributes(3)) {
      if(devDisplays == 1) {
        println("It's a cat !")
      }
      return "cat"
    } else if (currAttributes(0) == filmAttributes(0) && currAttributes(1) == filmAttributes(1) && currAttributes(2) == filmAttributes(2) && currAttributes(3) == filmAttributes(3)) {
      if(devDisplays == 1) {
        println("It's a film !")
      }
      return "film"
    } else if (currAttributes(0) == actorAttributes(0) && currAttributes(1) == actorAttributes(1) && currAttributes(2) == actorAttributes(2) && currAttributes(3) == actorAttributes(3)) {
      if(devDisplays == 1) {
        println("It's an actor !")
      }
      return "actor"
    } else if (currAttributes(0) == personAttributes(0) && currAttributes(1) == personAttributes(1) && currAttributes(2) == personAttributes(2) && currAttributes(3) == personAttributes(3)) {
      if(devDisplays == 1) {
        println("It's a person OR a car !")
      }
      if(currList(3).toInt < 25){
        if(devDisplays == 1) {
          println("It's a person !")
        }
        return "person"
      } else {
        if(devDisplays == 1) {
          println("It's a car !")
        }
        return "car"
      }
    } else {
      if(devDisplays == 1) {
        println("It's nothing that we know !")
      }
      return "undefined"
    }
  }

  def writeJsonContent(catList: List[Cat], carList: List[Car], filmList: List[Film], personList: List[Person], actorList: List[Actor]): String ={
    var jsonContent = "{\n\"Cats\":[\n"
    for(a <- 0 until catList.length){
      if(a < catList.length && a > 0){
        jsonContent += ",\n"
      }
      jsonContent += "{\n\"name\":\"" + catList(a).name + "\",\n\"race\":\"" + catList(a).race + "\",\n\"age\":\"" + catList(a).age + "\"\n}"
    }
    jsonContent += "\n],\n\"Cars\":[\n"
    for(a <- 0 until carList.length){
      if(a < carList.length && a > 0){
        jsonContent += ",\n"
      }
      jsonContent += "{\n\"brand\":\"" + carList(a).brand + "\",\n\"countryOfBirth\":\"" + carList(a).countryOfBirth + "\",\n\"maxSpeed\":\"" + carList(a).maxSpeed + "\",\n\"speeds\":\"" + carList(a).speeds + "\"\n}"
    }
    jsonContent += "\n],\n\"Persons\":[\n"
    for(a <- 0 until personList.length){
      if(a < personList.length && a > 0){
        jsonContent += ",\n"
      }
      jsonContent += "{\n\"firstname\":\"" + personList(a).firstName + "\",\n\"lastname\":\"" + personList(a).lastName + "\",\n\"salary\":\"" + personList(a).salary + "\",\n\"numberOfChildren\":\"" + personList(a).numberOfChildren + "\"\n}"
    }
    jsonContent += "\n],\n\"Actors\":[\n"
    for(a <- 0 until actorList.length){
      if(a < actorList.length && a > 0){
        jsonContent += ","
      }
      var filmsPayed = "[\n"
      for(b <- 0 until actorList(a).filmsPlayed.length){
        if(b < actorList(a).filmsPlayed.length && b > 0){
          filmsPayed += ",\n"
        }
        filmsPayed += "\"" + actorList(a).filmsPlayed(b) + "\""
      }
      filmsPayed += "\n]"
      jsonContent += "{\n\"name\":\"" + actorList(a).name + "\",\n\"filmsPlayed\":" + filmsPayed + "\n}"
    }
    jsonContent += "\n],\n\"Films\":[\n"
    for(a <- 0 until filmList.length){
      if(a < filmList.length && a > 0){
        jsonContent += ",\n"
      }
      var mainActors = "[\n"
      for(b <- 0 until filmList(a).mainActors.length){
        if(b < filmList(a).mainActors.length && b > 0){
          mainActors += ",\n"
        }
        mainActors += "\"" + filmList(a).mainActors(b) + "\""
      }
      mainActors += "\n]"
      jsonContent += "{\n\"mainActors\":" + mainActors + ",\n\"dateOfRelease\":\"" + filmList(a).dateOfRelease + "\"\n}"
    }
    jsonContent += "]\n}"

    println(jsonContent)
    return jsonContent
  }

  def main(args: Array[String]): Unit = {
    val sourceFile = Source.fromFile("CSVs/dataset.csv")
    val csvContent = sourceFile.getLines()
    //sourceFile.close()

    println("\n\n          ===================================")
    println("          ========== SCALA PROJECT ==========")
    println("          ===================================")
    println("          ==    Author : Svensson Jeremy   ==")
    println("          ==                               ==")
    println("          ==    Delivered the 05/02/2020   ==")
    println("          ===================================\n\n")

    if(devDisplays == 1) {
      println("Hello Ayoub !\n")
      println("Here are the details of my formated lists and some informations about the data processing i used :")
    }

    var nbLine = 0
    var lines: List[List[String]] = List(List(""))
    while(csvContent.hasNext) {
      val tmpList: List[String] = csvContent.next().split(";").map(_.trim).toList
      lines = tmpList::lines
      if(devDisplays == 1) {
        println("Line spotted " + nbLine + ", List:[" + tmpList + "]")
      }
      nbLine = nbLine + 1
    }
    if(devDisplays == 1){
      println("\nIndexes in the List of List are opposite to reality, a function 'processIndex' has been designed to process inputs.")
    }

    println("\nPlease write a value from 0 to " + (lines.size-2) + ", we'll find the type of this line.")
    val indexSelected = processIndex(lines.size, scala.io.StdIn.readInt())
    findTypeFromIndex(lines, indexSelected)

    var catArray: List[Cat] = List()
    var carArray: List[Car] = List()
    var personArray: List[Person] = List()
    var actorArray: List[Actor] = List()
    var filmArray: List[Film] = List()


    println("\n\nAnd now to every lines....")
    for(a <- 0 to lines.size-2){
      println("\n\nLine " + a + " of the list :")
      val lineAttributes: Array[Int] = findTypeFromIndex(lines, a)
      println("----------------------------------")
      val typeIs = createObject(lines(a), lineAttributes)
      typeIs match {
        case "cat" => println("Cat found")
          val myCat = new Cat(lines(a)(0), lines(a)(1), lines(a)(2).toInt)
          catArray = myCat :: catArray

        case "car" => println("Car found")
          val myCar = new Car(lines(a)(0), lines(a)(1), lines(a)(2).toInt, lines(a)(3).toInt)
          carArray = myCar :: carArray

        case "person" => println("Person found")
          val myPerson = new Person(lines(a)(0), lines(a)(1), lines(a)(2).toInt, lines(a)(3).toInt)
          personArray = myPerson :: personArray

        case "actor" => println("Actor found")
          val filmsPlayed: Seq[String] = lines(a)(1).split(",")
          val myActor = new Actor(lines(a)(0), filmsPlayed)
          actorArray = myActor :: actorArray

        case "film" => println("Film found")
          val mainActors: Seq[String] = lines(a)(0).split(",")
          val format = new SimpleDateFormat("dd/MM/yyyy")
          val filmDate = format.parse(lines(a)(1))
          val myFilm = new Film(mainActors, filmDate)
          filmArray = myFilm :: filmArray

        case "undefined" => println("Undefined found")
      }
    }

    println("We now have :")
    println("- " + catArray.length + " cats.")
    println("- " + carArray.length + " cars.")
    println("- " + actorArray.length + " actors.")
    println("- " + filmArray.length + " films.")
    println("- " + personArray.length + " persons.")

    val jsonContent = writeJsonContent(catArray, carArray, filmArray, personArray, actorArray)
    val filePointer = new PrintWriter(new File("JSONs/dataset.json"))
    filePointer.write(jsonContent)
    filePointer.close()
  }
}