package Simulator

import java.io.{File, IOException}

import com.fasterxml.jackson.databind.{MappingIterator, ObjectReader}
import com.fasterxml.jackson.dataformat.csv.{CsvMapper, CsvParser, CsvSchema}

import scala.reflect.ClassTag

/**
  * Created by WadeJensen on 19/08/2017.
  */

import scala.collection.JavaConverters._


object SolarCarSimulator {
  def main(args: Array[String]) {
    println("Hello, world!")

    val path = "C:\\Users\\WadeJensen\\Dropbox\\01_EN40\\YEAR_4_SEM_2\\" +
      "CAB401\\SolarCarSimulator\\src\\main\\scala\\Simulator\\route.csv"
    val coordinates = readCsvRowsAsArrays[Double](path)

    val controlStops

  }

  controlStopGps = [-12.46284 130.84179; % Darwin
    %-14.464216 132.261955; % Katherine
    -16.679497 133.411811; % Dunmarra
    -19.657682, 134.188508; % Tennant Creek
  -21.531326 133.887739; % Barrow Creek
  -23.70861 133.8756; % Alice Springs
  -25.83911 133.315722; % Kulgera
    -29.011056 134.75467; % Coober Pedy
  -30.9698611 135.74900; % Glendambo
    -32.5091944 137.796722; % Port Augusta
  -34.724838, 138.579624;% EOT
  ];

  def readCsvRowsAsArrays[T: ClassTag](path: String): List[Array[T]] = {
    var mapper = new CsvMapper()
    mapper.enable(CsvParser.Feature.WRAP_AS_ARRAY)

    val csvFile: File = new File(path)
    val javaIter: MappingIterator[Array[T]] = mapper.readerFor(classOf[Array[T]])
                                                    .readValues(csvFile)

    val rows = javaIter.asScala
      .collect { case xs: Array[T] => xs }
      .toList

    rows
  }


}


  //    catch {
//      case e: IOException => {
//        e.printStackTrace()
//        System.out.println("Route file is not valid.")
//        None
//      }
//    }


