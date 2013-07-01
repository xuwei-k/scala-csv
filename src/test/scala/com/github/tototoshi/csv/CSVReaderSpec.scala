package com.github.tototoshi.csv

import java.io.{UnsupportedEncodingException, FileReader, File}

import org.scalatest.FunSpec
import org.scalatest.matchers._

class CSVReaderSpec extends FunSpec with ShouldMatchers with Using {

  def fixture = new {

  }

  describe("CSVReader") {

    it("should be constructed with java.io.File") {
      var res: List[String] = Nil
      using (CSVReader.open(new File("src/test/resources/simple.csv"))) { reader =>
        reader foreach { fields =>
          res = res ++ fields
        }
      }
      res.mkString should be ("abcdef")
    }

    it("should be constructed with filename") {
      var res: List[String] = Nil
      using (CSVReader.open("src/test/resources/simple.csv")) { reader =>
        reader foreach { fields =>
          res = res ++ fields
        }
      }
      res.mkString should be ("abcdef")
    }

    it("should be consutrcted with CSVFormat") {
      implicit object format extends DefaultCSVFormat {
        override val delimiter: Char = '#'
        override val quoteChar: Char = '$'
      }

      using (CSVReader.open("src/test/resources/hash-separated-dollar-quote.csv")) { reader => {
          val map = reader.allWithHeaders()
          map(0)("Foo ") should be ("a")
        }
      }

      using (CSVReader.open("src/test/resources/hash-separated-dollar-quote.csv", "utf-8")) { reader =>
        val map = reader.allWithHeaders()
        map(0)("Foo ") should be ("a")
      }
    }

    it("should throws UnsupportedEncodingException when unsupprted encoding is specified") {
      intercept[UnsupportedEncodingException] {
        using (CSVReader.open("src/test/resources/hash-separated-dollar-quote.csv", "unknown")) { reader =>
          val map = reader.allWithHeaders()
          map(0)("Foo ") should be ("a")
        }
      }
    }

    it("read simple CSV from file") {
      var res: List[String] = Nil
      using (CSVReader.open(new FileReader("src/test/resources/simple.csv"))) { reader =>
        reader foreach { fields =>
          res = res ++ fields
        }
      }
      res.mkString should be ("abcdef")
    }

    it("read CSV file including escaped fields") {
      var res: List[String] = Nil
      using (CSVReader.open(new FileReader("src/test/resources/escape.csv"))) { reader =>
        reader foreach { fields =>
          res = res ++ fields
        }
      }
      res.mkString should be ("""abcd"ef""")
    }

    it("read TSV from file") {
      implicit val format = new TSVFormat {}
      var res: List[Seq[String]] = Nil
      using (CSVReader.open(new FileReader("src/test/resources/simple.tsv"))(format)) { reader =>
        reader.foreach { fields =>
          res = res ::: fields :: Nil
        }
      }
      res(0) should be (List("a", "b", "c"))
      res(1) should be (List("d", "e", "f"))
    }

    it("read TSV with escaped values from file") {
      implicit val format = new TSVFormat {}
      var res: List[Seq[String]] = Nil
      using (CSVReader.open(new FileReader("src/test/resources/escape.tsv"))(format)) { reader =>
        reader.foreach { fields =>
          res = res ::: fields :: Nil
        }
      }
      res(0) should be (List("a", "b\t", "c"))
      res(1) should be (List("d", "e", "f\ng"))
      res(2) should be (List("h", "i", "j"))
    }

    it("has #toStream") {
      using (CSVReader.open(new File("src/test/resources/simple.csv"))) { reader =>
        val stream = reader.toStream
        stream.drop(1).head.mkString should be ("def")
      }
    }

    it("has #readNext") {
      using (CSVReader.open(new File("src/test/resources/simple.csv"))) { reader =>
        reader.readNext()
        reader.readNext.get.mkString should be ("def")
      }
    }

    it("has #all") {
      using (CSVReader.open(new FileReader("src/test/resources/simple.csv"))) { reader =>
        reader.all should be (List(List("a", "b", "c"), List("d", "e", "f")))
      }
    }

    describe("iterator fetched from #iterator") {

      it ("has #hasNext") {
        using (CSVReader.open("src/test/resources/simple.csv")) { reader =>
          val it = reader.iterator
          it.hasNext should be (true)
          it.hasNext should be (true)
          it.hasNext should be (true)
          it.foreach(x => ()) // cosume
          it.hasNext should be (false)
        }
      }

      describe ("#next") {
        it ("should return the next line") {
          using (CSVReader.open("src/test/resources/simple.csv")) { reader =>
            val it = reader.iterator
            it.next should be (List("a", "b", "c"))
            it.next should be (List("d", "e", "f"))
          }
        }
        it ("should throw NoSuchElementException") {
          using (CSVReader.open("src/test/resources/simple.csv")) { reader =>
            val it = reader.iterator
            it.next
            it.next
            intercept[java.util.NoSuchElementException] {
              it.next
            }
          }
        }
      }

      it ("iterate all lines") {
        var lineCount = 0
        using (CSVReader.open("src/test/resources/simple.csv")) { reader =>
          val it = reader.iterator
          it.foreach { line => lineCount += 1 }
        }
        lineCount should be (2)
      }

      describe ("When the file to be parsed is huge") {
        it ("should iterate all lines without any trouble") {
          val tmpfile: File = File.createTempFile("csv", "test")
          tmpfile.deleteOnExit()
          using(new java.io.PrintWriter(tmpfile)) { writer =>
            (1 to 100000).foreach { i =>
              writer.println(i)
            }
          }
          var count = 0
          using (CSVReader.open(tmpfile)) { reader =>
            reader.foreach { row =>
              count += 1
            }
          }
          count should be (100000)
        }
      }
    }
    describe("#allHeaders") {
      describe("When the file is empty") {
        it("returns an empty list") {
          using (CSVReader.open(new FileReader("src/test/resources/empty.csv"))) { reader =>
            reader.allWithHeaders should be('empty)
          }
        }
      }
      describe("When the file has only one line") {
        it("returns an empty list") {
          using (CSVReader.open(new FileReader("src/test/resources/only-header.csv"))) { reader =>
            reader.allWithHeaders should be('empty)
          }
        }
      }
      describe("When the file has many lines") {
        it("returns a List of Map[String, String]") {
          using (CSVReader.open(new FileReader("src/test/resources/with-headers.csv"))) { reader =>
            reader.allWithHeaders should be (List(Map("Foo" -> "a", "Bar" -> "b", "Baz" -> "c"), Map("Foo" -> "d", "Bar" -> "e", "Baz" ->  "f")))
          }
        }
      }
    }

  }
}

