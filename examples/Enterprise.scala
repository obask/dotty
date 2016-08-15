package examples

object Enterprise {

//    object Fmt {
//
//      def Println(x: Any) = {
//        println(x)
//      }
//
//
//      val Pi = 3.14
//
//    }

  trait Geometry {
        def area: Double
        def perim: Double
  }


    class Rect(val width: Double, val height: Double) extends Geometry {

        def area = {
            width * height
        }

        def perim = {
            2*width + 2*height
        }

    }

    class Circle(val radius: Double) extends Geometry {


        def area = {
            Fmt.Pi * radius * radius
        }

        def perim = {
            2 * Fmt.Pi * radius
        }

    }

    def mesure(g: Geometry) = {
        Fmt.Println(g)
        Fmt.Println(g.area)
        Fmt.Println(g.perim)
    }


  def main(args: Array[String]): Unit = {

    println("Hello World")

  }

}

