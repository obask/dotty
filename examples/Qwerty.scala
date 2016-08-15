package examples

final module class Enterprise$
  extends
    Object {
  def init(): Unit = {
    super()
    ()
    }
    def mesure(g: examples.Enterprise.Geometry): Unit = {
    examples.Fmt.Println(g)
    examples.Fmt.Println(scala.Double.box(g.area()))
    examples.Fmt.Println(scala.Double.box(g.perim()))
    }


    def main(args: String[]): Unit = println("Hello World")

    }
    class Rect extends Object with examples.Enterprise.Geometry {
    def init(width: Double, height: Double): Unit = {
      this.width_local = width
      this.height_local = height
      super()
      ()
      }

      private val width_local: Double
      def width(): Double = this.width$$local
        private val height_local: Double
        def height(): Double = this.height$$local
          def area(): Double = Rect.this.width().*(Rect.this.height())
          def perim(): Double = 2.*(Rect.this.width()).+(2.*(Rect.this.height()))
      }

    class Circle extends Object with examples.Enterprise.Geometry {
          def init(radius: Double): Unit = {
            this.radius$$local = radius
            super()
            ()
            }
            private val radius_local: Double
             def radius(): Double = this.radius$$local
              def area(): Double = examples.Fmt.Pi().*(Circle.this.radius()).*(Circle.this.radius())
              def perim(): Double = 2.*(examples.Fmt.Pi()).*(Circle.this.radius())
    }

    trait Geometry extends Object {
                def init(): Unit = ()
                  def area(): Double
                  def perim(): Double
    }
    final lazy module val Enterprise: examples.Enterprise$ = new examples.Enterprise$()

