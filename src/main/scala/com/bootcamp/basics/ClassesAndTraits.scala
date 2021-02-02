package com.bootcamp.basics

object ClassesAndTraits {

  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.

  sealed trait Located
  sealed trait Bounded
  sealed trait Shape extends Located with Bounded

  sealed trait Shape1D extends Shape with LocatedX with BoundedX
  sealed trait Shape2D extends Shape1D with LocatedY with BoundedY
  sealed trait Shape3D extends Shape2D with LocatedZ with BoundedZ

  sealed trait SmartShape1D extends Shape1D {
    def lenth: Double
  }

  sealed trait SmartShape2D extends Shape2D {
    def area: Double
  }

  sealed trait SmartShape3D extends Shape3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait LocatedX {
    def x: Double
  }

  sealed trait LocatedY {
    def y: Double
  }

  sealed trait LocatedZ {
    def z: Double
  }

  sealed trait BoundedX {
    def maxX: Double
    def minX: Double
  }

  sealed trait BoundedY {
    def maxY: Double
    def minY: Double
  }

  sealed trait BoundedZ {
    def maxZ: Double
    def minZ: Double
  }

  final case class Point(x: Double) extends Shape1D {
    override def maxX: Double = x
    override def minX: Double = x
  }

  final case class Point2D(x: Double, y: Double) extends SmartShape2D {
    override def maxX: Double = x
    override def minX: Double = x
    override def maxY: Double = y
    override def minY: Double = y

    override def area: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends SmartShape3D {
    override def maxX: Double = x
    override def minX: Double = x
    override def maxY: Double = y
    override def minY: Double = y
    override def maxZ: Double = z
    override def minZ: Double = z

    override def surfaceArea: Double = 0
    override def volume: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends SmartShape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def maxX: Double = x + radius
    override def minX: Double = x - radius
    override def maxY: Double = y + radius
    override def minY: Double = y - radius

    override def area: Double = math.Pi * radius * radius
  }

  final case class Triangle(point1: Point2D, point2: Point2D, point3: Point2D) extends SmartShape2D {
    override def x: Double = (point1.x + point2.x + point3.x) / 3 //Barycenter
    override def y: Double = (point1.y + point2.y + point3.y) / 3 //Barycenter
    override def maxX: Double = math.min(math.min(point1.x, point2.x), point3.x)
    override def minX: Double = math.max(math.max(point1.x, point2.x), point3.x)
    override def maxY: Double = math.min(math.min(point1.y, point2.y), point3.y)
    override def minY: Double = math.max(math.max(point1.y, point2.y), point3.y)

    override def area: Double = ???
  }

  final case class Square(luX: Double, luY: Double, side: Double) extends SmartShape2D {
    override def x: Double = luX
    override def y: Double = luY
    override def maxX: Double = x + side
    override def minX: Double = x
    override def minY: Double = y - side
    override def maxY: Double = y

    override def area: Double = side * side
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends SmartShape3D {
    override def maxX: Double = centerX + radius
    override def minX: Double = centerX - radius
    override def maxY: Double = centerY + radius
    override def minY: Double = centerY - radius
    override def maxZ: Double = centerZ + radius
    override def minZ: Double = centerZ - radius
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def surfaceArea: Double = 4 * math.Pi * radius * radius
    override def volume: Double = 4 / 3 * math.Pi * radius * radius * radius
  }

  final case class Cube(minX: Double, minY: Double, minZ: Double, edge: Double) extends SmartShape3D {
    override def x: Double = minX
    override def y: Double = minY
    override def z: Double = minZ
    override def maxX: Double = x + edge
    override def maxY: Double = y + edge
    override def maxZ: Double = z + edge

    override def surfaceArea: Double = edge * edge * 6
    override def volume: Double = edge * edge * edge
  }

  final case class Cuboid(minX: Double, minY: Double, minZ: Double, radiusX: Double,
                          radiusY: Double, radiusZ: Double) extends SmartShape3D {
    override def x: Double = minX
    override def y: Double = minY
    override def z: Double = minZ
    override def maxX: Double = x + radiusX
    override def maxY: Double = y + radiusY
    override def maxZ: Double = z + radiusZ

    override def surfaceArea: Double = radiusX*radiusY*2 + radiusX*radiusZ*2 + radiusY*radiusZ*2
    override def volume: Double = radiusX * radiusY * radiusZ
  }

  final case class Triangle3D(point1: Point3D, point2: Point3D, point3: Point3D) extends SmartShape3D {

    override def x: Double = ???
    override def y: Double = ???
    override def z: Double = ???
    override def maxX: Double = ???
    override def minX: Double = ???
    override def maxY: Double = ???
    override def minY: Double = ???
    override def maxZ: Double = ???
    override def minZ: Double = ???

    override def surfaceArea: Double = ???
    override def volume: Double = 0
  }

//  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded =
//    new Bounded {
//      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering
//      override def minX: Double = objects.map(_.minX).min
//      override def maxX: Double = objects.map(_.maxX).max
//      override def minY: Double = objects.map(_.minY).min
//      override def maxY: Double = objects.map(_.maxY).max
//    }

  // Pattern matching and exhaustiveness checking
  def describe(x: Shape): String = x match {
    case Point2D(x, y)                      => s"Point(x = $x, y = $y)"
    case Circle(centerX, centerY, radius) => s"Circle(centerX = $centerX, centerY = $centerY, radius = $radius)"
  }

  // Singleton can extend classes and mix in traits
  object Origin extends LocatedX {
    override def x: Double = 0
  }

  object Origin2D extends LocatedX with LocatedY {
    override def x: Double = 0
    override def y: Double = 0
  }

  object Origin3D extends LocatedX with LocatedY with LocatedZ {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

}
