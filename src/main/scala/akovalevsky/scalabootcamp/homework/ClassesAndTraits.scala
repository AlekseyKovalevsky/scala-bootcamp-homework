package akovalevsky.scalabootcamp.homework

object ClassesAndTraits {

  sealed trait Shape2D extends Located2D with Bounded2D with Movable2D

  // let the location be the geometric center of a shape
  sealed trait Located2D {
    def x: Double

    def y: Double
  }

  sealed trait Bounded2D {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Shape2D
  }

  // Should we use adjectives as trait names? Area looks more natural
  sealed trait Area {
    def area: Double
  }

  // The area is not defined for a point
  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point2D = Point2D(x + dx, y + dy)

    def distanceTo(other: Point2D): Double = Math.sqrt(Math.pow(other.x - x, 2) + Math.pow(other.y - y, 2))
  }

  final case class Circle(center: Point2D, radius: Double) extends Shape2D with Area {
    override def x: Double = center.x

    override def y: Double = center.y

    override def minX: Double = x - radius

    override def maxX: Double = x + radius

    override def minY: Double = y - radius

    override def maxY: Double = y + radius

    override def move(dx: Double, dy: Double): Circle = Circle(center.move(dx, dy), radius)

    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Rectangle(leftBottomCorner: Point2D, size: Double) extends Shape2D with Area {
    override def x: Double = leftBottomCorner.x + size / 2

    override def y: Double = leftBottomCorner.y + size / 2

    override def minX: Double = leftBottomCorner.x

    override def maxX: Double = leftBottomCorner.x + size

    override def minY: Double = leftBottomCorner.y

    override def maxY: Double = leftBottomCorner.y + size

    override def area: Double = math.pow(size, 2)

    override def move(dx: Double, dy: Double): Rectangle = Rectangle(leftBottomCorner.move(dx, dy), size)
  }

  final case class Triangle(a: Point2D, b: Point2D, c: Point2D) extends Shape2D with Area {

    override def minX: Double = Set(a, b, c).map(_.x).min

    override def maxX: Double = Set(a, b, c).map(_.x).max

    override def minY: Double = Set(a, b, c).map(_.y).min

    override def maxY: Double = Set(a, b, c).map(_.y).max

    override def x: Double = (a.x + b.x + c.x) / 3

    override def y: Double = (a.y + b.y + c.y) / 3

    override def area: Double = {
      val aSideLength = a.distanceTo(b)
      val bSideLength = b.distanceTo(c)
      val cSideLength = c.distanceTo(a)
      val semiPerimeter = (aSideLength + bSideLength + cSideLength) / 2

      math.sqrt(semiPerimeter * (semiPerimeter - aSideLength) * (semiPerimeter - bSideLength)
        * (semiPerimeter - cSideLength))
    }

    override def move(dx: Double, dy: Double): Triangle =
      Triangle(a.move(dx, dy), b.move(dx, dy), c.move(dx, dy))
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D

  // let the location be the geometric center of a shape
  sealed trait Located3D {
    def x: Double

    def y: Double

    def z: Double
  }

  sealed trait Bounded3D {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double

    def minZ: Double

    def maxZ: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  // Should we use adjectives as trait names? SurfaceArea and Volume looks more natural when defining classes extending it
  sealed trait SurfaceArea {
    def surfaceArea: Double
  }

  sealed trait Volume {
    def volume: Double
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def minZ: Double = z

    override def maxZ: Double = z

    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Sphere(center: Point3D, radius: Double) extends Shape3D with SurfaceArea with Volume {
    override def minX: Double = x - radius

    override def maxX: Double = x + radius

    override def minY: Double = y - radius

    override def maxY: Double = y + radius

    override def minZ: Double = z - radius

    override def maxZ: Double = z + radius

    override def x: Double = center.x

    override def y: Double = center.y

    override def z: Double = center.z

    override def surfaceArea: Double = 4 * math.Pi * math.pow(radius, 2)

    override def volume: Double = 4d / 3 * math.Pi * math.pow(radius, 3)

    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(center.move(dx, dy, dz), radius)
  }

  final case class Cuboid(allMinCoordinateCorner: Point3D, length: Double, height: Double, depth: Double)
    extends Shape3D with SurfaceArea with Volume {
    override def minX: Double = allMinCoordinateCorner.x

    override def maxX: Double = allMinCoordinateCorner.x + depth

    override def minY: Double = allMinCoordinateCorner.y

    override def maxY: Double = allMinCoordinateCorner.y + length

    override def minZ: Double = allMinCoordinateCorner.z

    override def maxZ: Double = allMinCoordinateCorner.z + height

    override def x: Double = allMinCoordinateCorner.x + depth / 2

    override def y: Double = allMinCoordinateCorner.y + length / 2

    override def z: Double = allMinCoordinateCorner.z + height / 2

    override def surfaceArea: Double = (length * depth + length * height + depth * height) * 2

    override def volume: Double = length * depth * height

    override def move(dx: Double, dy: Double, dz: Double): Cuboid =
      Cuboid(allMinCoordinateCorner.move(dx, dy, dz), length, height, depth)
  }

  final case class Cube(allMinCoordinateCorner: Point3D, size: Double) extends Shape3D with SurfaceArea with Volume {
    private val cuboid: Cuboid = Cuboid(allMinCoordinateCorner, size, size, size)

    override def minX: Double = cuboid.minX

    override def maxX: Double = cuboid.maxX

    override def minY: Double = cuboid.minY

    override def maxY: Double = cuboid.maxY

    override def minZ: Double = cuboid.minZ

    override def maxZ: Double = cuboid.maxZ

    override def x: Double = cuboid.x

    override def y: Double = cuboid.y

    override def z: Double = cuboid.z

    override def surfaceArea: Double = cuboid.surfaceArea

    override def volume: Double = cuboid.volume

    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(allMinCoordinateCorner.move(dx, dy, dz), size)
  }

  final case class Triangle3D(a: Point3D, b: Point3D, c: Point3D) extends Shape3D with SurfaceArea {
    override def x: Double = ???

    override def y: Double = ???

    override def z: Double = ???

    override def minX: Double = Set(a, b, c).map(_.x).min

    override def maxX: Double = Set(a, b, c).map(_.x).max

    override def minY: Double = Set(a, b, c).map(_.y).min

    override def maxY: Double = Set(a, b, c).map(_.y).max

    override def minZ: Double = Set(a, b, c).map(_.z).min

    override def maxZ: Double = Set(a, b, c).map(_.z).max

    override def surfaceArea: Double = ???

    override def move(dx: Double, dy: Double, dz: Double): Triangle3D =
      Triangle3D(a.move(dx, dy, dz), b.move(dx, dy, dz), c.move(dx, dy, dz))
  }
}
