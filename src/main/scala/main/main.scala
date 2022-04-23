import math._
import scala.util._
import scala.io.StdIn._

@inline final case class Vec2(x: Int, y: Int) {
  @inline def unary_- = Vec2(-x, -y)
  @inline def abs = Vec2(Math.abs(x), Math.abs(y))
  @inline def psum = x + y

  @inline def +(that: Vec2) = Vec2(x + that.x, y + that.y)
  @inline def +(that: Int) = Vec2(x + that, y + that)
  @inline def -(that: Vec2) = Vec2(x - that.x, y - that.y)
  @inline def -(that: Int) = Vec2(x - that, y - that)
  @inline def *(a: Int) = Vec2(x * a, y * a)
  @inline def /(a: Int) = Vec2(x / a, y / a)
  @inline def dot(that: Vec2) = x * that.x + y * that.y
  @inline def cross(that: Vec2) = x * that.y - y * that.x

  @inline def lengthSq = x * x + y * y
  @inline def length = Math.sqrt(lengthSq)
  @inline def normalized = this / length.toLong
  @inline def truncate(size: Int) = this / size
  @inline def area = x * y
  @inline def normal = Vec2(y, -x)
  @inline def angle = Math.atan2(y, x)
  @inline def toTuple = (x, y)

  @inline def dist(that: Vec2) = (this - that).length
  @inline def hamdist(that: Vec2) = (this - that).abs.psum
}

object Vec2 {
  @inline def apply(x: Int, y: Int) = new Vec2(x, y)
  @inline def apply(tuple: (Int, Int)) = new Vec2(tuple._1, tuple._2)
  @inline def apply(x: Int) = new Vec2(x, x)
}

object Player extends App {
  // baseX: The corner of the map representing your base
  val Array(baseX, baseY) = (readLine split " ").filter(_ != "").map(_.toInt)
  val heroesPerPlayer = readLine.toInt // Always 3

  // game loop
  while (true) {
    for (i <- 0 until 2) {
      // health: Each player's base health
      // mana: Ignore in the first league; Spend ten mana to cast a spell
      val Array(health, mana) =
        (readLine split " ").filter(_ != "").map(_.toInt)
    }
    val entityCount = readLine.toInt // Amount of heros and monsters you can see
    for (i <- 0 until entityCount) {
      // id: Unique identifier
      // _type: 0=monster, 1=your hero, 2=opponent hero
      // x: Position of this entity
      // shieldLife: Ignore for this league; Count down until shield spell fades
      // isControlled: Ignore for this league; Equals 1 when this entity is under a control spell
      // health: Remaining health of this monster
      // vx: Trajectory of this monster
      // nearBase: 0=monster with no target yet, 1=monster targeting a base
      // threatFor: Given this monster's trajectory, is it a threat to 1=your base, 2=your opponent's base, 0=neither
      val Array(
        id,
        _type,
        x,
        y,
        shieldLife,
        isControlled,
        health,
        vx,
        vy,
        nearBase,
        threatFor
      ) = (readLine split " ").filter(_ != "").map(_.toInt)
    }
    for (i <- 0 until heroesPerPlayer) {

      // Write an action using println
      // To debug: Console.err.println("Debug messages...")

      // In the first league: MOVE <x> <y> | WAIT; In later leagues: | SPELL <spellParams>;
      println("WAIT")
    }
  }
}
