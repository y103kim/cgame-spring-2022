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
  @inline def normalized = this / length.toInt
  @inline def truncate(size: Int) = this / size
  @inline def area = x * y
  @inline def normal = Vec2(y, -x)
  @inline def angle = Math.atan2(y, x)
  @inline def toTuple = (x, y)

  @inline def dist(that: Vec2) = (this - that).length
  @inline def hamdist(that: Vec2) = (this - that).abs.psum
  @inline def bound = 0 <= x && x <= 17630 && 0 <= y && y <= 9000
}

object Vec2 {
  @inline def apply(x: Int, y: Int) = new Vec2(x, y)
  @inline def apply(tuple: (Int, Int)) = new Vec2(tuple._1, tuple._2)
  @inline def apply(x: Int) = new Vec2(x, x)
  @inline def apply() = new Vec2(0, 0)
}

case class Nexus(health: Int, mana: Int, pos: Vec2) {
  def isNear(v: Vec2) = pos.hamdist(v) <= 5000 && pos.dist(v) <= 5000
  def dirVec(p: Vec2) = (pos - p).truncate(400)
}

object GS {
  var myNexus = Nexus(3, 0, Vec2(0, 0))
  var oppNexus = Nexus(3, 0, Vec2(17630, 9000))

  def print() = {
    Console.err.println((myNexus, oppNexus))
  }
}

class Entity(id: Int, vPos: Vec2, vVel: Vec2) {
  def validate(data: Seq[Int]) = this
  def takeTurn() = this
}
case class Hero(id: Int, vPos: Vec2, owner: Int) extends Entity(id, vPos, Vec2())
case class Enemy(id: Int, vPos: Vec2, vVel: Vec2) extends Entity(id, vPos, vVel)

class EntityPool(
    val entityMap: Map[Int, Entity] = Map(),
    val myHeros: Seq[Hero] = Seq(),
    val oppHeros: Seq[Hero] = Seq(),
    val enemies: Seq[Enemy] = Seq()
) {

  def createHero(id: Int, data: Seq[Int], owner: Int) = {
    val Seq(x, y, shield, ctrl, _*) = data
    (id, Hero(id, Vec2(x, y), owner))
  }

  def createEnemy(id: Int, data: Seq[Int]) = {
    val Seq(x, y, shield, _, _, vx, vy, _*) = data
    (id, Enemy(id, Vec2(x, y), Vec2(vx, vy)))
  }

  def regen() = {
    val ec = readLine.toInt // Amount of heros and monsters you can see
    val inputData = (0 until ec).map(x => (readLine split " ").filter(_ != "").map(_.toInt))
    val em = entityMap.mapValues(_.takeTurn())
    val newEntityMap = inputData
      .map(_ match {
        case Array(id, rest @ _*) if em.contains(id) => (id, em(id).validate(rest))
        case Array(id, 1, rest @ _*)                 => createHero(id, rest, 1)
        case Array(id, 2, rest @ _*)                 => createHero(id, rest, 2)
        case Array(id, 0, rest @ _*)                 => createEnemy(id, rest)
      })
      .toMap

    def collectHero(owner: Int) = newEntityMap.values.collect {
      case hero: Hero if hero.owner == owner => hero
    }.toSeq

    val newMyHeros = if (myHeros.size != 0) myHeros else collectHero(1)
    val newOppHeros = if (oppHeros.size != 0) oppHeros else collectHero(2)
    val enemies = newEntityMap.values.collect { case e: Enemy => e }.toSeq

    new EntityPool(newEntityMap, newMyHeros, newOppHeros, enemies)
  }

  def print() = {
    Console.err.println(s"entities[${entityMap.size}]")
    entityMap.foreach(Console.err.println)
  }
}

object Game extends App {
  val DEBUG = false
  def initNexus() = {
    val Array(baseX, baseY) = (readLine split " ").filter(_ != "").map(_.toInt)
    val heroesPerPlayer = readLine.toInt
    if (baseX != 0) {
      val temp = GS.myNexus
      GS.myNexus = GS.oppNexus;
      GS.oppNexus = temp
    }
  }

  def updateNexusStatus() = {
    val Array(h1, m1) = (readLine split " ").filter(_ != "").map(_.toInt)
    GS.myNexus = Nexus(h1, m1, GS.myNexus.pos)
    val Array(h2, m2) = (readLine split " ").filter(_ != "").map(_.toInt)
    GS.oppNexus = Nexus(h2, m2, GS.oppNexus.pos)
  }

  def loop() = {
    var pool = new EntityPool
    while (true) {
      Game.updateNexusStatus()
      GS.print()
      pool = pool.regen()
      if (DEBUG) pool.print()
      for (i <- 0 until 3)
        println("WAIT")
    }
  }
}

object Player extends App {
  Game.initNexus()
  Game.loop()
}
