import math._
import scala.util._
import scala.io.StdIn._
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

// Vec2 ===========================================================================================

@inline final case class Vec2(x: Int, y: Int) {
  @inline def unary_- = Vec2(-x, -y)
  @inline def abs = Vec2(Math.abs(x), Math.abs(y))

  @inline def +(that: Vec2) = Vec2(x + that.x, y + that.y)
  @inline def +(that: Int) = Vec2(x + that, y + that)
  @inline def -(that: Vec2) = Vec2(x - that.x, y - that.y)
  @inline def -(that: Int) = Vec2(x - that, y - that)
  @inline def *(a: Int) = Vec2(x * a, y * a)
  @inline def /(a: Int) = Vec2(x / a, y / a)
  @inline def /(a: Double) = Vec2((x / a).toInt, (y / a).toInt)
  @inline def dot(that: Vec2) = x * that.x + y * that.y
  @inline def cross(that: Vec2) = x * that.y - y * that.x

  @inline def lengthSq = x * x + y * y
  @inline def length = Math.sqrt(lengthSq)
  @inline def normalized = this / length.toInt
  @inline def truncate(size: Int) = this / (length / size)
  @inline def area = x * y
  @inline def normal = Vec2(y, -x)
  @inline def angle = Math.atan2(y, x)
  @inline def toTuple = (x, y)

  @inline def dist(that: Vec2) = (this - that).length
  @inline def distSq(that: Vec2) = (this - that).lengthSq
  @inline def bound = 0 <= x && x <= 17630 && 0 <= y && y <= 9000
}

object Vec2 {
  @inline def apply(x: Int, y: Int) = new Vec2(x, y)
  @inline def apply(tuple: (Int, Int)) = new Vec2(tuple._1, tuple._2)
  @inline def apply(x: Int) = new Vec2(x, x)
  @inline def apply() = new Vec2(0, 0)
}

// Input and Output===============================================================================

case class NexusPosInput(myNexusX: Int, myNexusY: Int)
case class NexusStatus(health: Int, mana: Int)
case class EntityInput(
    id: Int,
    _type: Int,
    vPos: Vec2,
    shieldLife: Int,
    isControlled: Boolean,
    health: Int,
    vVel: Vec2,
    nearBase: Boolean,
    threatFor: Int
)

object InputHandler {
  def handleNexusPos() = {
    val Array(baseX, baseY) = (readLine split " ").filter(_ != "").map(_.toInt)
    val heroesPerPlayer = readLine.toInt
    NexusPosInput(baseX, baseY)
  }

  def handleNexusStatus() = {
    val Array(h1, m1) = (readLine split " ").filter(_ != "").map(_.toInt)
    val Array(h2, m2) = (readLine split " ").filter(_ != "").map(_.toInt)
    (NexusStatus(h1, m1), NexusStatus(h2, m2))
  }

  def handleEntity() = {
    val Array(id, _type, x, y, shieldLife, isControlled, health, vx, vy, nearBase, threatFor) =
      (readLine split " ").filter(_ != "").map(_.toInt)
    val vPos = Vec2(x, y)
    val vVel = Vec2(vx, vy)
    val ctrl = isControlled == 1
    val nb = nearBase == 1
    EntityInput(id, _type, vPos, shieldLife, ctrl, health, vVel, nb, threatFor)
  }

}

// Domain =========================================================================================

case class Nexus(health: Int, mana: Int, pos: Vec2) {
  def isNear(v: Vec2) = pos.distSq(v) <= 5000 * 5000
  def dirVec(p: Vec2) = (pos - p).truncate(400)
}

object GS {
  var myNexus = Nexus(3, 0, Vec2(0, 0))
  var oppNexus = Nexus(3, 0, Vec2(17630, 9000))

  def print() = {
    Console.err.println((myNexus, oppNexus))
  }
}

class Entity(val id: Int, val vPos: Vec2, val vVel: Vec2) {
  def validate(data: Seq[Int]) = (id, this)
  def takeTurn() = this
}

case class Hero(override val id: Int, override val vPos: Vec2, owner: Int)
    extends Entity(id, vPos, Vec2())

case class Enemy(
    override val id: Int,
    override val vPos: Vec2,
    override val vVel: Vec2,
    trajactory: Queue[(Vec2, Vec2)],
    threatFor: Int
) extends Entity(id, vPos, vVel) {

  override def validate(data: Seq[Int]): (Int, Entity) = {
    val Seq(x, y, shield, _, _, vx, vy, _, tf) = data
    if (vPos != Vec2(x, y))
      return EntityFactory.createEnemy(id, data)
    else
      return (id, this)
  }

  override def takeTurn(): Enemy = {
    val (newPos, newVel) = trajactory.tail.head
    new Enemy(id, newPos, newVel, trajactory.tail, threatFor)
  }

  override def toString() = s"E${id}, vPos=${vPos} vVel=${vVel} threatFor=${threatFor}"
}

object EntityFactory {
  def createHero(id: Int, data: Seq[Int], owner: Int) = {
    val Seq(x, y, shield, ctrl, _*) = data
    (id, Hero(id, Vec2(x, y), owner))
  }

  def createEnemy(id: Int, data: Seq[Int]) = {
    val Seq(x, y, shield, _, _, vx, vy, _*) = data
    val vPos = Vec2(x, y)
    val vVel = Vec2(vx, vy)

    def getTraj(curr: Vec2, vel: Vec2): (Queue[(Vec2, Vec2)], Int) = {
      @tailrec
      def getTrajR(
          curr: Vec2,
          vel: Vec2,
          q: Queue[(Vec2, Vec2)],
          tf: Int
      ): (Queue[(Vec2, Vec2)], Int) = {
        if (!curr.bound)
          (q, tf)
        else if (GS.myNexus.isNear(curr + vel)) {
          val newVel = GS.myNexus.dirVec(curr + vel)
          getTrajR(curr + vel, newVel, q :+ (curr, vel), 1)
        } else if (GS.oppNexus.isNear(curr + vel)) {
          val newVel = GS.oppNexus.dirVec(curr + vel)
          getTrajR(curr + vel, newVel, q :+ (curr, vel), 2)
        } else
          getTrajR(curr + vel, vel, q :+ (curr, vel), tf)
      }
      getTrajR(curr, vel, Queue(), 0)
    }

    val (trajactory, threatFor) = getTraj(vPos, vVel)
    (id, Enemy(id, vPos, vVel, trajactory, threatFor))
  }
}

class EntityPool(
    val entityMap: Map[Int, Entity] = Map(),
    val myHeros: Seq[Hero] = Seq(),
    val oppHeros: Seq[Hero] = Seq(),
    val enemies: Seq[Enemy] = Seq()
) {

  def regen() = {
    val ec = readLine.toInt // Amount of heros and monsters you can see
    val inputData = (0 until ec).map(x => (readLine split " ").filter(_ != "").map(_.toInt))
    val em = entityMap.mapValues(_.takeTurn())
    val newEntityMap = inputData
      .map(_ match {
        case Array(id, _, rest @ _*) if em.contains(id) => em(id).validate(rest)
        case Array(id, 1, rest @ _*)                    => EntityFactory.createHero(id, rest, 1)
        case Array(id, 2, rest @ _*)                    => EntityFactory.createHero(id, rest, 2)
        case Array(id, 0, rest @ _*)                    => EntityFactory.createEnemy(id, rest)
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

// Game ===========================================================================================

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
      var heros = pool.myHeros
      val heroIds = pool.myHeros.map(_.id)
      val moves = mutable.Map[Int, Vec2]()
      val dangers = pool.enemies
        .filter(_.threatFor == 1)
        .sortBy(_.trajactory.size)
        .take(3)
        .foreach(e => {
          heros = heros.sortBy(h => h.vPos.dist(e.vPos))
          moves(heros.head.id) = e.vPos
          heros = heros.tail
        })

      for (i <- heroIds)
        if (moves.contains(i))
          println(s"MOVE ${moves(i).x} ${moves(i).y}")
        else if (GS.myNexus.pos.x == 0)
          println(s"MOVE 3535 3535")
        else
          println(s"MOVE 14095 5465")
    }
  }
}

// Player =========================================================================================

object Player extends App {
  Game.initNexus()
  Game.loop()
}
