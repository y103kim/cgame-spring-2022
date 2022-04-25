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
    Vec2(baseX, baseY)
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

// Commands =======================================================================================

class Command
case class Move(pos: Vec2) extends Command {
  override def toString = s"MOVE ${pos.x} ${pos.y}"
}
case class Wind(dir: Vec2) extends Command {
  override def toString = s"WIND ${dir.x} ${dir.y}"
}
case class Control(e: Enemy, dest: Vec2) extends Command {
  override def toString = s"CONTROL ${e.id} ${dest.x} ${dest.y}"
}
case class Shield(e: Entity) extends Command {
  override def toString = s"SHIELD ${e.id}"
}
case object Wait extends Command {
  override def toString = s"WAIT"
}

// Entity =========================================================================================

class Entity(val id: Int, val vPos: Vec2, val vVel: Vec2, val owner: Int = 0) {
  def validate(e: EntityInput) = true
  def takeTurn() = this
}

case class Hero(
    override val id: Int,
    override val vPos: Vec2,
    override val owner: Int
) extends Entity(id, vPos, Vec2(), owner) {
  override def validate(e: EntityInput) = false
}

case class Enemy(
    override val id: Int,
    override val vPos: Vec2,
    override val vVel: Vec2,
    health: Int,
    trajactory: Queue[(Vec2, Vec2)],
    threatFor: Int
) extends Entity(id, vPos, vVel) {

  override def validate(e: EntityInput) = vPos == e.vPos && vVel == e.vVel && health == e.health

  override def takeTurn(): Enemy = {
    if (trajactory.size == 1) {
      this
    } else {
      val (newPos, newVel) = trajactory.tail.head
      new Enemy(id, newPos, newVel, health, trajactory.tail, threatFor)
    }
  }

  def shortest(vHero: Vec2) =
    trajactory
      .map(_._1)
      .zipWithIndex
      .find { case (p, i) => p.distSq(vHero) <= 800 * 800 * (i + 1) * (i + 1) }

  override def toString() =
    s"[E${id}] vPos=${vPos} vVel=${vVel} threatFor=${threatFor} owner=${owner}"
}

class EntityPool(val entityMap: Map[Int, Entity] = Map()) {

  def getEntities = {
    def filter(owner: Int) = entityMap.filter(_._2.owner == owner)
    val enemies: Map[Int, Enemy] = filter(0).asInstanceOf[Map[Int, Enemy]]
    val heros: Map[Int, Hero] = filter(1).asInstanceOf[Map[Int, Hero]]
    (heros, enemies)
  }

  def print() = {
    Console.err.println(s"entities[${entityMap.size}]")
    entityMap.foreach(Console.err.println)
  }
}

// Simulator ======================================================================================

class Simulator(gs: GameStatus, cmds: Map[Int, Command], inputData: IndexedSeq[EntityInput]) {
  val factory = new EntityFactory(gs)

  def simulate() = {
    var (heros, enemies) = gs.pool.getEntities
    doControl(heros, enemies)
  }

  def doControl(heros: Map[Int, Hero], enemies: Map[Int, Enemy]) = {
    doShield(heros, enemies)
  }

  def doShield(heros: Map[Int, Hero], enemies: Map[Int, Enemy]) = {
    moveHeroes(heros, enemies)
  }

  def moveHeroes(heros: Map[Int, Hero], enemies: Map[Int, Enemy]) = {
    performCombat(heros, enemies)
  }

  def performCombat(heros: Map[Int, Hero], enemies: Map[Int, Enemy]) = {
    doPush(heros, enemies)
  }

  def doPush(heros: Map[Int, Hero], enemies: Map[Int, Enemy]) = {
    moveMobs(heros, enemies)
  }

  def moveMobs(heros: Map[Int, Hero], enemies: Map[Int, Enemy]) = {
    shieldDecay(heros, enemies)
  }

  def shieldDecay(heros: Map[Int, Hero], enemies: Map[Int, Enemy]) = {
    validateWithInput(heros, enemies)
  }

  def validateWithInput(heros: Map[Int, Hero], enemies: Map[Int, Enemy]) = {
    gs
  }
}

// Game Status ====================================================================================

case class Nexus(pos: Vec2, status: NexusStatus = NexusStatus(3, 0)) {
  def isNear(v: Vec2) = pos.distSq(v) <= 5000 * 5000
  def dirVec(p: Vec2) = (pos - p).truncate(400)
  def withStatus(newStatus: NexusStatus) =
    Nexus(pos, newStatus)
}

case class GameStatus(
    val myNexus: Nexus,
    val oppNexus: Nexus,
    val pool: EntityPool = new EntityPool
) {
  def isL = myNexus.pos.x == 0
  def withPool(newPool: EntityPool) = GameStatus(myNexus, oppNexus, newPool)

  val startingL = List(Vec2(5000, 5000), Vec2(2100, 6000), Vec2(6000, 2100))
  val startingR = startingL.map(Vec2(17630, 9000) - _)
  def starting = if (isL) startingL else startingR
}

// Factory ========================================================================================

class EntityFactory(val gs: GameStatus) {
  def createHero(e: EntityInput) =
    Hero(e.id, e.vPos, e._type)

  def createEnemy(e: EntityInput) = {
    def getTraj(curr: Vec2, vel: Vec2): (Queue[(Vec2, Vec2)], Int) = {
      @tailrec
      def getTrajR(
          curr: Vec2,
          vel: Vec2,
          q: Queue[(Vec2, Vec2)],
          tf: Int
      ): (Queue[(Vec2, Vec2)], Int) = {
        if (!curr.bound || curr == Vec2() || curr == Vec2(17630, 9000))
          (q, tf)
        else if (gs.myNexus.isNear(curr + vel)) {
          val newVel = gs.myNexus.dirVec(curr + vel)
          getTrajR(curr + vel, newVel, q :+ (curr, vel), 1)
        } else if (gs.oppNexus.isNear(curr + vel)) {
          val newVel = gs.oppNexus.dirVec(curr + vel)
          getTrajR(curr + vel, newVel, q :+ (curr, vel), 2)
        } else
          getTrajR(curr + vel, vel, q :+ (curr, vel), tf)
      }
      getTrajR(curr, vel, Queue(), 0)
    }

    val (trajactory, threatFor) = getTraj(e.vPos, e.vVel)
    Enemy(e.id, e.vPos, e.vVel, e.health, trajactory, threatFor)
  }
}

// Game ===========================================================================================

object Game {
  val DEBUG = false
  def initNexus() = {
    val myPos = InputHandler.handleNexusPos()
    val oppPos = if (myPos.x == 0) Vec2(17630, 9000) else Vec2(0, 0)
    (Nexus(myPos), Nexus(oppPos))
  }

  def updateNexusStatus(gs: GameStatus) = {
    val (sMy, sOpp) = InputHandler.handleNexusStatus()
    (gs.myNexus.withStatus(sMy), gs.oppNexus.withStatus(sOpp))
  }

  @tailrec
  def simulate(gs: GameStatus): Unit = {
    val (myNexus, oppNexus) = Game.updateNexusStatus(gs)
    val factory = new EntityFactory(gs)
    val ec = readLine.toInt
    val inputData = (0 until ec).map(_ => InputHandler.handleEntity())

    val t0 = System.nanoTime()

    val simulator = new Simulator(gs)
    val pool = simulator.sim(Map(), inputData)
    var heros = pool.myHeros
    val heroIds = heros.map(_.id).sorted
    val st = gs.starting
    val moves = mutable.Map[Int, Vec2]().addAll(heroIds.zip(st))
    val ctrls = mutable.Map[Int, Int]()
    val dangers = pool.enemies
      .filter(_.threatFor == 1)
      .sortBy(_.trajactory.size)
      .take(3)

    dangers.foreach(e => {
      heros = heros.sortBy(h => h.vPos.distSq(e.vPos))
      moves(heros.head.id) = e.shortest(heros.head.vPos).map(_._1).getOrElse(e.vPos)
      if (
        heros.head.vPos.distSq(e.vPos) <= 2200 * 2200 &&
        myNexus.status.mana >= 30 &&
        e.health > 10
      ) {
        moves.remove(heros.head.id)
        ctrls(heros.head.id) = e.id
      }
      heros = heros.tail
    })

    val newGs = GameStatus(myNexus, oppNexus, pool)

    // decide heros' movements
    for (i <- heroIds)
      if (moves.contains(i))
        println(s"MOVE ${moves(i).x} ${moves(i).y}")
      else
        println(s"SPELL CONTROL ${ctrls(i)} ${gs.oppNexus.pos.x} ${gs.oppNexus.pos.y}")

    val t1 = System.nanoTime()
    val elapsed = (t1 - t0) / 1000000.0
    Console.err.println(s"Elapsed time: ${elapsed} ms")

    simulate(newGs)
  }

  def main() = {
    val (myNexus, oppNexus) = Game.initNexus()
    var gs = GameStatus(myNexus, oppNexus)
    simulate(gs)
  }
}

// Player =========================================================================================

object Player extends App {
  Game.main()
}
