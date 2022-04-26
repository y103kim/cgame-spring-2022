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

class Command(val heroId: Int)
case class Move(override val heroId: Int, pos: Vec2) extends Command(heroId) {
  override def toString = s"MOVE ${pos.x} ${pos.y}"
}
case class Wind(override val heroId: Int, dir: Vec2) extends Command(heroId) {
  override def toString = s"SPELL WIND ${dir.x} ${dir.y}"
}
case class Control(override val heroId: Int, enemyId: Int, dest: Vec2) extends Command(heroId) {
  override def toString = s"SPELL CONTROL ${enemyId} ${dest.x} ${dest.y}"
}
case class Shield(override val heroId: Int, entityId: Int) extends Command(heroId) {
  override def toString = s"SPELL SHIELD ${entityId}"
}
case class Wait(override val heroId: Int) extends Command(heroId) {
  override def toString = s"WAIT"
}

// Entity =========================================================================================

class Entity(val id: Int, val vPos: Vec2, val vVel: Vec2, val owner: Int = 0) {
  def validate(e: EntityInput) = true
  def takeTurn() = this
  @inline def distSq(that: Entity) = vPos.distSq(that.vPos)
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
    threatFor: Int,
    isControlled: Boolean
) extends Entity(id, vPos, vVel) {

  override def validate(e: EntityInput) =
    isControlled || (vPos == e.vPos && vVel == e.vVel && health == e.health)

  override def takeTurn(): Enemy = {
    if (trajactory.size == 1) {
      this
    } else {
      val (newPos, newVel) = trajactory.tail.head
      new Enemy(id, newPos, newVel, health, trajactory.tail, threatFor, false)
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
  def filter(owner: Int) = entityMap.filter(_._2.owner == owner)
  val enemies: Map[Int, Enemy] = filter(0).asInstanceOf[Map[Int, Enemy]]
  val heros: Map[Int, Hero] = filter(1).asInstanceOf[Map[Int, Hero]]

  def print() = {
    Console.err.println(s"entities[${entityMap.size}]")
    entityMap.foreach(Console.err.println)
  }
}

// Simulator ======================================================================================

class Simulator(gs: GameStatus, cmds: Seq[Command], inputData: IndexedSeq[EntityInput]) {
  type HMap = Map[Int, Hero]
  type EMap = Map[Int, Enemy]
  val factory = new EntityFactory(gs)

  def simulate() =
    doControl(gs.pool.heros, gs.pool.enemies)

  def doControl(heros: HMap, enemies: EMap) = {
    def check(hid: Int, eid: Int) = {
      val enemy = enemies(eid)
      !enemy.isControlled && heros(hid).distSq(enemy) <= 2200 * 2200
    }
    val newEnemies = cmds
      .filter(_.isInstanceOf[Control])
      .collect {
        case Control(hid, eid, dest) if check(hid, eid) =>
          (eid, factory.createEnemy(enemies(eid), dest, true))
      }
    doShield(heros, enemies ++ newEnemies)
  }

  def doShield(heros: HMap, enemies: EMap) = {
    moveHeroes(heros, enemies)
  }

  def moveHeroes(heros: HMap, enemies: EMap) = {
    performCombat(heros, enemies)
  }

  def performCombat(heros: HMap, enemies: EMap) = {
    doPush(heros, enemies)
  }

  def doPush(heros: HMap, enemies: EMap) = {
    moveMobs(heros, enemies)
  }

  def moveMobs(heros: HMap, enemies: EMap) = {
    shieldDecay(heros, enemies)
  }

  def shieldDecay(heros: HMap, enemies: EMap) = {
    validateWithInput(heros, enemies)
  }

  def validateWithInput(heros: HMap, enemies: EMap) = {
    val em = heros ++ enemies.mapValues(_.takeTurn())
    def check(e: EntityInput) = em.contains(e.id) && em(e.id).validate(e)
    val newEntityMap = inputData
      .map(_ match {
        case e: EntityInput if check(e)    => (e.id, em(e.id))
        case e: EntityInput if e._type > 0 => (e.id, factory.createHero(e))
        case e: EntityInput                => (e.id, factory.createEnemy(e))
      })
      .toMap
    new EntityPool(newEntityMap)
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

  val startingL = Map(
    0 -> Move(0, Vec2(5000, 5000)),
    1 -> Move(1, Vec2(2100, 6000)),
    2 -> Move(2, Vec2(6000, 2100))
  )
  val startingR = Map(
    3 -> Move(3, Vec2(12630, 4000)),
    4 -> Move(4, Vec2(11630, 6900)),
    5 -> Move(5, Vec2(15530, 3000))
  )
  def starting = if (isL) startingL else startingR
}

// Factory ========================================================================================

class EntityFactory(val gs: GameStatus) {
  def createHero(e: EntityInput) =
    Hero(e.id, e.vPos, e._type)

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

  def createEnemy(e: EntityInput) = {
    val (trajactory, threatFor) = getTraj(e.vPos, e.vVel)
    Enemy(e.id, e.vPos, e.vVel, e.health, trajactory, threatFor, false)
  }

  def createEnemy(e: Enemy, dest: Vec2, isControlled: Boolean) = {
    val vel = (dest - e.vPos).truncate(400)
    val (trajactory, threatFor) = getTraj(e.vPos, vel)
    Enemy(e.id, e.vPos, vel, e.health, trajactory, threatFor, isControlled)
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

  def heuristic(gs: GameStatus): Seq[Command] = {
    val pool = gs.pool
    var heros = pool.heros.values.toSeq
    val heroIds = pool.heros.keys.toSeq.sorted
    val dangers = pool.enemies.values.toSeq
      .filter(_.threatFor == 1)
      .sortBy(_.trajactory.size)
      .take(3)
    val mana = gs.myNexus.status.mana

    val cmds = dangers.map[Command](e => {
      heros = heros.sortBy(h => h.vPos.distSq(e.vPos))
      val hero = heros.head
      heros = heros.tail
      if (hero.distSq(e) <= 2200 * 2200 && mana >= 30 && e.health > 10) {
        Control(hero.id, e.id, gs.oppNexus.pos)
      } else {
        Move(hero.id, e.shortest(hero.vPos).map(_._1).getOrElse(e.vPos))
      }
    })
    cmds ++ heros.collect(h => gs.starting(h.id))
  }

  @tailrec
  def processTurn(gs: GameStatus, cmds: Seq[Command]): Unit = {
    val (myNexus, oppNexus) = Game.updateNexusStatus(gs)
    val factory = new EntityFactory(gs)
    val ec = readLine.toInt
    val inputData = (0 until ec).map(_ => InputHandler.handleEntity())

    val t0 = System.nanoTime()

    val simulator = new Simulator(gs, cmds, inputData)
    val pool = simulator.simulate()
    val newGs = GameStatus(myNexus, oppNexus, pool)
    val nextCmds = heuristic(newGs).sortBy(_.heroId)
    nextCmds.foreach(println)

    val t1 = System.nanoTime()
    val elapsed = (t1 - t0) / 1000000.0
    Console.err.println(s"Elapsed time: ${elapsed} ms")

    processTurn(newGs, nextCmds)
  }

  def main() = {
    val (myNexus, oppNexus) = Game.initNexus()
    var gs = GameStatus(myNexus, oppNexus)
    val sCmd = gs.starting.keys.map(i => Wait(i)).toSeq
    processTurn(gs, sCmd)
  }
}

// Player =========================================================================================

object Player extends App {
  Game.main()
}
