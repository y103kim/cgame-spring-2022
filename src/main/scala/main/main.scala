import math._
import scala.util._
import scala.io.StdIn._
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

// Vec2 ===========================================================================================

@inline final case class Vec2(x: Double, y: Double) {
  @inline def unary_- = Vec2(-x, -y)
  @inline def abs = Vec2(Math.abs(x), Math.abs(y))

  @inline def +(that: Vec2) = Vec2(x + that.x, y + that.y)
  @inline def +(that: Double) = Vec2(x + that, y + that)
  @inline def -(that: Vec2) = Vec2(x - that.x, y - that.y)
  @inline def -(that: Double) = Vec2(x - that, y - that)
  @inline def *(a: Double) = Vec2(x * a, y * a)
  @inline def /(a: Double) = Vec2(x / a, y / a)
  @inline def dot(that: Vec2) = x * that.x + y * that.y
  @inline def cross(that: Vec2) = x * that.y - y * that.x

  @inline def lengthSq = x * x + y * y
  @inline def length = Math.sqrt(lengthSq)
  @inline def unit = this / length
  @inline def normalize(size: Double) = this / length * size
  @inline def area = x * y
  @inline def normal = Vec2(y, -x)
  @inline def angle = Math.atan2(y, x)
  @inline def toTuple = (x, y)

  @inline def dist(that: Vec2) = (this - that).length
  @inline def distSq(that: Vec2) = (this - that).lengthSq
  @inline def bound = 0 <= x && x <= 17630 && 0 <= y && y <= 9000
  @inline def truncate = Vec2(x.toInt, y.toInt)
  @inline def symTruncate = (this - Vec2.CENTER).truncate + Vec2.CENTER
}

object Vec2 {
  @inline def apply(x: Double, y: Double) = new Vec2(x, y)
  @inline def apply(tuple: (Double, Double)) = new Vec2(tuple._1, tuple._2)
  @inline def apply(x: Double) = new Vec2(x, x)
  @inline def apply() = new Vec2(0, 0)
  val CENTER = Vec2(17630 / 2, 9000 / 2)
}

// Input and Output===============================================================================

case class Status(health: Int, mana: Int) {
  def withGained(gained: Int) = Status(health, mana + gained)
}
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

  def handleStatus() = {
    val Array(h1, m1) = (readLine split " ").filter(_ != "").map(_.toInt)
    val Array(h2, m2) = (readLine split " ").filter(_ != "").map(_.toInt)
    (Status(h1, m1), Status(h2, m2))
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
  override def toString = s"MOVE ${pos.x.toInt} ${pos.y.toInt}"
}
case class Wind(override val heroId: Int, dir: Vec2) extends Command(heroId) {
  override def toString = s"SPELL WIND ${dir.x.toInt} ${dir.y.toInt}"
}
case class Control(override val heroId: Int, enemyId: Int, dest: Vec2) extends Command(heroId) {
  override def toString = s"SPELL CONTROL ${enemyId} ${dest.x.toInt} ${dest.y.toInt}"
}
case class Shield(override val heroId: Int, enemyId: Int) extends Command(heroId) {
  override def toString = s"SPELL SHIELD ${enemyId}"
}
case class Wait(override val heroId: Int) extends Command(heroId) {
  override def toString = s"WAIT"
}

// Entity =========================================================================================

class Entity(
    val id: Int,
    val vPosRaw: Vec2,
    val vVel: Vec2,
    val owner: Int = 0,
    val shieldLife: Int = 0
) {
  val vPos: Vec2 = vPosRaw.symTruncate
  def validate(e: EntityInput) = true
  def takeTurn() = this
  @inline def distSq(that: Entity) = vPos.distSq(that.vPos)
}

case class Hero(
    override val id: Int,
    override val vPosRaw: Vec2,
    override val owner: Int,
    override val shieldLife: Int = 0
) extends Entity(id, vPosRaw, Vec2(), owner, shieldLife) {
  override def validate(e: EntityInput) = {
    val res = vPos == e.vPos
    if (!res) {
      Console.err.println("Validation Fail for hero")
      Console.err.println(s"  entity: ${this}")
      Console.err.println(s"  input: ${e}")
    }
    res
  }
  def withShield() = Hero(id, vPos, owner, 12)
  def withPos(pos: Vec2) = Hero(id, pos, owner, shieldLife)
}

case class Enemy(
    override val id: Int,
    override val vPosRaw: Vec2,
    override val vVel: Vec2,
    health: Int,
    trajactory: Queue[(Vec2, Vec2)],
    threatFor: Int,
    isControlled: Boolean,
    override val shieldLife: Int = 0
) extends Entity(id, vPosRaw, vVel) {
  override def validate(e: EntityInput) = {
    val res = isControlled || (vPos == e.vPos && vVel == e.vVel && health == e.health)
    if (!res) {
      Console.err.println("Validation Fail for Enemy")
      Console.err.println(s"  entity: ${this}")
      Console.err.println(s"  input: ${e}")
    }
    res
  }

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

  def withShield() = Enemy(id, vPos, vVel, health, trajactory, threatFor, isControlled, 12)

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

  def checkSpell(hero: Hero, enemy: Enemy) = {
    val distSq = hero.distSq(enemy)
    !enemy.isControlled && enemy.shieldLife == 0 && distSq <= 2200 * 2200
  }

  def doControl(heros: HMap, enemies: EMap) = {
    val newEnemies = cmds.collect {
      case Control(hid, eid, dest) if checkSpell(heros(hid), enemies(eid)) =>
        (eid, factory.createEnemy(enemies(eid), dest, true))
    }
    doShield(heros, enemies ++ newEnemies)
  }

  def doShield(heros: HMap, enemies: EMap) = {
    // TODO: need to verify working
    val newHeros = cmds.collect {
      case Shield(hid, eid) if hid == eid =>
        (hid, heros(hid).withShield())
    }
    val newEnemies = cmds.collect {
      case Shield(hid, eid) if checkSpell(heros(hid), enemies(eid)) =>
        (hid, enemies(eid).withShield())
    }
    moveHeroes(heros ++ newHeros, enemies ++ newEnemies)
  }

  def moveHeroes(heros: HMap, enemies: EMap) = {
    val newHeros = cmds.collect {
      case Move(hid, dest) => {
        val hero = heros(hid)
        val distSq = hero.vPos.distSq(dest)
        def vel = (dest - hero.vPos).normalize(800)
        val newPos = if (distSq > 800 * 800) hero.vPos + vel else dest
        (hid, heros(hid).withPos(newPos))
      }
    }
    performCombat(heros ++ newHeros, enemies)
  }

  def performCombat(heros: HMap, enemies: EMap) = {
    doPush(heros, enemies, 0)
  }

  def doPush(heros: HMap, enemies: EMap, gained: Int) = {
    moveMobs(heros, enemies, gained)
  }

  def moveMobs(heros: HMap, enemies: EMap, gained: Int) = {
    shieldDecay(heros, enemies, gained)
  }

  def shieldDecay(heros: HMap, enemies: EMap, gained: Int) = {
    validateWithInput(heros, enemies, gained)
  }

  def validateWithInput(heros: HMap, enemies: EMap, gained: Int) = {
    val em = heros ++ enemies.mapValues(_.takeTurn())
    def check(e: EntityInput) = em.contains(e.id) && em(e.id).validate(e)
    val newEntityMap = inputData
      .map(_ match {
        case e: EntityInput if check(e)    => (e.id, em(e.id))
        case e: EntityInput if e._type > 0 => (e.id, factory.createHero(e))
        case e: EntityInput                => (e.id, factory.createEnemy(e))
      })
      .toMap
    gs.withPoolAndGained(new EntityPool(newEntityMap), gained)
  }
}

// Game Status ====================================================================================

case class Nexus(pos: Vec2) {
  def isNear(v: Vec2) = pos.distSq(v) <= 5000 * 5000
  def dirVec(p: Vec2) = (pos - p).normalize(400)
}

case class GameStatus(
    val myNexus: Nexus,
    val myStatus: Status,
    val oppNexus: Nexus,
    val oppStatus: Status,
    val pool: EntityPool = new EntityPool
) {
  def isL = myNexus.pos.x == 0
  def withPoolAndGained(newPool: EntityPool, gained: Int) =
    GameStatus(myNexus, myStatus.withGained(gained), oppNexus, oppStatus, newPool)
  def withStatus(myS: Status, oppS: Status) =
    GameStatus(myNexus, myS, oppNexus, oppS, pool)

  val startingL = Map(
    0 -> Move(0, Vec2(5000, 5000)),
    1 -> Move(1, Vec2(6000, 2100)),
    2 -> Move(2, Vec2(2100, 6000))
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
    val vel = (dest - e.vPos).normalize(400)
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

  def heuristic(gs: GameStatus): Seq[Command] = {
    val pool = gs.pool
    var heros = pool.heros.values.toSeq
    val heroIds = pool.heros.keys.toSeq.sorted
    val dangers = pool.enemies.values.toSeq
      .filter(_.threatFor == 1)
      .sortBy(_.trajactory.size)
      .take(3)
    val mana = gs.myStatus.mana

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
  def processTurn(gsPrev: GameStatus, cmds: Seq[Command]): Unit = {
    val (myStatus, oppStatus) = InputHandler.handleStatus
    val gs = gsPrev.withStatus(myStatus, oppStatus)
    val factory = new EntityFactory(gs)
    val ec = readLine.toInt
    val inputData = (0 until ec).map(_ => InputHandler.handleEntity())

    val t0 = System.nanoTime()

    val simulator = new Simulator(gs, cmds, inputData)
    val newGs = simulator.simulate()
    val nextCmds = heuristic(newGs).sortBy(_.heroId)
    nextCmds.foreach(println)

    val t1 = System.nanoTime()
    val elapsed = (t1 - t0) / 1000000.0
    Console.err.println(s"Elapsed time: ${elapsed} ms")

    processTurn(newGs, nextCmds)
  }

  def main() = {
    val (myNexus, oppNexus) = Game.initNexus()
    var gs = GameStatus(myNexus, Status(3, 0), oppNexus, Status(3, 0))
    val sCmd = gs.starting.keys.map(i => Wait(i)).toSeq
    processTurn(gs, sCmd)
  }
}

// Player =========================================================================================

object Player extends App {
  Game.main()
}
