import math._
import scala.util._
import scala.io.StdIn._

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
