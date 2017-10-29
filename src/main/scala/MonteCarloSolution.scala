import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by benjaminsmith on 10/28/17.
  */

object TestData {

  val player1 = Player(1)
  val player2 = Player(2)
  val player3 = Player(4)
  val player4 = Player(10)

  val allPlayers = List(player1, player2, player3, player4)

  val agent1 = RandomAgent(Nil)
  val agent2 = MaxPointsAgent(Nil)

  val allAgents = List(agent2, agent1)
}


object FantasyBasketball {
  def main(args: Array[String]): Unit = {
    val startingEnv = Environment(TestData.allPlayers)

    val startingAgents = TestData.allAgents

    val (_, newAgents) = draft(startingEnv, startingAgents)

    val scorer = MaxPointsScorer

    val winner = scorer.pickWinner(newAgents)

    println("Winner", winner)

  }

  //TODO: Enable multi round drafts
  @tailrec
  def draft(environment: Environment, inAgents:List[Agent], outAgents:List[Agent] = Nil):(Environment, List[Agent]) = {
    inAgents match {
      case Nil => (environment, outAgents)
      case h :: t => {
        val allOtherAgents = t ++ outAgents
        val (newEnv, newAgent) = h.action(environment, allOtherAgents)
        draft(newEnv, t, outAgents :+ newAgent)
      }
    }
  }
}

object Math {
  def argMax(l:List[Int]):Int = {
    val (_, i) = l.zipWithIndex.foldLeft((0,-1)){ case (prev@(prevV, _), (vAndi)) =>
      val (value, i ) = vAndi
      if (value > prevV){
        vAndi
      }
      else {
        prev
      }
    }

    i
  }
}

case class Player(points:Int)

case class Environment(players:List[Player])

trait Scorer {
  def pickWinner(agents:List[Agent]):Agent
}

object MaxPointsScorer extends Scorer {
  def pickWinner(agents: List[Agent]): Agent = {
    val winner:Int = Math.argMax(agents.map(_.players.map(_.points).sum))
    agents(winner)
  }
}

trait Agent {
  def players:List[Player]

  def action(env:Environment, otherAgents:List[Agent]):(Environment, Agent)
}

case class RandomAgent(override val players:List[Player] = Nil) extends Agent{
  def action(environment: Environment, otherAgents:List[Agent]):(Environment, RandomAgent) = {
    val envPlayers = environment.players
    val randomN = Random.nextInt(envPlayers.length - 1)
    val selectedPlayer = envPlayers(randomN)

    //Abstract
    val remainingPlayers = envPlayers.patch(randomN, Nil, 1)
    val newAgent = this.copy(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }
}

case class MaxPointsAgent(override val players:List[Player] = Nil) extends Agent{
  def action(environment: Environment, otherAgents:List[Agent]):(Environment, MaxPointsAgent) = {
    val envPlayers = environment.players
    val selectedPlayerIndex:Int = Math.argMax(envPlayers.map(_.points))
    val selectedPlayer = envPlayers(selectedPlayerIndex)

    //Abstract
    val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
    val newAgent = this.copy(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }
}