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
  def argMax(l:Seq[Int]):Int = {
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
  def apply(players:List[Player]):Agent

  def players:List[Player]

  def action(env:Environment, otherAgents:List[Agent]):(Environment, Agent)

  def randomAction(env:Environment):(Environment, Agent) = {
    val envPlayers = env.players
    val randomN = Random.nextInt(envPlayers.length)
    val selectedPlayer = envPlayers(randomN)

    //Abstract
    val remainingPlayers = envPlayers.patch(randomN, Nil, 1)
    val newAgent = apply(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }
}

case class RandomAgent(override val players:List[Player] = Nil) extends Agent{
  def apply(players: List[Player]): Agent = RandomAgent(players)
  def action(environment: Environment, otherAgents:List[Agent]):(Environment, Agent) =
    randomAction(environment)
}

case class MaxPointsAgent(override val players:List[Player] = Nil) extends Agent{
  def apply(players: List[Player]): Agent = MaxPointsAgent(players)
  def action(environment: Environment, otherAgents:List[Agent]):(Environment, Agent) = {
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

object MaxScoreMonteCarloAgent {
  def update(score:Map[Player, Int], player:Player, value:Int):Map[Player, Int] =
    score + (player -> score.get(player).map(_ + value).getOrElse(value))
}

case class MaxScoreMonteCarloAgent(override val players:List[Player] = Nil, iters:Int = 100) extends Agent{
  import MaxScoreMonteCarloAgent._

  val playerOrdering = new Ordering[(Player, Int)] {
    override def compare(x: (Player, Int), y: (Player, Int)): Int = {
      if(x._2 > y._2) 1
      else if (x._2 < y._2) -1
      else 0
    }
  }

  def apply(players: List[Player]): Agent = MaxScoreMonteCarloAgent(players)
  def action(environment: Environment, otherAgents:List[Agent]):(Environment, Agent) = {
    val score = randomDraftAndScore(environment, otherAgents, Map(), iters)
    val (selectedPlayer, _ )= score.toList.max(playerOrdering)
    val envPlayers = environment.players
    val selectedPlayerIndex = envPlayers.indexOf(selectedPlayer)
    val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
    val newAgent = this.copy(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }

  @tailrec
  private def randomDraftAndScore(environment: Environment, otherAgents:List[Agent], score:Map[Player, Int], iters:Int): Map[Player, Int] = {
    if (iters > 0) {
      //We draft randomly
      val (newEnv, newAgent) = randomAction(environment)
      //All other agents draft randomly with updated env
      val (_, agents) = randomDraft(newEnv, otherAgents)
      //We score the draft
      val winner = MaxPointsScorer.pickWinner(agents :+ newAgent)
      //We figure out what player we drafted
      val newPlayer = newAgent
        .players
        .find( p => !players.contains(p))
        .getOrElse( throw new Exception("There was no new player"))

      val newScore = {
        //If we win, we add a new point to the player who we drafted and we won with
        if( winner == newAgent) update(score, newPlayer, 1)
        //If we lose, we subtract a point (don't necessarily need to do this, we could just add 0)
        else update(score, newPlayer, -1)
      }
      // We run the draft again with the updated score
      randomDraftAndScore(environment, otherAgents, newScore, iters - 1 )
    }
    //At the end of the iterations we return the score
    else score
  }

  @tailrec
  private def randomDraft(environment: Environment, inAgents:List[Agent], outAgents:List[Agent] = Nil):(Environment, List[Agent]) = {
    inAgents match {
      case Nil => (environment, outAgents)
      case h :: t => {
        val (newEnv, newAgent) = h.randomAction(environment)
        randomDraft(newEnv, t, outAgents :+ newAgent)
      }
    }
  }
}