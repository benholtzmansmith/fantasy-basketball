import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by benjaminsmith on 10/28/17.
  */

/**
  * TODO:
  *   Don't have a measurement of how much better 4 points is 2 points if they both beat 1 point
  *
  *   Some agents getting the same players multi agents end up
  * */

object TestData {

  val player1 = Player(points = 1, assists = 8, rebounds = 10)
  val player2 = Player(points = 2, assists = 3, rebounds = 8)
  val player3 = Player(points = 4, assists = 4, rebounds = 6)
  val player4 = Player(points = 10, assists = 2, rebounds = 1)

  val allPlayers = List(player1, player2, player3, player4)

  val agent1 = RandomAgent(Nil)
  val agent2 = MaxPointsAgent(Nil)
  val agent3 = MaxAllMonteCarloAgent(Nil, iters = 200)

  val allAgents = List(agent3, agent2)


  def playersGenerator(n:Int):List[Player] = {
    def genValue = (math.pow(Random.nextDouble(), 2) * 100).toInt
    List.fill(n){
      Player(
        points = 2,
        assists = genValue,
        rebounds = genValue,
        steals = genValue,
        blocks = genValue,
        freeThrowAttempts = genValue,
        threePointMakes = genValue,
        turnovers = genValue
      )
    }
  }
}


object FantasyBasketball {
  def main(args: Array[String]): Unit = {
    val startingPlayers = TestData.playersGenerator(10)

    val startingEnv = Environment(startingPlayers)

    val startingAgents:List[Agent] = scala.util.Random.shuffle(TestData.allAgents)

    val scoreMap = runGameN(startingEnv,startingAgents, 1000, Map())

    val winner = scoreMap.toList.max(MaxPointsMonteCarloAgent.tupleOrdering)

    println(
      s"""
         |Winner: ${startingAgents.find(_.name == winner._1).get.name}
         |
         |All Agents:
         |${scoreMap.toList.map{case (agent, score) =>
            s"""
               |agent:${startingAgents.find(_.name == agent).get.name}
               |score:${score}
               |""".stripMargin
            }
          }
         |
       """.stripMargin)

  }
  @tailrec
  def runGameN(startingEnv:Environment, startingAgents:List[Agent], runCount:Int, scoreTracker:Map[String, Int]):Map[String, Int] = {
    if (runCount > 0){
      val (_, newAgents) = draft(startingEnv, startingAgents)

      val winner = MaxAllScorer.pickWinner(newAgents)

      val newMap = MaxPointsMonteCarloAgent.update(scoreTracker, winner.name, 1)

      runGameN(startingEnv, startingAgents, runCount - 1, newMap)
    }
    else scoreTracker
  }

  //TODO: Enable multi round drafts
  @tailrec
  def draft(environment: Environment, inAgents:List[Agent], outAgents:List[Agent] = Nil, remainingRounds:Int = 1):(Environment, List[Agent]) = {
    assert(environment.players.length >= inAgents.length * remainingRounds, "fewer players then agents * remainingRounds")
    inAgents match {
      case _ if remainingRounds == 0 => (environment, outAgents)
      case Nil => draft(environment, outAgents, Nil, remainingRounds - 1)
      case h :: t => {
        val allOtherAgents = t ++ outAgents
        val (newEnv, newAgent) = h.action(environment, allOtherAgents)
        draft(newEnv, t, outAgents :+ newAgent)
      }
    }
  }
}

object Math {
  def argMax(l:Seq[Int]):Option[Int] = {
    val (_, i) = l.zipWithIndex.foldLeft((0,None:Option[Int])){ case (prev@(prevV, _), (vAndi)) =>
      val (value, i ) = vAndi
      if (value > prevV){
        (value, Some(i))
      }
      else {
        prev
      }
    }

    i
  }

  def argMin(l:Seq[Int]):Option[Int] = {
    l match {
      case Nil => None
      case h :: Nil => Some(h)
      case h :: t =>
        val (_, i) = t.zipWithIndex.foldLeft((h, 0)) { case (prev@(prevV, _), (vAndi)) =>
          val (value, i) = vAndi
          if (value < prevV) {
            (value, i + 1)
          }
          else {
            prev
          }
        }

        Some(i)
    }
  }
}

case class Player(
     points:Int = 0,
     assists:Int = 0,
     rebounds:Int = 0,
     steals: Int = 0,
     blocks:Int = 0,
     turnovers:Int = 0,
     threePointMakes:Int = 0,
     freeThrowAttempts:Int = 0,
     fieldGoalPercent:Int = 0,
     freeThrowPercent:Int = 0)

case class Environment(players:List[Player])

trait Scorer {
  def pickWinner(agents:List[Agent]):Agent
}

object MaxPointsScorer extends Scorer {
  def pickWinner(agents: List[Agent]): Agent = {
    val winner:Int = Math.argMax(agents.map(_.players.map(_.points).sum)).getOrElse(throw new Exception("need to get a player"))
    agents(winner)
  }
}

object MaxAllScorer extends Scorer {
  def pickWinner(agents: List[Agent]): Agent = {
    val pointsWinner = Math.argMax(agents.map(_.players.map(_.points).sum))
    val assistsWinner = Math.argMax(agents.map(_.players.map(_.assists).sum))
    val reboundsWinner = Math.argMax(agents.map(_.players.map(_.rebounds).sum))
    val stealsWinner = Math.argMax(agents.map(_.players.map(_.steals).sum))
    val blocksWinner = Math.argMax(agents.map(_.players.map(_.blocks).sum))
    val turnoverWinner = Math.argMin(agents.map(_.players.map(_.turnovers).sum))
    val threePointMakesWinner = Math.argMax(agents.map(_.players.map(_.threePointMakes).sum))
    val freeThrowAttemptsWinner = Math.argMax(agents.map(_.players.map(_.freeThrowAttempts).sum))

    //TODO: Add the percent categories
    val scoreGroup = List(
      pointsWinner,
      assistsWinner,
      reboundsWinner,
      stealsWinner,
      blocksWinner,
      turnoverWinner,
      threePointMakesWinner,
      freeThrowAttemptsWinner
    ).flatten.groupBy(identity)

    val overallWinner = scoreGroup.maxBy(_._2.size)._1

    agents(overallWinner)
  }
}

trait Agent {
  def name = this.getClass.getSimpleName

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
    val selectedPlayerIndex:Int = Math.argMax(envPlayers.map(_.points)).getOrElse(throw new Exception("need to get a player"))
    val selectedPlayer = envPlayers(selectedPlayerIndex)

    //Abstract
    val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
    val newAgent = this.copy(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }
}

object MaxPointsMonteCarloAgent {
  def update[A](score:Map[A, Int], player:A, value:Int):Map[A, Int] =
    score + (player -> score.get(player).map(_ + value).getOrElse(value))

  def tupleOrdering = new Ordering[(_, Int)] {
    override def compare(x: (_, Int), y: (_, Int)): Int = {
      if(x._2 > y._2) 1
      else if (x._2 < y._2) -1
      else 0
    }
  }
}

trait MonteCarloAgent extends Agent {
  import MaxPointsMonteCarloAgent._

  lazy val epsilon = .1

  def iters:Int

  def scorer:Scorer

  def action(environment: Environment, otherAgents:List[Agent]):(Environment, Agent) = {
    val score = randomDraftAndScore(
      environment,
      otherAgents,
      Map(),
      iters,
      players.length + 1 /** Keeping track of how many players to give other agents*/,
      e = epsilon
    )
    val (selectedPlayer, _ )= score.toList.max(tupleOrdering)
    val envPlayers = environment.players
    val selectedPlayerIndex = envPlayers.indexOf(selectedPlayer)
    val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
    val newAgent = apply(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }

  @tailrec
  private def randomDraftAndScore(environment: Environment, otherAgents:List[Agent], score:Map[Player, Int], iters:Int, numberOfPlayers:Int, e:Double): Map[Player, Int] = {
    if (iters > 0) {
      //We draft randomly e fraction of the time, otherwise pick player that contributes to max score
      val (newEnv, newAgent) = {
        if( e > Random.nextDouble() || score.isEmpty) {
            randomAction(environment)
          }
        else {
          val (selectedPlayer, _ ) = score.toList.max(tupleOrdering)
          val envPlayers = environment.players
          val selectedPlayerIndex = envPlayers.indexOf(selectedPlayer)
          val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
          val newAgent = apply(players :+ selectedPlayer)
          val newEnv = Environment(remainingPlayers)
          (newEnv, newAgent)
        }
      }
      //All other agents that need another player draft randomly with updated env

      val agentsThatNeedAnotherPlayer = otherAgents.filter(_.players.length < numberOfPlayers)

      val (_, agents) = randomDraft(newEnv, agentsThatNeedAnotherPlayer)
      //We score the draft
      val winner = scorer.pickWinner(agents :+ newAgent)
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
      randomDraftAndScore(environment, otherAgents, newScore, iters - 1, numberOfPlayers, e)
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

case class MaxPointsMonteCarloAgent(override val players:List[Player] = Nil, override val iters:Int = 100) extends MonteCarloAgent{
  override def scorer: Scorer = MaxPointsScorer
  def apply(players: List[Player]): Agent = MaxPointsMonteCarloAgent(players)
}

case class MaxAllMonteCarloAgent(override val players:List[Player] = Nil, override val iters:Int = 1000) extends MonteCarloAgent{
  override def scorer: Scorer = MaxAllScorer
  def apply(players: List[Player]): Agent = MaxAllMonteCarloAgent(players)

}