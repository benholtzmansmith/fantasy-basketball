import scala.annotation.tailrec
import scala.util.Random

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
    val selectedPlayerIndex:Int = Math.argMax(envPlayers.map(_.points.toDouble)).getOrElse(throw new Exception("need to get a player"))
    val selectedPlayer = envPlayers(selectedPlayerIndex)

    //Abstract
    val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
    val newAgent = this.copy(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }
}

object MonteCarloAgentUtils {
  import Utils._

  // TODO:write test for this
  def scoreAndUpdateMap(
                         scorer:Scorer,
                         allCompetitors:List[Agent],
                         newThisAgent:Agent,
                         players:List[Player],
                         score:Map[Player, (Double, Int)]):Map[Player, (Double, Int)] = {
    val winner = scorer.pickWinner(allCompetitors)
    //We figure out what player we drafted
    val newPlayer = newThisAgent
      .players
      .find( p => !players.contains(p))
      .getOrElse( throw new Exception("There was no new player"))

    winner match {
      //If we win, we add a new point to the player who we drafted and we won with
      case Some( w ) if w == newThisAgent => updateCumulativeAverageReward(score, newPlayer, 1)
      //If we lose, we subtract a point (don't necessarily need to do this, we could just add 0)
      case Some( w ) if w != newThisAgent => updateCumulativeAverageReward(score, newPlayer, -1)
      //If nobody wins add a zero entry
      case _ =>  updateCumulativeAverageReward(score, newPlayer, 0)
    }
  }
}

trait MonteCarloAgent extends Agent {
  import Utils._

  def epsilon = .5

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

    val estimatedRewards = score.toList.map{ case (p, (r, c)) => (p,r)}
    val (selectedPlayer, _ )= estimatedRewards.max(tupleDoubleOrdering)
    //    println("\n")
    //    println("*" * 50 )
    //    println(estimatedRewards.mkString("\n"))
    //    println("*" * 50 )
    //    println(s"selected player:$selectedPlayer")
    //    println("*" * 50 )
    //    println("\n")
    val envPlayers = environment.players
    val selectedPlayerIndex = envPlayers.indexOf(selectedPlayer)
    val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
    val newAgent = apply(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }

  def epsilonGreedyValueFunc(e:Double, score:Map[Player, (Double, Int)], environment: Environment)= {
    {
      if( e > Random.nextDouble() || score.isEmpty) {
        randomAction(environment)
      }
      else {
        val (selectedPlayer, _ ) = score.toList.map{ case (p, (r, c)) => (p,r)}.max(tupleDoubleOrdering)
        val envPlayers = environment.players
        val selectedPlayerIndex = envPlayers.indexOf(selectedPlayer)
        val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
        val newAgent = apply(players :+ selectedPlayer)
        val newEnv = Environment(remainingPlayers)
        (newEnv, newAgent)
      }
    }
  }

  case class RewardEstimate(estimatedReward:Double, trials:Int)

  case class RewardResult(player:Player, reward:Int)

  trait TreeSearchMonte extends Agent {

    def action(env: Environment, otherAgents: List[Agent]): (Environment, Agent) = ???

    def apply(players:List[Player]):TreeSearchMonte

    def scorer:Scorer

    def players:List[Player]

    def epsilon:Double

    def pickMax(score:Map[Player, RewardEstimate]):Player

    def pickRandom(score:Map[Player, RewardEstimate]):Player

    def updateRewardEstimate(score:Map[Player, RewardEstimate], rewardResult: RewardResult):Map[Player, RewardEstimate]

    def simulateRound(
                       environment: Environment,
                       currentRewardEstimates:Map[Player, RewardEstimate],
                       otherAgents:List[Agent],
                       iters:Int
                     ):Map[Player, RewardEstimate] = {
      val random = Random.nextDouble()
      val pickedPlayer = if( random > epsilon ){
        pickMax(currentRewardEstimates)
      }
      else pickRandom(currentRewardEstimates)

      val newPlayers = players :+ pickedPlayer

      val newThisAgent = apply(newPlayers)

      val allAgents = newThisAgent +: otherAgents

      val winningAgent = scorer.pickWinner(allAgents)

      val updatedRewardEstimate = winningAgent match {
        case Some( a ) if a.name == this.name =>
          updateRewardEstimate(currentRewardEstimates, RewardResult(pickedPlayer, 1))
        case Some( a ) if a.name != this.name =>
          updateRewardEstimate(currentRewardEstimates, RewardResult(pickedPlayer, -1))
        case Some( a ) if a.name != this.name =>
          updateRewardEstimate(currentRewardEstimates, RewardResult(pickedPlayer, 0))
      }

      updatedRewardEstimate
    }
  }

  @tailrec
  private def randomDraftAndScore(environment: Environment, otherAgents:List[Agent], score:Map[Player, (Double, Int)], iters:Int, numberOfPlayers:Int, e:Double): Map[Player, (Double, Int)] = {
    if (iters > 0) {
      //We draft randomly e fraction of the time, otherwise pick player that contributes to max score
      val (newEnv, newThisAgent) = epsilonGreedyValueFunc(e, score, environment)

      val (agentsThatNeedAnotherPlayer, agentsThatDont) = otherAgents.partition(_.players.length < numberOfPlayers)

      val (_, newAgents) = randomDraft(newEnv, agentsThatNeedAnotherPlayer)
      //We score the draft

      val allCompetitors = newAgents ++ agentsThatDont :+ newThisAgent

      val newScore = MonteCarloAgentUtils.scoreAndUpdateMap(scorer, allCompetitors, newThisAgent, players, score)

      // We run the draft again with the updated score
      randomDraftAndScore(environment, otherAgents, newScore, iters - 1, numberOfPlayers, e)
    }
    //At the end of the iterations we return the score
    else score
  }

  @tailrec
  final def randomDraft(environment: Environment, inAgents:List[Agent], outAgents:List[Agent] = Nil):(Environment, List[Agent]) = {
    inAgents match {
      case Nil => (environment, outAgents)
      case h :: t => {
        val (newEnv, newAgent) = h.action(environment, outAgents ++ inAgents)
        randomDraft(newEnv, t, outAgents :+ newAgent)
      }
    }
  }
}

case class MaxAllMonteCarloAgent(override val players:List[Player] = Nil, override val iters:Int = 100, override val epsilon:Double = .2) extends MonteCarloAgent{
  override def scorer: Scorer = MaxAllScorer

  def apply(players: List[Player]): Agent = MaxAllMonteCarloAgent(players, iters, epsilon)

}
case class MaxPointsCarloAgent(override val players:List[Player] = Nil, override val iters:Int = 100, override val epsilon:Double = .2) extends MonteCarloAgent {
  override def scorer: Scorer = MaxPointsScorer

  def apply(players: List[Player]): Agent = MaxAllMonteCarloAgent(players, iters, epsilon = epsilon)

}