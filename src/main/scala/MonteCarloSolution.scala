import PlayerFormat.PlayerSerializer
import play.api.libs.json._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

/**
  * Created by benjaminsmith on 10/28/17.
  */

object PlayerFormat {
  case class PlayerSerializer(
                               Name:String,
                               p:Int,
                               a:Int,
                               r:Int,
                               `3`:Int,
                               s:Int,
                               b:Int,
                               to:Int,
                               fga:Int,
                               fta:Int,
                               ftm:Int,
                               fgm:Int)
  object PlayerSerializer {
    implicit val playerSerializer:Format[PlayerSerializer] = Json.format[PlayerSerializer]
  }

  def test = {
    val source: String = Source.fromFile("/Users/benjaminsmith/Programming/fantasy-basketball/src/main/resources/players_2017.json").getLines.mkString
    val json: JsValue = Json.parse(source)
    Json.fromJson[List[PlayerSerializer]](json)
  }
}

object TestData {

  val player1 = Player(points = 1, assists = 8, rebounds = 10)
  val player2 = Player(points = 2, assists = 3, rebounds = 8)
  val player3 = Player(points = 4, assists = 4, rebounds = 6)
  val player4 = Player(points = 10, assists = 2, rebounds = 1)

  val allPlayers = List(player1, player2, player3, player4)

  val agent1 = RandomAgent(Nil)
  val agent2 = MaxPointsAgent(Nil)
  val agent3 = MaxAllMonteCarloAgent(Nil, iters = 200)

  val allAgents = List(agent3, agent2, agent1)


  def playersGenerator(n:Int):List[Player] = {
    def pair = {
      val v1 = genValue
      val v2 = genValue
      if (v1 > v2) (v1, v2)
      else (v2, v1)
    }

    def genValue = math.abs(Random.nextDouble() * 100).toInt
    val (freeThrowAttempts, freeThrowMakes) = pair
    val (fieldGoalAttempts, fieldGoalMakes) = pair
    List.fill(n){
      Player(
        points = 2,
        assists = genValue,
        rebounds = genValue,
        steals = genValue,
        blocks = genValue,
        threePointMakes = genValue,
        turnovers = genValue,
        freeThrowAttempts = freeThrowAttempts,
        freeThrowMakes = freeThrowMakes,
        fieldGoalAttempts = fieldGoalAttempts,
        fieldGoalMakes = fieldGoalMakes
      )
    }
  }

  def realPlayers:List[Player] = {
    val source: String = Source.fromFile("/Users/benjaminsmith/Programming/fantasy-basketball/src/main/resources/players_2017.json").getLines.mkString
    val json: JsValue = Json.parse(source)
    Json.fromJson[List[PlayerSerializer]](json).map{
      _.map {
        p =>
          Player(
            points = p.p,
            assists = p.a,
            rebounds = p.r,
            steals = p.s,
            blocks = p.b,
            turnovers = p.to,
            threePointMakes = p.`3`,
            freeThrowAttempts = p.fta,
            freeThrowMakes = p.ftm,
            fieldGoalAttempts = p.fga,
            fieldGoalMakes = p.fgm,
            name = p.Name
          )
      }
    } match {
      case JsError( e ) => println(e); Nil
      case JsSuccess( a, _) => a
    }
  }
}


object FantasyBasketball {
  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()

    val startingPlayers = TestData.realPlayers

    val numberOfRoundsInDraft = 10

    val startingEnv = Environment(startingPlayers)

    val startingAgents:List[Agent] = scala.util.Random.shuffle(TestData.allAgents)

    val scoreMap = runGameN(startingEnv,startingAgents, 40, Map(), numberOfRoundsInDraft)

    val winner = scoreMap.toList.max(Utils.tupleOrdering)

    val endTime = System.nanoTime()

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
         | RunTime = ${(endTime - startTime) / (1000 * 1000 * 1000) } seconds
         |
       """.stripMargin)

  }
  @tailrec
  def runGameN(startingEnv:Environment, startingAgents:List[Agent], runCount:Int, scoreTracker:Map[String, Int], numberOfRoundsDraft:Int):Map[String, Int] = {
    if (runCount > 0){
      val (_, newAgents) = draft(startingEnv, startingAgents, Nil, numberOfRoundsDraft)

      val winner = MaxAllScorer.pickWinner(newAgents)

      val newMap = Utils.update(scoreTracker, winner.name, 1)

      runGameN(startingEnv, startingAgents, runCount - 1, newMap, numberOfRoundsDraft)
    }
    else scoreTracker
  }

  @tailrec
  def draft(environment: Environment,
            inAgents:List[Agent],
            outAgents:List[Agent] = Nil,
            remainingRounds:Int
           ):(Environment, List[Agent]) = {
    assert(environment.players.length >= inAgents.length * remainingRounds, "fewer players then agents * remainingRounds")
    inAgents match {
      case _ if remainingRounds <= 0 => (environment, inAgents)
      case Nil => draft(environment, outAgents.reverse, Nil, remainingRounds - 1)
      case h :: t => {
        val allOtherAgents = t ++ outAgents
        val (newEnv, newAgent) = h.action(environment, allOtherAgents)
        draft(newEnv, t, outAgents :+ newAgent, remainingRounds)
      }
    }
  }
}

object Math {
  def argMax(l:Seq[Double]):Option[Int] = {
    val (_, i) = l.zipWithIndex.foldLeft((0.0,None:Option[Int])){ case (prev@(prevV, _), (vAndi)) =>
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

  def argMin(l:Seq[Double]):Option[Int] = {
    l match {
      case Nil => None
      case h :: Nil => Some(0)
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
     freeThrowMakes:Int = 0,
     fieldGoalAttempts:Int = 0,
     fieldGoalMakes:Int = 0,
     name:String = "") {
  assert(freeThrowAttempts >= freeThrowMakes, s"${name}: need to attempt more free throws than makes")
  assert(fieldGoalAttempts >= fieldGoalMakes, s"${name}: need to attempt more field goals than makes")
}

case class Environment(players:List[Player])

trait Scorer {
  def pickWinner(agents:List[Agent]):Agent
}

object MaxPointsScorer extends Scorer {
  def pickWinner(agents: List[Agent]): Agent = {
    val winner = Math.argMax(agents.map(_.players.map(_.points).sum.toDouble)).getOrElse(throw new Exception("need to get a player"))
    agents(winner)
  }
}

object MaxAllScorer extends Scorer {
  def pickWinner(agents: List[Agent]): Agent = {
    val pointsWinner = Math.argMax(agents.map(_.players.map(_.points).sum.toDouble))
    val assistsWinner = Math.argMax(agents.map(_.players.map(_.assists).sum.toDouble))
    val reboundsWinner = Math.argMax(agents.map(_.players.map(_.rebounds).sum.toDouble))
    val stealsWinner = Math.argMax(agents.map(_.players.map(_.steals).sum.toDouble))
    val blocksWinner = Math.argMax(agents.map(_.players.map(_.blocks).sum.toDouble))
    val turnoverWinner = Math.argMin(agents.map(_.players.map(_.turnovers).sum.toDouble))
    val threePointMakesWinner = Math.argMax(agents.map(_.players.map(_.threePointMakes).sum.toDouble))
    val freeThrowAttemptsWinner = Math.argMax(agents.map(_.players.map(_.freeThrowAttempts).sum.toDouble))
    val fieldGoalPercentageWinner = Math.argMax(agents.map{ a =>
      val attempts = a.players.map(_.fieldGoalAttempts).sum
      val makes = a.players.map(_.fieldGoalAttempts).sum
      makes.toDouble / (makes + attempts)
    })
    val fieldThrowPercentageWinner = Math.argMax(agents.map{ a =>
      val attempts = a.players.map(_.freeThrowAttempts).sum
      val makes = a.players.map(_.freeThrowMakes).sum
      makes.toDouble / (makes + attempts)
    })

    //TODO: Add the percent categories
    val scoreGroup = List(
      pointsWinner,
      assistsWinner,
      reboundsWinner,
      stealsWinner,
      blocksWinner,
      turnoverWinner,
      threePointMakesWinner,
      freeThrowAttemptsWinner,
      fieldGoalPercentageWinner,
      fieldThrowPercentageWinner
    ).flatten.groupBy(identity)

    val overallWinner = scoreGroup.maxBy(_._2.size)._1

    agents(overallWinner.toInt)
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
    val selectedPlayerIndex:Int = Math.argMax(envPlayers.map(_.points.toDouble)).getOrElse(throw new Exception("need to get a player"))
    val selectedPlayer = envPlayers(selectedPlayerIndex)

    //Abstract
    val remainingPlayers = envPlayers.patch(selectedPlayerIndex, Nil, 1)
    val newAgent = this.copy(players :+ selectedPlayer)
    val newEnv = Environment(remainingPlayers)
    (newEnv, newAgent)
  }
}

object Utils {
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
  import Utils._

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
  final def randomDraft(environment: Environment, inAgents:List[Agent], outAgents:List[Agent] = Nil):(Environment, List[Agent]) = {
    inAgents match {
      case Nil => (environment, outAgents)
      case h :: t => {
        val (newEnv, newAgent) = h.randomAction(environment)
        randomDraft(newEnv, t, outAgents :+ newAgent)
      }
    }
  }
}

case class MaxAllMonteCarloAgent(override val players:List[Player] = Nil, override val iters:Int = 100) extends MonteCarloAgent{
  override def scorer: Scorer = MaxAllScorer

  def apply(players: List[Player]): Agent = MaxAllMonteCarloAgent(players)

}
case class MaxPointsCarloAgent(override val players:List[Player] = Nil, override val iters:Int = 100) extends MonteCarloAgent{
  override def scorer: Scorer = MaxPointsScorer

  def apply(players: List[Player]): Agent = MaxAllMonteCarloAgent(players)

}