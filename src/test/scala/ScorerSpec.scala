import org.scalatest.FunSuite

/**
  * Created by benjaminsmith on 10/29/17.
  */
class ScorerSpec extends FunSuite {

  def mkDummy(inPlayers: List[Player]) = {
    new Agent {
      def action(env: Environment, otherAgents: List[Agent]): (Environment, Agent) = null

      def players: List[Player] = inPlayers

      def apply(players: List[Player]): Agent = null
    }
  }

  test("max points scorer should take agent with max cumulative points of all its players") {
    val player1 = Player(2)
    val player2 = Player(4)
    val player3 = Player(10)
    val player4 = Player(8)

    val agent1 = mkDummy(List(player1))
    val agent2 = mkDummy(List(player2, player4))
    val agent3 = mkDummy(List(player3))

    assert(MaxPointsScorer.pickWinner(List(agent1, agent2, agent3)).contains(agent2))
  }

  test("max all scorer should pick agent with max cumulative points of all its players") {
    val player1 = Player(2)
    val player2 = Player(4)
    val player3 = Player(10)
    val player4 = Player(8)

    val agent1 = mkDummy(List(player1))
    val agent2 = mkDummy(List(player2, player4))
    val agent3 = mkDummy(List(player3))

    assert(MaxAllScorer.pickWinner(List(agent1, agent2, agent3)).contains(agent2))
  }

  test("max all works with single player, many categories, when they tie it should be None"){
    val a2 = mkDummy(List(Player(2376,612,502,138,51,374,236,837,117,1617,907,"James Harden")))
    val a3 = mkDummy(List(Player(1385,264,733,148,140,191,74,408,99,1067,566,"Paul Millsap")))

    assert(MaxAllScorer.pickWinner(List(a3, a2)).isEmpty)
  }
}