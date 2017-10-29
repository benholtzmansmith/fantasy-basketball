import org.scalatest.FunSuite

/**
  * Created by benjaminsmith on 10/29/17.
  */
class ScorerSpec extends FunSuite {

  def mkDummy(inPlayers:List[Player]) = {
    new Agent {
      def action(env: Environment): (Environment, Agent) = null
      def players: List[Player] = inPlayers
    }
  }

  test("max points scorer should take agent with max cumulative points of all its players"){
    val player1 = Player(2)
    val player2 = Player(4)
    val player3 = Player(10)
    val player4 = Player(8)

    val agent1 = mkDummy(List(player1))
    val agent2 = mkDummy(List(player2, player4))
    val agent3 = mkDummy(List(player3))

    assert(MaxPointsScorer.pickWinner(List(agent1, agent2, agent3)) == agent2)
  }
}
