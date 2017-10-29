import org.scalatest.FunSuite

/**
  * Created by benjaminsmith on 10/29/17.
  */
class AgentSpec extends FunSuite{
  test("the max points agent should pick the player with the max points and update the env"){
    val maxPointsAgent = MaxPointsAgent()

    val p1 = Player(10)
    val p2 = Player(12)

    val environment = Environment(List(p1,p2))

    val (newEnv, newAgent) = maxPointsAgent.action(environment)

    assert(newAgent.players == List(p2))
    assert(newEnv.players == List(p1))
  }
}
