import org.scalatest.FunSuite

import TestData._
import FantasyBasketball._

/**
  * Created by benjaminsmith on 10/29/17.
  */
class DraftSpec extends FunSuite{
 test("draft should end with each agent having a single player"){
   val startingEnv = Environment(TestData.allPlayers)

   val startingAgents = TestData.allAgents

   val (_, newAgents) = draft(startingEnv, startingAgents)

   assert(newAgents.forall(_.players.length == 1))
 }
}
