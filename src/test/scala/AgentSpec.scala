import org.scalatest.{FlatSpec, FunSuite}

/**
  * Created by benjaminsmith on 10/29/17.
  */
class AgentSpec extends FlatSpec {
  behavior of "Max Points Agent"

  it should "pick the player with the max points and update the env" in {
    val maxPointsAgent = MaxPointsAgent()

    val p1 = Player(10)
    val p2 = Player(12)

    val environment = Environment(List(p1,p2))

    val (newEnv, newAgent) = maxPointsAgent.action(environment, Nil)

    assert(newAgent.players == List(p2))
    assert(newEnv.players == List(p1))
  }

  behavior of "Monte carlo agent"

  it should "update func should increment score for already existing player" in {
    val testPlayer = Player(1)
    val testMap = Map(testPlayer -> 1)
    val newMap = Utils.update(testMap, testPlayer, 1)
    assert(newMap == Map(testPlayer -> 2))
  }

  it should "make a new entry for a player" in {
    val testPlayer = Player(1)
    val testMap:Map[Player, Int] = Map()
    val newMap = Utils.update(testMap, testPlayer, 1)
    assert(newMap == Map(testPlayer -> 1))
  }

  it should "should be able to subtract from the score" in {
    val testPlayer = Player(1)
    val testMap:Map[Player, Int] = Map()
    val newMap = Utils.update(testMap, testPlayer, -1)
    assert(newMap == Map(testPlayer -> -1))
  }

  it should "should be able to subtract from the score if an entry already exists" in {
    val testPlayer = Player(1)
    val testMap:Map[Player, Int] = Map(testPlayer -> 1)
    val newMap = Utils.update(testMap, testPlayer, -1)
    assert(newMap == Map(testPlayer -> 0))
  }

  it should "draft random" in {
    val p1 = Player(2)
    val p2 = Player(10)

    val environment = Environment(players = List(p1, p2))

    val monteAgent = MaxPointsAgent()

    val competitor = RandomAgent()

    val (newEnv, newAgent) = monteAgent.action(environment, List(competitor))

    //This should almost always happen
    assert(newAgent.players == List(p2))

  }


  it should "almost always find the best player" in {
    val p1 = Player(2, 12, 50, 60, fieldGoalAttempts = 10, fieldGoalMisses = 4, freeThrowAttempts = 10, freeThrowMisses = 4)
    val p2 = Player(100, 8, 40, 20, fieldGoalAttempts = 10, fieldGoalMisses = 6, freeThrowAttempts = 10, freeThrowMisses = 6)

    val environment = Environment(players = List(p1, p2))

    val monteAgent = MaxAllMonteCarloAgent()

    val competitor = MaxPointsAgent()

    val (newEnv, newAgent) = monteAgent.action(environment, List(competitor))

    assert(newAgent.players == List(p1))
  }

  val steph = Player(2375,527,430,169,16,262,402,400,37,1596,791,"Stephen Curry")
  val durant = Player(2029,360,590,68,85,250,186,498,51,1381,683,"Kevin Durant")
  val harden = Player(2376,612,502,138,51,374,236,837,117,1617,907,"James Harden")
  val westbrook = Player(1878,836,628,163,20,341,101,573,108,1444,788,"Russell Westbrook")
  val leonard = Player(1525,186,493,128,71,105,129,334,42,1091,539,"Kawhi Leonard")
  val lebron = Player(1920,512,565,104,49,249,87,491,132,1416,679,"LeBron James")
  val paul = Player(1446,738,310,151,13,194,122,328,34,1115,600,"Chris Paul")
  val thompson = Player(1771,166,306,60,50,138,276,221,28,1386,735,"Klay Thompson")
  val towns = Player(1503,161,854,58,138,183,30,275,52,1152,527,"Karl-Anthony Towns")
  val draymond = Player(1131,597,765,118,111,258,100,329,100,819,418,"Draymond Green")
  val all = List(steph, durant, westbrook, leonard, lebron, paul, thompson, towns , draymond)

  it should "find harden" in {
    val environment = Environment(players = all)

    val monteAgent = MaxAllMonteCarloAgent(iters = 1000)

    val competitor = MaxPointsAgent(List(harden))

    val (newEnv, newAgent) = monteAgent.action(environment, List(competitor))

    assert(List(steph, durant, paul, westbrook).contains(newAgent.players.head))
  }
}
