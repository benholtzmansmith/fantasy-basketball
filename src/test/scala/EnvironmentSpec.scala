import org.scalatest.FunSuite

/**
  * Created by benjaminsmith on 11/21/17.
  */
class EnvironmentSpec extends FunSuite{
  val p1 = Player(name = "ben")
  val p2 = Player(name = "smith")
  val env = Environment(List(p1, p2))

  test("it should drop a player if it exists in list"){
    val newEnv = env.dropPlayer(p1)
    assert(newEnv.players == List(p2))
  }
  test("it shouldn't drop any players if a player is passed to the func that doesn't exist in the lists"){
    val p3 = Player(name = "holt")
    val newEnv = env.dropPlayer(p3)
    assert(newEnv.players == List(p1, p2))
  }
}
