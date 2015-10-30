import org.scalatest._
import Dijkstra._

class DijkstraTest extends FunSuite with Matchers {

  val G = Map(0 -> Map(1 -> 10, 2 -> 30, 3 -> 15),
      1 -> Map(4 -> 20),
      2 -> Map(5 -> 5),
      3 -> Map(5 -> 20, 2 -> 5),
      4 -> Map(5 -> 20),
      5 -> Map(3 -> 20))

  test("dijkstra #1 : Result Count") {
    assert(applyDijkstra(G, List(), None).size === 6)
  }

  // 갈수 있는 경로의 맵을 인자로 받고,
  // 최단 거리를 가진 곳의 포인트를 반환한다
  test("dijkstra #2 : Extract min") {
    assert(extractMin(List(5, 3, 30, Int.MaxValue), List(3)) === 1)
  }

  test("dijkstra #3 : Add dist sequence") {
    addMinDist((0, Map(1 -> 10, 2 -> 30, 3 -> 15)),List(0, Int.MaxValue, 30, 40, 10, 30), List(0)) should equal(List(0, 10, 30, 15, 10, 30))
  }

  test("dijkstra #4 : finalResult") {
    applyDijkstra(G, List()) should equal(List(0,10,20,15,30,25))
  }
}
