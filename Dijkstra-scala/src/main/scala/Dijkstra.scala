object Dijkstra {

  def main(args: Array[String]) {
    val G = Map(0 -> Map(1 -> 10, 2 -> 30, 3 -> 15),
      1 -> Map(4 -> 20),
      2 -> Map(5 -> 5),
      3 -> Map(5 -> 20, 2 -> 5),
      4 -> Map(5 -> 20),
      5 -> Map(3 -> 20))

    println(applyDijkstra(G, List(), None))
  }

  def applyDijkstra(graph: Map[Int, Map[Int, Int]], filter: List[Int], dist: Option[List[Int]]=None): List[Int] = {
    val lDist = dist match {
      case Some(mp)=> mp
      case None=> List.tabulate(graph.keys.size){(e)=>if(e==0) 0 else Int.MaxValue}
    }
    val min = extractMin(lDist, filter)
    val rDist = addMinDist((min, graph(min)),lDist, filter)
    val nfilter = filter ++ Seq(min)
    if(nfilter.size == rDist.size)
      rDist
    else   
      applyDijkstra(graph, nfilter, Some(rDist))
  }

  def extractMin(dist: List[Int], ft: List[Int]): Int =
    dist.zipWithIndex.map(_.swap).sortBy(_._2).filterNot((x)=>ft.contains(x._1)).head._1

  def addMinDist(m: Tuple2[Int, Map[Int, Int]], dist: List[Int], ft: List[Int]): List[Int] = {
    val (c, o) = dist.zipWithIndex.map(_.swap).partition((p)=>ft.contains(p._1))
      (c ::: o.map((p2) => (p2._1,
        if(m._2 contains p2._1) dist(p2._1).min(m._2(p2._1) + dist(m._1))
        else p2._2)
      )).sortBy(_._1).map(_._2).toList
  }
}
