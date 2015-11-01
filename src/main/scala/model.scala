

/**
 * @author sjalipar
 */

class model {
  type ConnectedGraph[A, W] = Map[(A, A), W]
}

abstract class ConnectedGraph[A, W] private (val edges: Map[(A, A), W]) {
  lazy val nodes: List[A] =
    edges.keys.flatMap { case (s, d) => List(s, d) }.toList

  def weight(source: A, dest: A): W = edges((source, dest))

  def mapEdge(source: A, dest: A)(f: W => W): ConnectedGraph[A, W] = {
    val v = f(edges((source, dest)))
    new ConnectedGraph(edges.updated((source, dest), v)) {}
  }

  def map[B](f: A => B): ConnectedGraph[B, W] = {
    val mappedNodes = nodes.map { case a => a -> f(a) }.toMap
    val newEdges = edges.map {
      case ((source, dest), w) => ((mappedNodes(source), mappedNodes(dest)), w)
    }

    new ConnectedGraph(newEdges) {}
  }

  def transform[V](f: W => V): ConnectedGraph[A, V] = {
    val newEdges = edges.mapValues(f).view.force
    new ConnectedGraph(newEdges) {}
  }

  import scala.util.Random
  def randomNode(): A = nodes(Random.nextInt(nodes.size))
}

object ConnectedGraph {
  def empty[A, W]: ConnectedGraph[A, W] = new ConnectedGraph(Map.empty[(A, A), W]) {}

  def apply[A, W](es: Seq[A])(f: (A, A) => W): ConnectedGraph[A, W] = {
    es match {
      case Seq() | Seq(_) => empty
      case Seq(a, b, _*) => {
        val initial = Map((a, b) -> f(a, b))
        val edges = es.foldRight(initial) {
          case (node, allEdges) =>
            val newEdges = allEdges.flatMap {
              case ((otherNode, _), _) =>
                Map(((node, otherNode), f(node, otherNode)),
                  ((otherNode, node), f(otherNode, node)))
            }
            allEdges ++ newEdges
        }
        new ConnectedGraph(edges) {}
      }
    }

  }

}
