package org.lambdaunbound.network_ga

object Network {
  def create[N,E]():Network[N,E] = Network(Set(),Map(),Map())
}

case class Network[N,E](nodes:Set[N],toEdges:Map[N,Map[N,E]],fromEdges:EdgeMap[N,E]){
  def addNode(node:N):Network[N,E] = {
    require(!nodes.contains(node))
    Network(nodes+node,toEdges+((node,Map())),fromEdges+((node,Map())))
  }

  def addNodes(newNodes:List[N]) = newNodes.foldLeft(this)(_.addNode(_))

  def removeNode(node:N):Network[N,E] = {
    def removeNodeFrom(edges:EdgeMap[N,E]):EdgeMap[N,E] = {
      val tmpEdges = edges-node
      tmpEdges.map{case (n,em) => if(em.contains(node)) (n,em-node) else (n,em)}
    }
    require(contains(node))
    val newNodes = nodes-node
    val newTo = removeNodeFrom(toEdges)
    val newFrom = removeNodeFrom(fromEdges)
    Network(newNodes,newTo,newFrom)
  }

  def addEdge(node1:N,node2:N,edge:E):Network[N,E] = {
    require(nodes.contains(node1))
    require(nodes.contains(node2))
    val ne = toEdges(node1)
    require(!ne.contains(node2))
    val newTo = toEdges+((node1,ne+((node2,edge))))
    val newFrom = fromEdges+((node2,fromEdges(node2)+((node1,edge))))
    Network(nodes,newTo,newFrom)
  }

  def getEdge(node1:N,node2:N):Option[E] = {
    require(nodes.contains(node1))
    require(nodes.contains(node2))
    val ne = toEdges(node1)
    ne.get(node2)
  }

  def removeEdge(node1:N,node2:N):Network[N,E] = {
    require(nodes.contains(node1))
    require(nodes.contains(node2))
    val ne = toEdges(node1)
    require(ne.contains(node2))
    val newTo = toEdges+((node1,ne-(node2)))
    val newFrom = fromEdges+((node2,fromEdges(node2)-(node1)))

    Network(nodes,newTo,newFrom)
  }

  def contains(node:N) = nodes.contains(node)

  def connectedTo(node:N):Set[N] = {
    require(contains(node))
    toEdges(node).toList.map(_._1).toSet
  }

  def connectedFrom(node:N):Set[N] = {
    require(contains(node))
    fromEdges(node).toList.map(_._1).toSet
  }

  def getNeighbours(node:N):Set[N] = {
    connectedTo(node)++connectedFrom(node)
  }

  lazy val numEdges:Int = {
    toEdges.foldLeft(0)(_ + _._2.size)
  }

  def out(out:java.io.PrintWriter){
    val nl = nodesList
    nl.map{n=>
      out.print(n+" ")
      out.println(toEdges(n).map(_._1).mkString(" "))
    }
    out.println("End network")
  }

  override def toString:String = {
    nodesList.map{n=>
      n+" "+ toEdges(n).map(_._1).mkString(" ")
    }.mkString("\n")
  }

  lazy val nodesList = nodes.toList

  lazy val edgeList:List[(N,N)] = toEdges.map{case (n1,m)=> m.map{case (n2,e)=> (n1,n2) }}.flatten.toList

}

object NetworkMeasures {
  import scala.collection.immutable.Queue
  
  def degreeDistribution[N,E](net:Network[N,E]):Map[Int,Double] = {
    require(net.nodes.size!=0)
    val neighbours = net.nodes.groupBy(n=>net.toEdges(n).size)
    val nodesSize = net.nodes.size.toDouble
    neighbours.map{case (k,v) => (k,v.size/nodesSize)}
  }

  def averageDegree[N,E](net:Network[N,E]):Double = {
    logger.debug("Calculating Average Degree")
    require(net.nodes.size!=0)
    net.numEdges/net.nodes.size.toDouble
  }

  def nodeClusteringCoefficient[N,E](net:Network[N,E],node:N):Double = {
    val neighbours = net.getNeighbours(node).toList
    val expected = neighbours.size*(neighbours.size-1).toDouble
    val connections:Seq[Int] = for{n1 <- neighbours
      n2 <- neighbours
      if(n1!=n2)
      if(net.getEdge(n1,n2)!=None || net.getEdge(n2,n1)!=None)
      }yield 1
    if(expected==0)0
    else connections.foldLeft(0)(_+_)/expected
  }

  def averageClusteringCoefficient[N,E](net:Network[N,E]):Double = {
    logger.debug("Calculating Clustering Coefficient")
    require(net.nodes.size!=0)
    net.nodes.foldLeft(0.0)(_+nodeClusteringCoefficient(net,_))/net.nodes.size
  }

  def nodePathLength[N,E](net:Network[N,E],node:N):(Int,Int) = {
    require(net.contains(node))
    def bfs(queue:Seq[(N,Int)],visited:Set[N]):Iterable[(N,Int)] = queue.headOption match {
      case None => Nil
      case Some(n) => {
        val neighbours = net.getNeighbours(n._1)--visited
        val nvis = visited++neighbours
        val pls = neighbours.map((_,n._2+1))
        val nqueue = queue.tail++pls
        if(visited.size==net.nodes.size)
          pls.toList
        else
          pls.toList ++ bfs(nqueue,nvis)
      }
    }
    val nds = bfs(List((node,0)),Set(node))
    (nds.foldLeft(0)(_+_._2),nds.size)
  }

  def averagePathLength[N,E](net:Network[N,E]):Double = {
    logger.debug("Calculating Path Length")
    val npls = net.nodesList.par.map(nodePathLength(net,_))
    val pls = npls.foldLeft((0,0)){(running,npl)=>
      (running._1+npl._1,running._2+npl._2)
    }
    if(pls._2==0)
      0
    else
      pls._1/pls._2.toDouble
  }
}

object NetworkIO {
  import java.io._

  def loadAdjacencyMatrix(reader:Reader):Network[Int,Int] = {
    logger.debug("Loading in Adjacency Matrix")
    using(new BufferedReader(reader)){br:BufferedReader =>
      var line = br.readLine()
      val size = line.length
      var net = Network.create[Int,Int]()
      net = net.addNodes(0 until size toList)
      var row:Int = 0
      do{
        var col = 0
        while(col<line.size){
          if(line.charAt(col)!='0')
            net = net.addEdge(row,col,1)
          col+=1
        }
        assert(col==size)
        row+=1
        line=br.readLine()
      }while(line!=null)
      net
    }
  }

  def fileReader(fname:String):Reader = {
    new FileReader(fname)
  }

  def URLReader(fname:String,clazz:AnyRef=this):Reader = {
    new InputStreamReader(clazz.getClass.getResource(fname).openStream)
  }


//  def loadAdjacencyMatrixWithSpaces(fname:String):Network[Int,Int] = {
//    using(new BufferedReader(new FileReader(fname))){br:BufferedReader =>
//      var line = br.readLine()
//      var parts = line.split("\\s*")
//      var net = Network[Int,Int](Set(),Map())
//      net = net.addNodes(0 until parts.size toList)
//      var row:Int = 0
//      do{
//        var parts = line.split("\\s*").map(_.toInt)
//        var col = 0
//        while(col<parts.size){
//          if(parts(col)!=0)
//            net = net.addEdge(row,col,parts(col))
//          col+=1
//        }
//        row+=1
//        line=br.readLine()
//      }while(line!=null)
//      net
//    }
//  }
}
