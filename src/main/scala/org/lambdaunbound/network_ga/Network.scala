package org.lambdaunbound.network_ga

//type EdgeMap[N,E] = Map[N,Map[N.E]]

case class Network[N,E](nodes:Set[N],edges:Map[N,Map[N,E]]){
  def addNode(node:N):Network[N,E] = {
    require(!nodes.contains(node))
    Network(nodes+node,edges+((node,Map())))
  }

  def addNodes(newNodes:List[N]):Network[N,E] = newNodes match {
    case Nil => this
    case n::ns => addNode(n).addNodes(ns)
  }

  def removeNode(node:N):Network[N,E] = {
    require(contains(node))
    val newNodes = nodes-node
    val tmpEdges = edges-node
    val newEdges = tmpEdges.map{case (n,em) => if(em.contains(node)) (n,em-node) else (n,em)}
    Network(newNodes,newEdges)
  }

  def addEdge(node1:N,node2:N,edge:E):Network[N,E] = {
    require(nodes.contains(node1))
    require(nodes.contains(node2))
    val ne = edges(node1)
    require(!ne.contains(node2))
    Network(nodes,edges+((node1,ne+((node2,edge)))))
  }

  def getEdge(node1:N,node2:N):Option[E] = {
    require(nodes.contains(node1))
    require(nodes.contains(node2))
    val ne = edges(node1)
    ne.get(node2)
  }

  def removeEdge(node1:N,node2:N):Network[N,E] = {
    require(nodes.contains(node1))
    require(nodes.contains(node2))
    val ne = edges(node1)
    require(ne.contains(node2))
    Network(nodes,edges+((node1,ne-(node2))))
  }

  def contains(node:N) = nodes.contains(node)

  def connectedTo(node:N):Set[N] = {
    require(contains(node))
    edges(node).toList.map(_._1).toSet
  }

  def connectedFrom(node:N):Set[N] = {
    require(contains(node))
    for{n<- nodes
      if(n!=node)
      if(getEdge(n,node)!=None)
      }yield n
  }

  def getNeighbours(node:N):Set[N] = {
    connectedTo(node)++connectedFrom(node)
  }

  lazy val numEdges:Int = {
    edges.foldLeft(0)(_ + _._2.size)
  }
}

object NetworkMeasures {
  
  def degreeDistribution[N,E](net:Network[N,E]):Map[Int,Double] = {
    require(net.nodes.size!=0)
    val neighbours = net.nodes.groupBy(n=>net.edges(n).size)
    val nodesSize = net.nodes.size.toDouble
    neighbours.map{case (k,v) => (k,v.size/nodesSize)}
  }

  def averageDegree[N,E](net:Network[N,E]):Double = {
    require(net.nodes.size!=0)
    net.nodes.foldLeft(0)((t,n)=>t+net.edges(n).size)/net.nodes.size.toDouble
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
    require(net.nodes.size!=0)
    net.nodes.foldLeft(0.0)(_+nodeClusteringCoefficient(net,_))/net.nodes.size
  }

  def nodePathLength[N,E](net:Network[N,E],node:N):(Int,Int) = {
    require(net.contains(node))
    def bfs(queue:List[(N,Int)],visited:Set[N]):List[(N,Int)] = queue match {
      case Nil => Nil
      case n::ns => {
        //TODO: is this connectedTo or neighbours
        val neighbours = net.connectedTo(n._1)--visited
        val nvis = visited++neighbours
        val pls = neighbours.map((_,n._2+1))
        val nqueue = ns++pls
        if(visited.size==net.nodes.size)
          pls.toList
        else
          pls.toList ++ bfs(nqueue,nvis)
      }
    }
    val nds = bfs(List((node,0)),Set[N]())
    (nds.foldLeft(0)(_+_._2),nds.size)
  }

  def averagePathLength[N,E](net:Network[N,E]):Double = {
    val pls = net.nodes.foldLeft((0,0)){(running,node)=>
      val npl = nodePathLength(net,node)
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
    using(new BufferedReader(reader)){br:BufferedReader =>
      var line = br.readLine()
      val size = line.length
      var net = Network[Int,Int](Set(),Map())
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