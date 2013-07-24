package org.lambdaunbound.network_ga

import org.scalatest.FunSpec

class ExampleSpec extends FunSpec {

  describe("A Network") {

    val empty = Network(Set[Int](),Map[Int,Map[Int,Int]]())

    it("should contain nodes") {
      var net = empty.addNode(0)
      net = net.addNode(1)
      assert(net.contains(0))
      assert(net.contains(1))
    }

    it("should have multiple nodes added at once"){
      var net = empty.addNodes((0 until 10).toList)
      assert(net.nodes.size===10)
    }

    it("should throw IllegalArgumentException if a node is added twice") {
      var net = empty.addNode(1)
      intercept[IllegalArgumentException] {
        net.addNode(1)
      }
    }

    it("should have edges between nodes"){
      var net = empty.addNodes(List(0,1,2))
      net = net.addEdge(0,1,9)
      net = net.addEdge(0,2,10)
      assert(net.getEdge(0,1)===Some(9))
      assert(net.getEdge(0,2)===Some(10))
      assert(net.getEdge(2,0)===None)
    }

    it("should remove edges between nodes"){
      var net = empty.addNode(0).addNode(1)
      net = net.addEdge(0,1,9)
      assert(net.getEdge(0,1)===Some(9))
      net = net.removeEdge(0,1)
      assert(net.getEdge(0,1)===None)
    }

    it("should not have multiple edges between nodes"){
      var net = empty.addNode(0).addNode(1)
      net = net.addEdge(0,1,9)
      assert(net.getEdge(0,1)===Some(9))
      intercept[IllegalArgumentException]{
        net = net.addEdge(0,1,10)
      }
    }

    it("should have nodes removed cleanly"){
      var net = empty.addNode(0).addNode(1).addNode(2)
      net = net.addEdge(0,1,9).addEdge(0,2,10).addEdge(2,0,11)
      net = net.removeNode(2)
      assert(!net.contains(2))
      assert(net.getEdge(0,1)===Some(9))
      assert(net.edges.size===2)
      assert(net.edges(0).size===1)
    }

    it("should only remove existing nodes"){
      var net = empty.addNode(0)
      intercept[IllegalArgumentException]{
        net.removeNode(1)
      }
    }

    it("should be able to get all neighbours of a node"){
      var net = empty.addNodes(List(0,1,2,3))
      net = net.addEdge(0,1,1)
      net = net.addEdge(3,0,1)
      assert(net.getNeighbours(0)===Set(1,3))
    }

    it("should be able to count the number of edges"){
      var net = empty.addNodes((0 until 10).toList)
      net = net.addEdge(0,1,1)
      net = net.addEdge(0,2,1)
      net = net.addEdge(0,3,1)
      net = net.addEdge(0,4,1)
      net = net.addEdge(1,5,1)
      net = net.addEdge(1,2,1)
      net = net.addEdge(1,3,1)
      net = net.addEdge(1,4,1)
      net = net.addEdge(2,3,1)
      net = net.addEdge(2,4,1)
      net = net.addEdge(5,3,1)
      net = net.addEdge(5,4,1)
      assert(net.numEdges === 12)
    }

  }

  describe("A Gene"){
    import java.util.Random
    val empty = Network(Set[Int](),Map[Int,Map[Int,Int]]())
    it("should be able to be mutated to a new gene"){
      val rnd = new Random(0)
      val net = empty.addNodes(0 until 100 toList)
      val gene = Gene(net, 0.1)
      val mutated = gene.mutate(rnd)
      assert(gene!=mutated)
      assert(gene.net.numEdges!=mutated.net.numEdges)
    }
  }

  describe("An Objective"){
    it("should apply to a gene"){
      val gene = 7
      val f = {a:Int => (a-6).abs.toDouble}
      val ob = Objectives(List(f))
      val sg = ob(gene)
      assert(sg===ScoredGene(7,List(1),0))
    }

    it("should be composed with other objectives functions"){
      val gene = 7
      val f = {a:Int => (a-6).abs.toDouble}
      val f2 = {a:Int => a*3.toDouble}
      var ob = Objectives(List(f))
      ob = ob && f2
      val sg = ob(gene)
      assert(sg===ScoredGene(7,List(1.0,21.0),0))
    }
  }
  describe("A ScoredGene"){
    it("should dominate a gene with worse objective performance"){
      val gene = 7
      val gene2 = 8
      val f = {a:Int => (a-6).abs.toDouble}
      val f2 = {a:Int => (a-5).abs.toDouble}
      var ob = Objectives(List(f))
      ob = ob && f2
      val sg = ob(gene)
      val sg2 = ob(gene2)
      assert(sg.dominate(sg2)===true)
    }
    it("should not dominate a gene with a better performance on one objective"){
      val gene = 7
      val gene2 = 8
      val f = {a:Int => (a-6).abs.toDouble}
      val f2 = {a:Int => (a-9).abs.toDouble}
      var ob = Objectives(List(f))
      ob = ob && f2
      val sg = ob(gene)
      val sg2 = ob(gene2)
      assert(sg.dominate(sg2)===false)
    }
  }

  describe("A DegreeDistribution"){
    val empty = Network(Set[Int](),Map[Int,Map[Int,Int]]())

    it("should work for networks with no edges"){
      val dd = NetworkMeasures.degreeDistribution(empty.addNode(0))
      assert(dd === Map(0->1.0))
    }
    it("shouldn't work for networks with no nodes"){
      intercept[IllegalArgumentException]{
        NetworkMeasures.degreeDistribution(empty)
      }
    }
    it("should be correct"){
      var net = empty.addNodes((0 until 10).toList)
      net = net.addEdge(0,1,1)
      net = net.addEdge(0,2,1)
      net = net.addEdge(0,3,1)
      net = net.addEdge(0,4,1)
      net = net.addEdge(1,5,1)
      net = net.addEdge(1,2,1)
      net = net.addEdge(1,3,1)
      net = net.addEdge(1,4,1)
      net = net.addEdge(2,3,1)
      net = net.addEdge(2,4,1)
      net = net.addEdge(5,3,1)
      net = net.addEdge(5,4,1)
      val dd = NetworkMeasures.degreeDistribution(net)
      val expected = Map(0->6/10.0,2->2.0/10.0,4->2.0/10.0) 
      assert(dd === expected)
    }
  }

  describe("An average degree"){
    val empty = Network(Set[Int](),Map[Int,Map[Int,Int]]())
    it("should be correct"){
      var net = empty.addNodes((0 until 10).toList)
      net = net.addEdge(0,1,1)
      net = net.addEdge(0,2,1)
      net = net.addEdge(0,3,1)
      net = net.addEdge(0,4,1)
      net = net.addEdge(1,5,1)
      net = net.addEdge(1,2,1)
      net = net.addEdge(1,3,1)
      net = net.addEdge(1,4,1)
      net = net.addEdge(2,3,1)
      net = net.addEdge(2,4,1)
      net = net.addEdge(5,3,1)
      net = net.addEdge(5,4,1)
      val deg = NetworkMeasures.averageDegree(net)
      assert(deg === 12/10.0)
    }
    it("should not work for empty networks"){
      intercept[IllegalArgumentException]{
        NetworkMeasures.averageDegree(empty)
      }
    }
  }

  describe("An average clustering coefficient"){
    val empty = Network(Set[Int](),Map[Int,Map[Int,Int]]())
    it("should not work for empty networks"){
      intercept[IllegalArgumentException]{
        NetworkMeasures.averageClusteringCoefficient(empty)
      }
    }
    it("should work for networks with no edges"){
      val net = empty.addNodes(List(0,1,2))
      assert(NetworkMeasures.averageClusteringCoefficient(net)===0.0)
    }
    it("should be correct"){
      var net = empty.addNodes(0 until 8 toList)
      net = net.addEdge(1,2,1)
      net = net.addEdge(1,3,1)
      net = net.addEdge(2,3,1)
      net = net.addEdge(3,2,1)
      net = net.addEdge(5,2,1)
      net = net.addEdge(4,3,1)
      net = net.addEdge(3,4,1)
      net = net.addEdge(4,2,1)
      net = net.addEdge(6,7,1)
      val avgCC = NetworkMeasures.averageClusteringCoefficient(net)
      assert(avgCC===0.375)

    }
  }

  describe("An average path length"){
    val empty = Network(Set[Int](),Map[Int,Map[Int,Int]]())
    it("should work from a single node"){
      var net = empty.addNodes(List(0,1,2,3))
      net = net.addEdge(0,1,1)
      net = net.addEdge(1,2,1)
      val expected = (3,2)
      val actual = NetworkMeasures.nodePathLength(net,0)
      assert(actual===expected)
    }
    it("should work from a node with no connections"){
      var net = empty.addNodes(List(0,1,2,3))
      net = net.addEdge(0,1,1)
      net = net.addEdge(1,2,1)
      val expected = (0,0)
      val actual = NetworkMeasures.nodePathLength(net,3)
      assert(actual===expected)
    }
    it("should work for a whole network"){
      var net = empty.addNodes(List(0,1,2,3))
      net = net.addEdge(0,1,1)
      net = net.addEdge(1,2,1)
      val expected = 4/3.0
      val actual = NetworkMeasures.averagePathLength(net)
      assert(actual===expected)
    }
    it("should be 0 for a network with no connections"){
      val net = empty.addNodes(0 until 10 toList)
      val pl = NetworkMeasures.averagePathLength(net)
      assert(pl===0.0)
    }
  }

  describe("The NetworkIO object"){
    import NetworkIO._
    it("should be able to load in adjacency matrices"){
      val reader = URLReader("er.adj",this)
      val net = loadAdjacencyMatrix(reader)
      assert(net.nodes.size===297)
      assert(net.numEdges===2327)
    }
  }

}
