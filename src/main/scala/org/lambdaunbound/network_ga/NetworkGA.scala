package org.lambdaunbound.network_ga

case class Gene(net:Network[Int,Int],mutationRate:Double) extends Mutatable[Gene] {
  //TODO: Add parent to the Gene (or parents)
  import java.util.Random
  type IntNet = Network[Int,Int]

  def mutate(rnd:Random):Gene = {
    val mutNet = mutateNetwork(rnd)
    val mutR = mutateRate(rnd)
    Gene(mutNet,mutR)
  }

  def mutateNetwork(rnd:Random):IntNet = {
    val allNodes = (0 until net.nodes.size).toList

    def mutateNet(nodes:List[Int],net:IntNet,rnd:Random):IntNet = nodes match{
      case Nil => net
      case n::ns => mutateNet(ns,mutateNode(allNodes,n,net,rnd),rnd)
    }
    def mutateNode(nodes:List[Int],node:Int,net:IntNet,rnd:Random):IntNet = nodes match {
      case Nil => net
      case n::ns if(n==node) => mutateNode(ns,node,net,rnd)
      case n::ns if(rnd.nextDouble()<mutationRate) => {
        val newNet = if(net.getEdge(node,n)==None)net.addEdge(node,n,1) else net.removeEdge(node,n)
        mutateNode(ns,node,newNet,rnd)
      }
      case n::ns => mutateNode(ns,node,net,rnd)
    }

    mutateNet(allNodes,net,rnd)
  }

  def mutateRate(rnd:Random):Double = if(rnd.nextDouble()<scala.math.sqrt(mutationRate))rnd.nextDouble() else mutationRate

}


object NetworkGA {
  type IntNet = Network[Int,Int]
  import org.lambdaunbound.network_ga.{NetworkMeasures => NM}
  import java.io._
  import java.util.Random

  def main(args:Array[String]){
    //process input
    val popSize = args(0).toInt
    val gens = args(1).toInt
    val output = args(2)
    val degree = args(3).toDouble
    val pathLength = args(4).toDouble
    val cluster = args(5).toDouble
    val nodes = args(6).toInt

    val f1 = {g:Gene=> (NM.averageDegree(g.net)-degree).abs}
    val f2 = {g:Gene=> (NM.averagePathLength(g.net)-pathLength).abs}
    val f3 = {g:Gene=> (NM.averageClusteringCoefficient(g.net)-cluster).abs}

    val objs = Objectives(List(f1,f2,f3))

    val rnd = new Random(0)
    //val out = new BufferedWriter(new PrintWriter(output))

    //set up initial pop

    val emptyNet = Network.create[Int,Int]
    val startNet = emptyNet.addNodes(0 until nodes toList)
    val startMutRate = 0.01
    val tmpGene = Gene(startNet,startMutRate).mutate(rnd)
    val startGene = Gene(tmpGene.net,startMutRate)


    val pop = Population[Gene](objs.all((List(startGene))))

    def eogf(gen:Int,pop:Population[Gene]){
      println("End of Generation " + (gen+1)+", Pop size " + pop.pop.length)
      val p = pop.pop.map(sg=>sg.doms+" "+sg.scores.mkString(" ")+" "+sg.gene.mutationRate).mkString("\n")
      println(p)
    }

    //do the evo
    val outPop = evo(gens,popSize,objs,rnd,pop,Some(eogf _))

    //output results
    eogf(gens-1,outPop)
  }

  def evo[G<:Mutatable[G]](noGen:Int,popSize:Int,objs:Objectives[G],rnd:Random,startPop:Population[G],endOfGenFunction:Option[(Int,Population[G])=>Unit]=None):Population[G] = {
    def generation(gen:Int,pop:Population[G]):Population[G] = {
      if(gen==noGen) pop
      else {
        val genPop = pop.createPop(rnd).take(popSize).toList
        val paretoOrdered = pop.++(objs.all(genPop)).paretoOrdered
        val undominated = paretoOrdered.filter(_.doms==0)
        val nextPop = if(undominated.length>popSize)
            undominated
          else
            paretoOrdered.take(popSize)
        endOfGenFunction.map(_(gen,Population(nextPop)))
        generation(gen+1,Population(nextPop))
      }
    }
    generation(0,startPop)

  }
  

}


