package org.lambdaunbound.network_ga

case class Gene(
  net:Network[Int,Int],
  mutationRate:Double,
  parent:Option[String]=None,
  id:String="0:0")
  extends Mutatable[Gene] with HasNet {

  //TODO: Ugly hacks! Mutable data! Fix it, fix it fix it!
  val gen = parent.map(_.takeWhile(_ != ':').toInt).getOrElse(-1)+1
  var children = -1

  import java.util.Random
  type IntNet = Network[Int,Int]

  def mutate(rnd:Random):Gene = {
    logger.debug("Mutating Gene")
    val mutR = mutateRate(rnd)
    val mutNet = mutateNetwork(rnd,mutR)
    children+=1
    Gene(mutNet,mutR,Some(id),gen+":"+children)
  }

  def mutateNetwork(rnd:Random,mutR:Double):IntNet = {
    val allNodes = (0 until net.nodes.size).toList

    def mutateNet(nodes:List[Int],net:IntNet,rnd:Random):IntNet = nodes match{
      case Nil => net
      case n::ns => mutateNet(ns,mutateNode(allNodes,n,net,rnd),rnd)
    }
    def mutateNode(nodes:List[Int],node:Int,net:IntNet,rnd:Random):IntNet = nodes match {
      case Nil => net
      case n::ns if(n==node) => mutateNode(ns,node,net,rnd)
      case n::ns if(rnd.nextDouble()<mutR) => {
        val newNet = if(net.getEdge(node,n)==None)net.addEdge(node,n,1) else net.removeEdge(node,n)
        mutateNode(ns,node,newNet,rnd)
      }
      case n::ns => mutateNode(ns,node,net,rnd)
    }

    mutateNet(allNodes,net,rnd)
  }

  def rewireNetwork(rnd:Random,mutR:Double):IntNet = {
    def rewireEdge(current:(Int,Int),net:IntNet):IntNet = {
      val nn1 = rnd.nextInt(net.nodes.size)
      val nn2 = rnd.nextInt(net.nodes.size)
      if(nn1==nn2 || net.getEdge(nn1,nn2)!=None)
        rewireEdge(current,net)
      else
        net.removeEdge(current._1,current._2).addEdge(nn1,nn2,1)
    }
    def rewire(edges:List[(Int,Int)],net:IntNet):IntNet = edges match {
      case Nil => net
      case (n1,n2)::ns => if(rnd.nextDouble() < mutR) {
        val newNet = rewireEdge((n1,n2),net)
        rewire(ns,newNet)
      }
      else{
        rewire(ns,net)
      }
    }
    rewire(net.edgeList,net)
  }

  def mutateRate(rnd:Random):Double = if(rnd.nextDouble()<scala.math.sqrt(mutationRate))rnd.nextDouble() else mutationRate

  override def toString:String = {
    "NetworkGene:"+id+"\n"+
    "Parent:"+parent.getOrElse("")+"\n" 
  }

}

case class MultiMutateNetworkGene(
  net:Network[Int,Int],
  mutRates:Seq[Double],
  parent:Option[String],
  id:String="0:0")
  extends Mutatable[MultiMutateNetworkGene] with HasNet{

  //TODO: Fix as above
  val gen = parent.map(_.takeWhile(_ != ':').toInt).getOrElse(-1)+1
  var children = -1

  import java.util.Random
  type IntNet = Network[Int,Int]

  def mutate(rnd:Random):MultiMutateNetworkGene = {
    logger.debug("Mutating MMNGene")
    val mutNet = mutateNetwork(rnd)
    val mutRs = mutateRates(rnd)
    children+=1
    MultiMutateNetworkGene(mutNet,mutRs,Some(id),gen+":"+children)
  }

  def mutateNetwork(rnd:Random):Network[Int,Int] = {
    val allNodes = (0 until net.nodes.size).toList
    val nodesAndMR = allNodes.zip(mutRates)

    def mutateNet(nodes:List[(Int,Double)],net:IntNet,rnd:Random):IntNet = nodes match{
      case Nil => net
      case (node,mRate)::ns => mutateNet(ns,mutateNode(allNodes,node,mRate,net,rnd),rnd)
    }

    def mutateNode(nodes:List[Int],node:Int,mRate:Double,net:IntNet,rnd:Random):IntNet = nodes match {
      case Nil => net
      case n::ns if(n==node) => mutateNode(ns,node,mRate,net,rnd)
      case n::ns if(rnd.nextDouble()<mRate) => {
        val newNet = if(net.getEdge(node,n)==None)net.addEdge(node,n,1) else net.removeEdge(node,n)
        mutateNode(ns,node,mRate,newNet,rnd)
      }

      case n::ns => mutateNode(ns,node,mRate,net,rnd)
    }

    mutateNet(nodesAndMR,net,rnd)
  }

  def mutateRates(rnd:Random):Seq[Double] = mutRates.map(mr=>if(rnd.nextDouble()<scala.math.sqrt(mr))rnd.nextDouble() else mr)
}

trait HasNet {
  def net:Network[Int,Int]
}


object NetworkGA {
  type IntNet = Network[Int,Int]
  import org.lambdaunbound.network_ga.{NetworkMeasures => NM}
  import java.io._
  import java.util.Random

  def main(args:Array[String]){
    //process input
    logger.info("Starting Network_GA")
    val popSize = args(0).toInt
    val gens = args(1).toInt
    val output = args(2)
    val degree = args(3).toDouble
    val pathLength = args(4).toDouble
    val cluster = args(5).toDouble
    val nodes = args(6).toInt

    val f1 = {g:HasNet=> (NM.averageDegree(g.net)-degree).abs}
    val f2 = {g:HasNet=> (NM.averagePathLength(g.net)-pathLength).abs}
    val f3 = {g:HasNet=> (NM.averageClusteringCoefficient(g.net)-cluster).abs}

    val objs = Objectives(List(f1,f2,f3))

    val rnd = new Random(0)
    val out = new PrintWriter(output)
    outing = Some(new PrintWriter(output+".complete"))

    //set up initial pop

    val emptyNet = Network.create[Int,Int]
    val startNet = emptyNet.addNodes(0 until nodes toList)
    val erMR = (degree)/((nodes-1).toDouble)
    println(erMR)
    val startMutRate = 0.01
    val startMRL = (0 until nodes).map(_=>startMutRate)
    val tmpNet = Gene(startNet,erMR,None).mutateNetwork(rnd,erMR)
    val tmpGene = MultiMutateNetworkGene(startNet,startMRL,None).mutate(rnd)
//    val startGene = MultiMutateNetworkGene(tmpGene.net,startMRL,None)
    val startGene = Gene(tmpNet,startMutRate,None)

    val pop = Population(objs.all((List(startGene))))

    def eogf(gen:Int,pop:Population[Gene]){
      println("End of Generation " + (gen+1)+", Pop size " + pop.pop.length)
      val p = pop.pop.map(sg=>sg.doms+" "+sg.scores.mkString(" ")/*+" "+sg.gene.mutationRate*/).mkString("\n")
      println(p)
    }

    //do the evo
    logger.info("Starting evolution")
    val outPop = evo(gens,popSize,objs,rnd,pop,Some(eogf _))

    //output results
    eogf(gens-1,outPop)

    logger.info("Writing out final population")
    using(out){out=>
      outPop.pop.map(_.gene.net.out(out))
    }
    outing.map(_.close)
  }

  var outing:Option[PrintWriter] = None

  def evo[G<:Mutatable[G],B>:G](noGen:Int,popSize:Int,objs:Objectives[B],rnd:Random,startPop:Population[G],endOfGenFunction:Option[(Int,Population[G])=>Unit]=None):Population[G] = {
    def generation(gen:Int,pop:Population[G]):Population[G] = {
      if(gen==noGen) pop
      else {
        logger.info("Evolving generation {}",gen+1)
        outing.map(_.println("GENERATION:"+gen+1))
        val genPop = pop.createPop(rnd).take(popSize).toList
        logger.debug("Evaluating and sorting generation {}",gen+1)
        val scoredPop = objs.all(genPop)
        outing.map(o => scoredPop.foreach(o.print(_)))
        val paretoOrdered = pop.++(scoredPop).paretoOrdered
        val undominated = paretoOrdered.filter(_.doms==0)
        val nextPop = if(undominated.length>popSize)
            undominated
          else
            paretoOrdered.take(popSize)
        logger.debug("Call end of gen function with generation {}",gen+1)
        endOfGenFunction.map(_(gen,Population(nextPop)))
        generation(gen+1,Population(nextPop))
      }
    }
    generation(0,startPop)

  }
  

}


