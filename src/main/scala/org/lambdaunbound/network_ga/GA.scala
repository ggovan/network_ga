package org.lambdaunbound.network_ga

case class Objectives[-G](obs:List[G=>Double]) {
    def apply[B<:G](gene:B):ScoredGene[B] = ScoredGene(gene,obs.map(_(gene)),0)
    def all[B<:G](genes:List[B]):List[ScoredGene[B]] = genes.par.map(apply(_)).toList
    def &&[C<:G](f:C=>Double):Objectives[C] = Objectives(obs++List(f))
}

case class ScoredGene[G](gene:G,scores:List[Double],doms:Int){
  def dominate(other:ScoredGene[G]):Boolean = {
    assert(scores.size==other.scores.size)
    val zipped = scores.zip(other.scores)
    zipped.forall{case (a,b)=>a<=b}&&zipped.exists{case (a,b)=>a<b}
  }

  def countDominated(others:Seq[ScoredGene[G]]):ScoredGene[G] = others match{
    case Nil => this
    case g::gs => 
      if(g.dominate(this))
        ScoredGene(gene,scores,doms+1).countDominated(gs)
      else
        countDominated(gs)
  }

  lazy val clearDominated = if(doms!=0)ScoredGene(gene,scores,0)else this

  override def toString:String = {
    "ScoredGene\n"+
    scores.mkString("\t")+"\tdoms:"+doms+"\n"+
    gene + "\n"
  }
}

trait Mutatable[G] {
  this:G =>
  def mutate(rnd:java.util.Random):G
}

case class Population[G<:Mutatable[G]](pop:List[ScoredGene[G]]){
  import java.util.Random

  lazy val paretoOrdered:List[ScoredGene[G]] = {
    val domList = pop.map(_.clearDominated.countDominated(pop))
    domList.sortBy(_.doms)
  }

  def ++(gl:List[ScoredGene[G]]):Population[G] = Population(pop++gl)

  def select(rnd:java.util.Random):G = {
    val size = paretoOrdered.size
    val next = rnd.nextInt(size)
    paretoOrdered(next).gene
  }

  lazy val dominanceTournametList:List[(Double,ScoredGene[G])] = {
    val maxD = paretoOrdered.last.doms+1
    val total = paretoOrdered.foldLeft(0)(_+maxD-_.doms).toDouble
    paretoOrdered.map(sg=>((maxD-sg.doms)/total,sg))
  }

  def dominanceTournamentSelect(rnd:Random):G = tournamentSelect(rnd.nextDouble,dominanceTournametList)

  def tournamentSelect(p:Double,l:List[(Double,ScoredGene[G])]):G = l match {
    case Nil => throw new AssertionError("The % of all items is less than 1.")
    case (q,g)::ls =>
      if(p<q) g.gene
      else tournamentSelect(p-q,ls)
  }

  lazy val crowdedProbabiltyList:List[(Double,ScoredGene[G])] = {
    val list = paretoOrdered
    val starting:List[(Double,Double)] = list.head.scores.map(x=>(x,x))
    val minMaxList:List[(Double,Double)] = list.foldLeft(starting){(mm,sg) =>
      sg.scores.zip(mm).map{case (v,(min,max)) => (math.min(v,min),math.max(v,max))}
    }
    val normalisedList:List[(List[Double],ScoredGene[G])] = list.map{sg =>
      (sg.scores.zip(minMaxList).map{case (v,(min,max)) =>
        (v-min)/(max-min)
      },sg)
    }
    val distancesList:List[(Double,ScoredGene[G])] = for{g1 <- normalisedList
      g2 <- normalisedList
      val td = g1._1.zip(g2._1).map(x=>math.pow(x._1-x._2,2)).reduce(_+_)
      val dist = math.sqrt(td)
      if(dist!=0)
    } yield (dist,g1._2)

    val distancedList = distancesList.groupBy(_._2).toList.map{case (g,ldg) =>
      (ldg.map(_._1).sum,g)
    }

    val sortedDistancedList = distancedList.sortBy(_._1)
    val indexes = 1 to sortedDistancedList.size
    val total = indexes.sum.toDouble
    
    sortedDistancedList.zip(indexes).map{case ((d,sg),i) => (i/total,sg)}
  }

  def crowdingSelecter(rnd:Random):G = tournamentSelect(rnd.nextDouble,crowdedProbabiltyList)

  def createPop(rnd:java.util.Random):Stream[G] = {
    val selected:G = crowdingSelecter(rnd)
    val g = selected.mutate(rnd)
    g#::createPop(rnd)
  }

}
