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

  def dominanceTournamentSelect(rnd:Random):G = {
    def tselect(p:Double,l:List[(Double,ScoredGene[G])]):G = l match {
      case Nil => throw new AssertionError("The % of all items is less than 1.")
      case (q,g)::ls =>
        if(p<q) g.gene
        else tselect(p-q,ls)
    }
    tselect(rnd.nextDouble,dominanceTournametList)
  }

  def createPop(rnd:java.util.Random):Stream[G] = {
    val selected:G = dominanceTournamentSelect(rnd)
    val g = selected.mutate(rnd)
    g#::createPop(rnd)
  }

}
