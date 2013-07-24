package org.lambdaunbound.network_ga

case class Objectives[G](obs:List[G=>Double]) {
    def apply(gene:G):ScoredGene[G] = ScoredGene(gene,obs.map(_(gene)),0)
    def all(genes:List[G]):List[ScoredGene[G]] = genes.par.map(apply(_)).toList
    def &&(f:G=>Double):Objectives[G] = Objectives(obs++List(f))
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

  lazy val clearDominated = ScoredGene(gene,scores,0)
}

trait Mutatable[G] {
  this:G =>
  def mutate(rnd:java.util.Random):G
}

case class Population[G<:Mutatable[G]](pop:List[ScoredGene[G]]){

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

  def createPop(rnd:java.util.Random):Stream[G] = {
    val selected:G = select(rnd)
    val g = selected.mutate(rnd)
    g#::createPop(rnd)
  }

}
