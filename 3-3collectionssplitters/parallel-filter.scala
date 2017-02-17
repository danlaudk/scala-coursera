// within any parallelizable Collection class?
def filter(p: T => Boolean) : Repr = {
  val splttr = this.par.splitter // splitter supposed to exist on any parallel collections class
  if(splttr.remaining < splttr.threshold) {
    val b = newCombiner 
    for (x <- splttr) if (p(x)) b += x // possible bc Combiner extends Builder
  }
  else {
    val children = for(child <- splttr.split) yield task { child.filter(p) } //recursive
    children.map(_.join()).reduce( (this,that) => this.combine(that))  
  }
}
