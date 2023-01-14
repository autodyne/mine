package apps

case class Word[W](v: W, k: Int) {
	var z = util.Random.nextInt(k)
}

class LDA[D,W](texts: Map[D,Seq[W]], val k: Int, a: Double = 0.1, b: Double = 0.01) {
	val words = texts.map(_ -> _.map(Word(_,k)))
	val vocab = words.flatMap(_._2).groupBy(_.v)
	val nd = words.map((d,s) => d -> Array.tabulate(k)(k => s.count(_.z == k) + a)).toMap
	val nv = vocab.map((v,s) => v -> Array.tabulate(k)(k => s.count(_.z == k) + b)).toMap
	val nk = Array.tabulate(k)(k => nv.map(_._2(k)).sum)
	def apply(k: Int) = vocab.keys.toList.filter(v => nv(v).max == nv(v)(k))
	def probs(v: W, d: D) = 0.until(k).map(k => nv(v)(k) * nd(d)(k) / nk(k))
}

class Gibbs[D,W](texts: Map[D,Seq[W]], k: Int, epochs: Int = 500) extends LDA(texts, k) {
	for(epoch <- 1 to epochs; (document,words) <- util.Random.shuffle(words); w <- words) {
		nk(w.z) -= 1
		nv(w.v)(w.z) -= 1
		nd(document)(w.z) -= 1
		val uni = util.Random.between(0, probs(w.v,document).sum.toDouble)
		w.z = probs(w.v,document).scan(0.0)(_+_).tail.indexWhere(_ >= uni)
		nd(document)(w.z) += 1
		nv(w.v)(w.z) += 1
		nk(w.z) += 1
	}
}

object LDA {
	def main() = {
		val bases = Seq(2,3,5,7)
		def sample(n: Int, m: Int, k: Int) = Seq.fill(n)(k * util.Random.nextInt(m / k + 1))
		val texts = Seq.fill(1000)(bases.map(sample(util.Random.nextInt(100),80,_)).flatten)
		val gibbs = new Gibbs(texts.indices.zip(texts).toMap, bases.size)
		println("LDA:")
		for(k <- 0.until(gibbs.k)) println(gibbs(k).sorted.mkString(","))
		println("kmeans:")
		val kmeans = new Kmeans(gibbs.nv.values.map(_.toList).toSeq, gibbs.k)
		val groups = texts.flatten.distinct.groupBy(v => kmeans(gibbs.nv(v)))
		for(group <- groups.values) println(group.toSeq.sorted.mkString(","))
	}
}
