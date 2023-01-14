package apps

import java.io._

class Kmeans(x: Seq[Seq[Double]], k: Int, epochs: Int = 100) {
	val mu = Array.fill(k, x.map(_.size).min)(math.random)
	def apply(x: Seq[Double]) = mu.map(quads(x)(_).sum).zipWithIndex.minBy(_._1)._2
	def quads(a: Seq[Double])(b: Seq[Double]) = a.zip(b).map(_-_).map(d=> d * d)
	def estep = x.groupBy(apply).values.map(c=> c.transpose.map(_.sum / c.size))
	for(epoch <- 0 until epochs) estep.zip(mu).foreach((e,m)=> e.copyToArray(m))
}

class GMM(val d: Int, val k: Int) {
	val w = Array.fill(k)(1.0 / k)
	val m -> s = (Array.fill(k, d)(math.random), Array.fill(k, d)(math.random))
	def apply(x: Seq[Double]) = w.lazyZip(m).lazyZip(s).map(Normal(x)(_,_,_).p)
}

case class Normal(x: Seq[Double])(w: Double, m: Seq[Double], s: Seq[Double]) {
	def n = math.exp(-0.5 * x.zip(m).map(_-_).map(d=>d*d).zip(s).map(_/_).sum)
	def p = w * n / math.pow(2 * math.Pi, 0.5 * x.size) / math.sqrt(s.product)
}

class EM(val x: Seq[Seq[Double]], val mm: GMM, epochs: Int = 100) {
	def mstep(P: Seq[Seq[Double]]) = {
		P.map(_.sum / x.size).copyToArray(mm.w)
		val m = P.map(_.zip(x).map((p,x) => x.map(x => p * x)).transpose.map(_.sum))
		val s = P.map(_.zip(x).map((p,x) => x.map(x => p*x*x)).transpose.map(_.sum))
		m.zip(P).map((m,p) => m.map(_ / p.sum)).zip(mm.m).foreach(_.copyToArray(_))
		s.zip(P).map((m,p) => m.map(_ / p.sum)).zip(mm.s).foreach(_.copyToArray(_))
		for((s,m) <- mm.s.zip(mm.m); d <- 0 until mm.d) s(d) -= m(d) * m(d)
	}
	for(epoch <- 1 to epochs) mstep(x.map(mm(_)).map(p=>p.map(_/p.sum)).transpose)
}

class VB(val x: Seq[Seq[Double]], val mm: GMM, epochs: Int = 1000, W: Double = 1) {
	val n = Array.fill(mm.k)(1.0 / mm.k)
	val w -> m = (Array.fill(mm.k, mm.d)(W), Array.fill(mm.k, mm.d)(math.random))
	for(epoch <- 1 to epochs) new MstepGMM(this, mm, new EstepGMM(this, mm).post)
}

class EstepGMM(vb: VB, mm: GMM) {
	val eq35 = vb.n.map(Digamma).map(_-Digamma(vb.n.sum))
	val eq3A = vb.n.map(n=>0.to(mm.d-1).map(d=>(n-d)/2).map(Digamma))
	val eq36 = eq3A.zip(vb.w).map(_.sum-_.map(math.log).sum).map(_/2)
	def wish = vb.x.toArray.map(_.toArray).map(vb.m-_).map(d=>d.mul(d).div(vb.w))
	def eq34 = wish.map(_.zip(vb.n).map(-_.sum/2*_))+eq35+eq36-vb.n.map(mm.d/_/2)
	def post = eq34.map(_.map(math.exp)).map(x=>x.map(_/x.sum)).toSeq.transpose
}

class MstepGMM(vb: VB, mm: GMM, post: Seq[Seq[Double]]) {
	new EM(vb.x, mm, 0).mstep(post)
	val eq11 = post.map(_.sum).toArray
	val eq38 = vb.n.zip(eq11).map(_+_)
	val eq39 = vb.m.mul(vb.n).div(eq38).add(mm.m.mul(eq11).div(eq38))
	val eq41 = vb.m.mul(vb.m).mul(vb.n).sub(eq39.mul(eq39).mul(eq38))
	val eq40 = mm.s.add(mm.m.mul(mm.m)).mul(eq11).add(vb.w.add(eq41))
	eq38.copyToArray(vb.n)
	eq39.zip(vb.m).foreach(_.copyToArray(_))
	eq40.zip(vb.w).foreach(_.copyToArray(_))
}

implicit class Vector(x: Array[Array[Double]]) {
	def +(y: Array[Double]) = x.map(_.zip(y).map(_+_))
	def -(y: Array[Double]) = x.map(_.zip(y).map(_-_))
	def add(y: Array[Double]) = x.zip(y).map((x,y) => x.map(_+y))
	def sub(y: Array[Double]) = x.zip(y).map((x,y) => x.map(_-y))
	def mul(y: Array[Double]) = x.zip(y).map((x,y) => x.map(_*y))
	def div(y: Array[Double]) = x.zip(y).map((x,y) => x.map(_/y))
	def add(y: Array[Array[Double]]) = x.zip(y).map(_.zip(_).map(_+_))
	def sub(y: Array[Array[Double]]) = x.zip(y).map(_.zip(_).map(_-_))
	def mul(y: Array[Array[Double]]) = x.zip(y).map(_.zip(_).map(_*_))
	def div(y: Array[Array[Double]]) = x.zip(y).map(_.zip(_).map(_/_))
}

object Digamma extends Function[Double, Double] {
	def apply(x: Double): Double = {
		var index -> value = (x, 0.0)
		def d = 1.0 / (index * index)
		while(index < 49) (value -= 1 / index, index += 1)
		val s = d * (1.0 / 12 - d * (1.0 / 120 - d / 252))
		(value + math.log(index) - 0.5 / index - s)
	}
}

object GMM {
	val range = (BigDecimal(-3.0) to 3.0 by 0.05).map(_.toDouble)
	def main() = {
		val size = 2000
		val random = new util.Random()
		val samples = Array.ofDim[Double](size, 2)
		val labels = Array.ofDim[Int](size)
		val M = Array(Seq(-1.0, -1.0), Seq(+1.0, +1.0))
		val S = Array(Seq(+0.4, +0.4), Seq(+0.6, +0.6))
		val W = Array(+0.4, +0.6)
		val K = W.size
		val J = K + 1
		val D = M.head.size
		val seg = new Array[Int](K + 1)
		for(i <- 1 to K) seg(i) = (W(i-1) * size).toInt
		for(i <- 1 to K) seg(i) = seg(i-1) + seg(i)
		seg(K) = size
		for (k <- 0 until K; i <- seg(k) until seg(k+1); d <- 0 until 2) {
			samples(i)(d) = random.nextGaussian * math.sqrt(S(k)(d)) + M(k)(d)
			labels(i) = k
		}
		val data = samples.map(_.toSeq).toSeq
		val truth = new GMM(D, K)
		W.copyToArray(truth.w)
		M.zip(truth.m).foreach(_.copyToArray(_))
		S.zip(truth.s).foreach(_.copyToArray(_))
		util.Try {
			val out = new PrintStream("train.dat")
			for(d <- data) out.println(d.mkString(","))
			out.close
		}
		util.Try {
			val out = new PrintStream("dense.dat")
			for(y <- range) out.println(range.map(x => truth(Seq(x,y)).sum).mkString(","))
			out.close
		}
		exec.Python.run("GMM", "truth", "dense", K)
		em(truth, K, data, "truth")
		km(new Kmeans(data, K), K, data, s"km.k$K")
		km(new Kmeans(data, J), J, data, s"km.k$J")
		em(new EM(data, new GMM(D, K)).mm, K, data, s"em.k$K")
		em(new EM(data, new GMM(D, J)).mm, J, data, s"em.k$J")
		em(new VB(data, new GMM(D, K)).mm, K, data, s"vb.k$K")
		em(new VB(data, new GMM(D, J)).mm, J, data, s"vb.k$J")
	}
	def classify(mm: GMM, x: Seq[Double]) = mm(x).zipWithIndex.maxBy(_._1)._2
	def km(km: Kmeans, K: Int, data: Seq[Seq[Double]], title: String) = {
		util.Try {
			val out = new PrintStream("cents.dat");
			for(k <- 0 until K) out.println(km.mu(k).mkString(","));
			out.close
		}
		for(k <- 0 until K) util.Try {
			val out = new PrintStream(s"mixt$k.dat")
			for(d <- data if km(d) == k) out.println(d.mkString(","))
			out.close
		}
		exec.Python.run("GMM", "KM", title, K)
	}
	def em(mm: GMM, K: Int, data: Seq[Seq[Double]], title: String) = {
		util.Try {
			val out = new PrintStream("cents.dat");
			for(k <- 0 until K) out.println(mm.m(k).mkString(","));
			out.close
		}
		util.Try {
			val out = new PrintStream("dense.dat")
			for(y <- range) out.println(range.map(x => mm(Seq(x,y)).sum).mkString(","))
			out.close
		}
		for(k <- 0 until K) util.Try {
			val out = new PrintStream(s"mixt$k.dat")
			for(d <- data if classify(mm,d) == k) out.println(d.mkString(","))
			out.close
		}
		exec.Python.run("GMM", "EM", title, K)
	}
}
