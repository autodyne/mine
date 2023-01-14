package apps

import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer

object Eta {
	var eta = 1e-5
}

trait Node[T] extends (Seq[Int] => T)

case class Question[T](x: Seq[(Seq[Int], T)], limit: Double = Eta.eta) extends Node[T] {
	lazy val m = x.groupBy(_._2).maxBy(_._2.size)._1
	lazy val p = x.groupBy(_._2).map(_._2.size.toDouble / x.size)
	lazy val ent = p.map(p => -p * math.log(p)).sum / math.log(2)
	lazy val division = x.head._1.indices.map(split).minBy(_.ent)
	def apply(x: Seq[Int]) = if(ent - division.ent < limit) m else division(x)
	def split(v: Int) = x.map(_._1(v)).toSet.map(Division(x,v,_)).minBy(_.ent)
}

case class Division[T](x: Seq[(Seq[Int], T)], axis: Int, value: Int) extends Node[T] {
	val sn1 = Question(x.filter(_._1(axis) >  value))
	val sn2 = Question(x.filter(_._1(axis) <= value))
	val ent = (sn1.ent * sn1.x.size + sn2.ent * sn2.x.size) / x.size
	def apply(x: Seq[Int]) = if(x(axis) >= value) sn1(x) else sn2(x)
}

case class Bagging[T](x: Seq[(Seq[Int], T)], t: Int, n: Int) extends Node[T] {
	val f = Seq.fill(t)(Question(Seq.fill(n)(x(util.Random.nextInt(x.size)))))
	def apply(x: Seq[Int]) = f.map(_(x)).groupBy(identity).maxBy(_._2.size)._1
}

case class AdaBoost[T](x: Seq[(Seq[Int], T)], m: Int) extends Node[T] {
	val k = x.map(_._2).toSet
	val t = Seq(AdaStage(x, Seq.fill(x.size)(1.0 / x.size), m)).toBuffer
	def apply(x: Seq[Int]) = k.maxBy(y => t.map(_.score(x, y)).sum)
	while(t.last.best.error < 0.5) t += AdaStage(x, t.last.next, m)
}

case class AdaStage[T](x: Seq[(Seq[Int], T)], p: Seq[Double], m: Int) extends Node[T] {
	val best = List.fill(m)(Resample(x, p.map(_ / p.sum))).minBy(_.error)
	val gain = math.log((1 / best.error - 1) * (x.map(_._2).toSet.size - 1))
	val next = x.map(score).map(gain - _).map(math.exp).zip(p).map(_ * _)
	def score(x: Seq[Int], y: T) = if(this(x) == y) gain else 0
	def apply(x: Seq[Int]) = best(x)
}

case class Resample[T](x: Seq[(Seq[Int], T)], p: Seq[Double]) extends Node[T] {
	val data = Seq[(Seq[Int], T)]().toBuffer
	def reject(i: Int) = if(util.Random.nextDouble * p.max < p(i)) x(i) else null
	while(data.size < p.size) data += reject(util.Random.nextInt(p.size)) -= null
	def error = x.map((x, y) => this(x) != y).zip(p).filter(_._1).map(_._2).sum
	def apply(x: Seq[Int]) = quest(x)
	val quest = Question(data.toList)
}

object DT {
	val range = -100 to 100
	def main() = {
		val size = 300
		val random = new util.Random()
		val buff = new ArrayBuffer[(Seq[Int], Int)]
		val M = Seq(Seq(+60, +60), Seq(+20, -20))
		val S = Seq(Seq(+16, +16), Seq(+20, +20))
		val W = Seq(0.4, 0.6)
		for (k <- 0 until W.size; i <- 1 to (W(k) * size).toInt) {
			val x = (random.nextGaussian * S(k)(0) + M(k)(0)).toInt
			val y = (random.nextGaussian * S(k)(1) + M(k)(1)).toInt
			if (range.contains(x) && range.contains(y)) {
				buff += ((Seq(+x, +y), +k))
				buff += ((Seq(-x, -y), -k))
			}
		}
		val K = 50
		val data = buff.toSeq
		test(s"plain", Question(data), data)
		test(s"bag$K", Bagging (data, K, size / 5), data)
		test(s"ada$K", AdaBoost(data, K), data)
		Eta.eta = 1e-1
		test(s"prune", Question(data), data)
	}
	def test(id: String, root: Node[Int], data: Seq[(Seq[Int], Int)]) = {
		util.Try {
			val out = new PrintStream("data0.dat")
			for((d, k) <- data if k == -1) out.println(d.mkString(","))
			out.close
		}
		util.Try {
			val out = new PrintStream("data1.dat")
			for((d, k) <- data if k ==  0) out.println(d.mkString(","))
			out.close
		}
		util.Try {
			val out = new PrintStream("data2.dat")
			for((d, k) <- data if k == +1) out.println(d.mkString(","))
			out.close
		}
		util.Try {
			val out = new PrintStream("class.dat")
			for(y <- range) out.println(range.map(x => root(Seq(x, y))).mkString(","))
			out.close
		}
		exec.Python.run("DT", id)
	}
}
