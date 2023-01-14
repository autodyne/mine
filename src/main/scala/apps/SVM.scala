package apps

import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer

case class Data(x: Seq[Double], t: Int, var l: Double = 0) {
	def kkt(svm: SVM, C: Double) = t * svm(this) match {
		case e if e < 1 => l >= C
		case e if e > 1 => l == 0
		case _ => true
	}
}

class SVM(data: Seq[Data], k: (Data, Data) => Double) {
	var const = 0.0
	def group(t: Int) = data.filter(_.t == t).map(apply)
	def apply(x: Data) = data.map(d => d.l * d.t * k(x,d)).sum + const
}

class SMO(data: Seq[Data], k: (Data, Data) => Double, C: Double = 1e-10) extends SVM(data,k) {
	while(data.filterNot(_.kkt(this,C)).size >= 2) {
		val a = data(util.Random.nextInt(data.size))
		val b = data(util.Random.nextInt(data.size))
		val min = math.max(-a.l, if(a.t == b.t) b.l - this.C else -b.l)
		val max = math.min(-a.l, if(a.t == b.t) b.l - this.C else -b.l) + C
		val prod = this(Data(a.x.zip(b.x).map(_-_), 0)) - this.const
		val best = -a.t * (prod - a.t + b.t) / (k(a,a) - 2 * k(a,b) + k(b,b))
		if(!best.isNaN) a.l += a.t * a.t * math.max(min, math.min(max, best))
		if(!best.isNaN) b.l -= a.t * b.t * math.max(min, math.min(max, best))
		this.const = -0.5 * (group(+1).min + group(-1).max) + this.const
	}
}

object SVM {
	val range = (BigDecimal(-2.0) to 2.0 by 0.05).map(_.toDouble)
	def main() = {
		val data1 = new ArrayBuffer[(Seq[Double], Int)]
		for(r <- (BigDecimal(0.0) to 2 * math.Pi by math.Pi / 18).map(_.toDouble)) {
			data1 += (Seq(0.75 + 1.0 * math.cos(r), 0.75 + 1.0 * math.sin(r)) -> +1)
			data1 += (Seq(-1.0 + 0.6 * math.cos(r), -1.0 + 0.6 * math.sin(r)) -> -1)
		}
		val data2 = ArrayBuffer[(Seq[Double], Int)](data1.toSeq :_*)
		data2 += (Seq(0.75, 0.75) -> -1)
		data2 += (Seq(-1.0, -1.0) -> +1)
		val data3 = new ArrayBuffer[(Seq[Double], Int)]
		for(r <- (BigDecimal(0.0) to 2 * math.Pi by math.Pi / 9).map(_.toDouble)) {
			val x1 = +1.2 + 0.4 * Math.cos(r)
			val y1 = +0.0 + 0.4 * Math.sin(r)
			val x2 = -1.2 + 0.4 * Math.cos(r)
			val y2 = +0.0 + 0.4 * Math.sin(r)
			data3 += (Seq(x1, y1) -> -1)
			data3 += (Seq(x2, y2) -> -1)
			val x3 = +0.0 + 0.4 * Math.cos(r)
			val y3 = +1.2 + 0.4 * Math.sin(r)
			val x4 = +0.0 + 0.4 * Math.cos(r)
			val y4 = -1.2 + 0.4 * Math.sin(r)
			data3 += (Seq(x3, y3) -> +1)
			data3 += (Seq(x4, y4) -> +1)
		}
		val data4 = new ArrayBuffer[(Seq[Double], Int)]
		for(r <- (BigDecimal(0.0) to 2 * math.Pi by math.Pi / 18).map(_.toDouble)) {
			val x1 = 1.6 * math.cos(r)
			val y1 = 1.6 * math.sin(r)
			data4 += (Seq(x1, y1) -> -1)
			val x2 = 0.6 * math.cos(r)
			val y2 = 0.6 * math.sin(r)
			data4 += (Seq(x2, y2) -> +1)
		}
		test("svm.line1", data1.toSeq, 1e300, (a, b) => a.zip(b).map(_*_).sum)
		test("svm.line2", data2.toSeq, 1e-10, (a, b) => a.zip(b).map(_*_).sum)
		test("svm.kern1", data3.toSeq, 1e-10, (a, b) => math.exp(- 8 * a.zip(b).map(_-_).map(math.pow(_, 2)).sum))
		test("svm.kern2", data4.toSeq, 1e-10, (a, b) => math.exp(- 8 * a.zip(b).map(_-_).map(math.pow(_, 2)).sum))
	}
	def test(id: String, data: Seq[(Seq[Double], Int)], C: Double, k: (Seq[Double],Seq[Double])=>Double) = {
		val svm = new SMO(data.map{case (x,t)=>Data(x,t)}.toSeq, (x,y) => k(x.x,y.x), C)
		val out0 = new PrintStream("data0.dat"); for((x,t) <- data if t == +1) out0.println(x.mkString(",")); out0.close;
		val out1 = new PrintStream("data1.dat"); for((x,t) <- data if t == -1) out1.println(x.mkString(",")); out1.close;
		val out2 = new PrintStream("dense.dat")
		for(y <- range) out2.println(range.map(x => svm(Data(Seq(x, y),0))).mkString(","))
		out2.close
		exec.Python.run("SVM", id)
	}
}
