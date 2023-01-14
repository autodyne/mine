package apps

import java.io.PrintStream

class RNN(dim: Int, hidden: Neuron, output: Neuron, value: Double = 0) extends Neuron(dim) {
	val hist = Seq[Seq[Double]]().toBuffer
	val loop = Seq[Seq[Double]]().toBuffer
	def fp(x: Seq[Double]) = output.fp(hp(x).last)
	def tt(x: Seq[Double]) = hist.zip(loop).map(_++_).foldRight(x)
	def hp(x: Seq[Double]) = loop.append(hidden.fp(hist.append(x).last++loop.last))
	def bp(x: Seq[Double], t: Seq[Double]) = tt(output.bp(hp(x).last,t))(hidden.bp)
	def init = hist.clear -> loop.clear -> loop.append(Seq.fill(hidden.dim)(value))
}

class DelaySGD(sgd: SGD = new PlainSGD, var d: Double = 0) extends SGD {
	def apply(dE: Double) = (sgd.w = w, sgd(dE), d += sgd.w - w)
	def force = (w += d, d = 0)
}

object RNN {
	def main(): Unit = {
		val num = 50
		val dI = 1
		val dO = 1
		val dH = 3
		val sin1 = Seq.tabulate(2 * num)(n => Seq(math.cos(2 * math.Pi * n / num) / 2 + 0.5))
		val sin2 = Seq.tabulate(2 * num)(n => Seq(math.sin(2 * math.Pi * n / num) / 2 + 0.5))
		val weights = Seq[DelaySGD]().toBuffer
		def delaySGD() = weights.append(new DelaySGD(new PlainSGD(0.02))).last
		val hidden = new Offset(dI + dH, new Sigmoid, delaySGD, new Output(dH, (x,t)=>t))
		val output = new Offset(dH, new Sigmoid, delaySGD, new Output(dO))
		val rnn = new RNN(1, hidden, output)
		for (step <- 1 to 50000) rnn.init -> sin1.zip(sin2).foreach(rnn.bp(_,_) -> weights.foreach(_.force))
		rnn.init
		val out = new PrintStream("wave.dat")
		sin1.lazyZip(sin1.map(rnn.fp)).lazyZip(sin2).foreach((x,y,t)=>out.printf("%.5f,%.5f,%.5f\n", x.head, y.head, t.head))
		out.close
		exec.Python.run("RNN")
	}
}
