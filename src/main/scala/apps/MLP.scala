package apps

import java.io.PrintStream

trait Act {
	def fp(z: Seq[Double]): Seq[Double]
	def bp(y: Seq[Double]): Seq[Double]
}

class Sigmoid extends Act {
	def fp(z: Seq[Double]) = z.map(z => 1 / (1 + math.exp(-z)))
	def bp(z: Seq[Double]) = this.fp(z).map(y => y * (1.0 - y))
}

abstract class Neuron(val dim: Int) {
	def fp(x: Seq[Double]): Seq[Double]
	def bp(x: Seq[Double], t: Seq[Double]): Seq[Double]
}

class Output(dim: Int = 1, loss: (Double,Double)=>Double = _-_) extends Neuron(dim) {
	def fp(x: Seq[Double]) = x
	def bp(x: Seq[Double], t: Seq[Double]) = x.zip(t).map(loss.tupled)
}

class Hidden(dim: Int, act: Act, weight: ()=>SGD, next: Neuron) extends Neuron(dim) {
	lazy val w = List.fill(next.dim, dim)(weight())
	def fp(x: Seq[Double]) = next.fp(act.fp(wx(x)))
	def wx(x: Seq[Double]) = w.map(_.map(_.w).zip(x).map(_ * _).sum)
	def bp(x: Seq[Double], t: Seq[Double]) = ((z: Seq[Double]) => {
		val bp = next.bp(act.fp(z),t).zip(act.bp(z)).map(_ * _)
		for((w,g) <- w.zip(bp); (sgd,x) <- w.zip(x)) sgd(x * g)
		w.transpose.map(_.zip(bp).map(_.w * _).sum)
	})(wx(x))
}

class Offset(dim: Int, act: Act, weight: ()=>SGD, next: Neuron) extends Neuron(dim) {
	lazy val body = new Hidden(dim + 1, act, weight, next)
	def fp(x: Seq[Double]) = body.fp(x.padTo(dim + 1, 1d))
	def bp(x: Seq[Double], t: Seq[Double]) = body.bp(x.padTo(dim + 1, 1d), t).init
}

class Softmax extends Act {
	def fp(z: Seq[Double]) = z.map(math.exp(_)/z.map(math.exp).sum)
	def bp(z: Seq[Double]) = Seq.fill(z.size)(1.0)
}

object MLP {
	val data1 = for(x<-0 to 1;y<-0 to 1) yield (x,y,x|y)
	val data2 = for(x<-0 to 1;y<-0 to 1) yield (x,y,x^y)
	val data3 = Seq((0,1,0), (1,0,1), (0,-1,2), (-1,0,3))
	def main() = {
		val binary3 = new Output(1, _-_)
		val binary0 = new Offset(2, new Sigmoid, ()=>new PlainSGD, binary3)
		val binary2 = new Hidden(3, new Sigmoid, ()=>new PlainSGD, binary3)
		val binary1 = new Hidden(2, new Sigmoid, ()=>new PlainSGD, binary2)
		val offset2 = new Offset(3, new Sigmoid, ()=>new PlainSGD, binary3)
		val offset1 = new Offset(2, new Sigmoid, ()=>new PlainSGD, offset2)
		binary(binary0, "slp.class", data1)
		binary(binary1, "mlp.class", data2)
		binary(offset1, "mlp.const", data2)
		val select3 = new Output(4, _-_)
		val select0 = new Offset(3, new Softmax, ()=>new PlainSGD, select3)
		val select2 = new Offset(3, new Softmax, ()=>new PlainSGD, select3)
		val select1 = new Offset(2, new Sigmoid, ()=>new PlainSGD, select2)
		select(select0, "slp.zflag", data3, data3.size)
		select(select1, "mlp.zflag", data3, data3.size)
	}
	def binary(model: Neuron, id: String, data: Seq[(Int,Int,Int)]) = {
		val range = (BigDecimal(-1.0) to 2.0 by 0.005).map(_.toDouble)
		for(n<-1 to 500000; (x,y,t)<-data) model.bp(Seq(x,y), Seq(t))
		val out = new PrintStream("dist.dat")
		for(y <- range) out.println(range.map(x => model.fp(Seq(x, y)).head).mkString(","))
		out.close
		exec.Python.run("MLP", id)
	}
	def select(model: Neuron, id: String, data: Seq[(Int,Int,Int)], D: Int) = {
		val range = (BigDecimal(-2.0) to 2.0 by 0.005).map(_.toDouble)
		for(n<-1 to 1000000; (x,y,t)<-data) model.bp(Seq(x,y), Seq.tabulate(D)(d=>if(t==d) 1 else 0))
		val out = new PrintStream("dist.dat")
		for(y <- range) out.println(range.map(x => model.fp(Seq(x, y)).zipWithIndex.maxBy(_._1)._2).mkString(","))
		out.close
		exec.Python.run("MLP", id)
	}
}
