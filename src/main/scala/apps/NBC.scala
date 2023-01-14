package apps

import java.io.PrintStream
import scala.io.Source

class NaiveBayes[D<:Seq[W],W,C](texts: Seq[D], classes: Seq[C]) {
	val nw = scala.collection.mutable.Map[(W,C),Double]().withDefaultValue(1)
	val pc = classes.groupBy(identity).map(_ -> _.size.toDouble / texts.size)
	def pwc(c: C)(w: W) = nw(w,c) / texts.flatten.distinct.map(nw(_,c)).sum
	def pcd(d: D)(c: C) = math.log(pc(c)) + d.map(pwc(c)).map(math.log).sum
	def apply(d: D) = classes.distinct.maxBy(pcd(d))
	for((d,c) <- texts.zip(classes); w <- d) nw(w,c) += 1
}

object NBC {
	def main() = {
		val prefs = for(p<-1 to 47) yield {
			val src = Source.fromResource("p%02d".format(p))
			val words = src.getLines.toList
			src.close
			words.head -> words.tail
		}
		val area2 = for(r <-1 to 2) yield {
			val src = Source.fromResource("r2%d".format(r))
			val words = src.getLines.toList.tail
			src.close
			words.head -> words.tail
		}
		val area8 = for(r <-1 to 8) yield {
			val src = Source.fromResource("r8%d".format(r))
			val words = src.getLines.toList.tail
			src.close
			words.head -> words.tail
		}
		val nb2 = new NaiveBayes(area2.map(_._2), area2.map(_._1)) {
			val out = new PrintStream("pref.dat")
			for((pref,words) <- prefs) out.println("%s,%s".format(pref, this(words)))
			out.close
			exec.Python.run("NBC", 2)
		}
		val nb8 = new NaiveBayes(area8.map(_._2), area8.map(_._1)) {
			val out = new PrintStream("pref.dat")
			for((pref,words) <- prefs) out.println("%s,%s".format(pref, this(words)))
			out.close
			exec.Python.run("NBC", 8)
		}
	}
}
