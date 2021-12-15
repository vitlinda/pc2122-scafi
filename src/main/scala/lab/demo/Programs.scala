package lab.demo

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ExportEvaluation.EXPORT_EVALUATION
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.{ScafiProgramBuilder, ScafiWorldInformation}
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ScafiWorldIncarnation.EXPORT
import it.unibo.scafi.simulation.s2.frontend.view.{ViewSetting, WindowConfiguration}
import lab.gui.patch.RadiusLikeSimulation
import it.unibo.scafi.space.graphics2D.BasicShape2D.Circle

object Incarnation extends BasicAbstractIncarnation
import lab.demo.Incarnation._ //import all stuff from an incarnation

object Simulation extends App {

  val formatter_evaluation: EXPORT_EVALUATION[Any] = (e : EXPORT) => formatter(e.root[Any]())

  val formatter: Any => Any = (e) => e match {
    case (a,b) => (formatter(a),formatter(b))
    case (a,b,c) => (formatter(a),formatter(b),formatter(c))
    case (a,b,c,d) => (formatter(a),formatter(b),formatter(c),formatter(d))
    case l:Iterable[_] => l.map(formatter(_)).toString
    case i: java.lang.Number if (i.doubleValue()>100000) => "Inf"
    case i: java.lang.Number if (-i.doubleValue()>100000) => "-Inf"
    case i: java.lang.Double => f"${i.doubleValue()}%1.2f"
    case x => x.toString
  }

  val programClass = classOf[Main16_0]
  val nodes = 30
  val neighbourRange = 200
  val (width, height) = (1080, 720)
  ViewSetting.windowConfiguration = WindowConfiguration(width, height)
  ScafiProgramBuilder (
    Random(nodes, width, height),
    SimulationInfo(programClass,exportEvaluations = List(formatter_evaluation)),
    RadiusLikeSimulation(neighbourRange),
    ScafiWorldInformation(shape = Some(Circle(5,5))),
    neighbourRender = true,
  ).launch()
}

abstract class AggregateProgramSkeleton extends AggregateProgram with StandardSensors {
  def sense1 = sense[Boolean]("sens1")
  def sense2 = sense[Boolean]("sens2")
  def sense3 = sense[Boolean]("sens3")
  def boolToInt(b: Boolean) = mux(b){1}{0}
}

class Main extends AggregateProgramSkeleton {
  override def main() = 1
}

class Main1 extends AggregateProgramSkeleton {
  override def main() = 1
}

class Main2 extends AggregateProgramSkeleton {
  override def main() = 2+3
}

class Main3 extends AggregateProgramSkeleton {
  override def main() = (10,20)
}

class Main4 extends AggregateProgramSkeleton {
  override def main() = Math.random()
}

class Main5 extends AggregateProgramSkeleton {
  override def main() = sense1
}

class Main6 extends AggregateProgramSkeleton {
  override def main() = if (sense1) 10 else 20
}

class Main7 extends AggregateProgramSkeleton {
  override def main() = mid()
}

class Main8 extends AggregateProgramSkeleton {
  override def main() = minHoodPlus(nbrRange)
}

class Main8_0 extends AggregateProgramSkeleton {
  override def main() = (mid(), foldhoodPlus((Double.MaxValue, mid()))((t1, t2) => if(t1._1 > t2._1) t2 else t1)((nbrRange(), nbr(mid())))._2)
}

class Main9 extends AggregateProgramSkeleton {
  override def main() = rep(0){_+1}
}

class Main9_0 extends AggregateProgramSkeleton {
  override def main() = mux(sense1){
    (rep(0){ x => if(x < 1000) x + 1 else 1000})
  }(0)
}

class Main9_1 extends AggregateProgramSkeleton {
  override def main() = branch(sense1){
    (rep(0){ x => if(x < 1000) x + 1 else 1000})
  }(0)
}

class Main10 extends AggregateProgramSkeleton {
  override def main() = rep(Math.random()){x=>x}
}

class Main11 extends AggregateProgramSkeleton {
  override def main() = rep[Double](0.0){x => x + rep(Math.random()){y=>y}}
}

class Main12 extends AggregateProgramSkeleton {
  import Builtins.Bounded.of_i

  override def main() = maxHoodPlus(boolToInt(nbr{sense1}))
}

class Main12_0 extends AggregateProgramSkeleton {

  override def main(): Set[ID] = foldhood[Set[ID]](Set())(_++_)(nbr{Set(mid())})
}

class Main12_1 extends AggregateProgramSkeleton {

  override def main(): Set[ID] = foldhoodPlus(Set[ID]())(_++_)(nbr{Set(mid())})
}

class Main13 extends AggregateProgramSkeleton {
  override def main() = foldhoodPlus(0)(_+_){nbr{1}}
}

class Main14 extends AggregateProgramSkeleton {
  import Builtins.Bounded.of_i

  override def main() = rep(0){ x => boolToInt(sense1) max maxHoodPlus( nbr{x}) }
}

class Main14_0 extends AggregateProgramSkeleton {
  override def main() = rep(mid()){ x => x max maxHoodPlus( nbr{x} ) }
}

class Main15 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+1.0)} }
}

class Main16 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+nbrRange)} }
}

class Main16_0 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d} + branch(sense2)(nbrRange() * 5)(nbrRange))}}
}