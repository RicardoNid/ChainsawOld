//package Chainsaw.Architectures
//
//import com.mxgraph.layout._
//import com.mxgraph.swing._
//import org.jgrapht._
//import org.jgrapht.ext._
//import org.jgrapht.graph._
//import javax.swing._
//import java.awt._
//import org.jgrapht.alg.flow.PadbergRaoOddMinimumCutset
//import scala.collection.JavaConversions._
//
//// TODO: write a scala version of this as my utility
//
///**
// * A demo applet that shows how to use JGraphX to visualize JGraphT graphs. Applet based on
// * JGraphAdapterDemo.
// */
//@SerialVersionUID(2202072534703043194L)
//object Visualization {
//  private val DEFAULT_SIZE: Dimension = new Dimension(530, 320)
//  /**
//   * An alternative starting point for this demo, to also allow running this applet as an
//   * application.
//   *
//   * @param args command line arguments
//   */
//  def main(args: Array[String]): Unit = {
//    val applet: Visualization = new Visualization
//    applet.init()
//    val frame: JFrame = new JFrame
//    frame.getContentPane.add(applet)
//    frame.setTitle("JGraphT Adapter to JGraphX Demo")
//    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
//    frame.pack()
//    frame.setVisible(true)
//  }
//}
//
//@SerialVersionUID(2202072534703043194L)
//class Visualization extends JApplet {
//  private var jgxAdapter: JGraphXAdapter[Int, DefaultEdge] = null
//  override def init(): Unit = { // create a JGraphT graph
//    val g = new DefaultListenableGraph(new BinarySFG())
//    // create a visualization using JGraph, via an adapter
//    jgxAdapter = new JGraphXAdapter(g)
//    setPreferredSize(Visualization.DEFAULT_SIZE)
//    val component: mxGraphComponent = new mxGraphComponent(jgxAdapter)
//    component.setConnectable(false)
//    component.getGraph.setAllowDanglingEdges(false)
//    getContentPane.add(component)
//    resize(Visualization.DEFAULT_SIZE)
//    val v1 = 1
//    val v2 = 2
//    val v3 = 3
//    val v4 = 4
//    // add some sample data (graph manipulated via JGraphX)
//    g.addVertex(v1)
//    g.addVertex(v2)
//    g.addVertex(v3)
//    g.addVertex(v4)
//    g.addEdge(v1, v2)
//    g.addEdge(v1, v3)
//    g.addEdge(v2, v4)
//    g.addEdge(v3, v4)
//
//    val layout = new mxCompactTreeLayout(jgxAdapter)
//    layout.execute(jgxAdapter.getDefaultParent)
//
//    val cutset = new PadbergRaoOddMinimumCutset(g)
//    println(cutset.getCutEdges.mkString(" "))
//
//  }
//}