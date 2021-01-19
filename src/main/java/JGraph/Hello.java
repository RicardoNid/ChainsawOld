package JGraph;

import com.sun.jna.platform.win32.WinUser;
import org.jgrapht.Graph;
import org.jgrapht.Graphs;
import org.jgrapht.generate.*;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleGraph;
import org.jgrapht.graph.builder.GraphBuilder;
import org.jgrapht.traverse.BreadthFirstIterator;
import org.jgrapht.traverse.DepthFirstIterator;
import org.jgrapht.traverse.RandomWalkIterator;
import org.jgrapht.util.SupplierUtil;

import java.net.URI;
import java.util.Iterator;
import java.util.function.Supplier;

public class Hello {
    public static void main(String[] args) {

        // design 指定图的顶点和边类型
        Graph<Integer, DefaultEdge> g = new DefaultDirectedGraph<>(DefaultEdge.class);

        Graph<Integer, DefaultEdge> G = new GraphBuilder<>(g).addEdgeChain(1, 2, 3, 2, 1).addEdge(2, 4).addEdge(3, 5).build();

        Iterator<Integer> dfs = new DepthFirstIterator<Integer, DefaultEdge>(G);
        while (dfs.hasNext()) System.out.println(dfs.next());
        Graphs.addEdgeWithVertices(G, 7, 8);

        Supplier<String> vSupplier = new Supplier<String>() {

            private int id = 0;

            @Override
            public String get() {
                return "v" + id++;
            }
        };

        Graph<String, DefaultEdge> completeGraph = new SimpleGraph<>(vSupplier, SupplierUtil.createDefaultEdgeSupplier(), false);

        CompleteGraphGenerator<String, DefaultEdge> completeGraphGenerator = new CompleteGraphGenerator<>(10);
//        completeGraphGenerator.generateGraph(completeGraph);

        GridGraphGenerator<String, DefaultEdge> gridGraphGenerator = new GridGraphGenerator<>(5, 5);
        gridGraphGenerator.generateGraph(completeGraph);

//        Iterator<String> iter1 = new DepthFirstIterator<>(completeGraph);
//        while (iter1.hasNext()) System.out.println(completeGraph.edgesOf(iter1.next()).toString());
//        Iterator<String> iter2 = new BreadthFirstIterator<>(completeGraph);
//        while (iter2.hasNext()) System.out.println(completeGraph.edgesOf(iter2.next()).toString());
        Iterator<String> iter3 = new RandomWalkIterator<>(completeGraph);
        while (iter3.hasNext()) System.out.println(iter3.next());

    }
}
