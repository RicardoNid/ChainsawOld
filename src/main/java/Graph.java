import java.io.File;
import java.io.IOException;
import java.util.Scanner;
import java.util.TreeSet;

public class Graph {
    private int V;
    private int E;
    private TreeSet<Integer>[] adj;

    public int V() {
        return V;
    }

    public int E() {
        return E;
    }

    public boolean hasEdge(int v, int w) {
        validateVertex(v);
        validateVertex(w);
        return adj[v].contains(w);
    }

    // design 这里返回iterable而非具体集合类型,实现了对屏蔽了实现细节,从具有具体数据结构的集合提升到了"集合"
    public Iterable<Integer> adj(int v) {

        validateVertex(v); // design 对用户传入的数据,总是进行合法性检查
        return adj[v];
    }

    public int degree(int v) {
        validateVertex(v);
        return adj[v].size();
    }

    public Graph(String filename) {
        File file = new File(filename);
        try (Scanner scanner = new Scanner(file)) {
            V = scanner.nextInt();
            if (V < 0) throw new IllegalArgumentException("V must be non-negative");
            adj = new TreeSet[V]; // design 首先申请整个数组的内存空间
            for (int i = 0; i < V; i++) {
                adj[i] = new TreeSet<Integer>(); // design 然后申请每个对象的内存空间
            }
            E = scanner.nextInt();
            if (E < 0) throw new IllegalArgumentException("E must be non-negative");
            for (int i = 0; i < E; i++) {
                int a = scanner.nextInt();
                validateVertex(a);
                int b = scanner.nextInt();
                validateVertex(b);
                if (a == b) throw new IllegalArgumentException("Self loop is forbidden!");
                if (adj[a].contains(b)) throw new IllegalArgumentException("Parallel edges are forbidden!");
                adj[a].add(b);
                adj[b].add(a);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void validateVertex(int v) {
        if (v < 0 || v >= V) throw new IllegalArgumentException("vertex " + v + "is invalid");
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append(String.format("V = %d, E = %d\n", V, E));
        for (int v = 0; v < V; v++) {
            sb.append(String.format("%d: ", v));
            for (int w : adj[v]) {
                sb.append(String.format("%d ", w));
            }
            sb.append('\n');
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        Graph graph = new Graph("src/g.txt");
        System.out.print(graph);
    }
}
