import java.util.ArrayList;

public class FindBridges {

    private Graph G;
    private boolean[] visited;
    private int[] order;
    private int[] low;
    private int orderCount = 0;

    private ArrayList<Edge> result;

    private void dfs(int v, int parent) {
        visited[v] = true;
        order[v] = orderCount;
        low[v] = order[v];
        orderCount += 1;
        for (int w : G.adj(v)) {
            if (!visited[w]) {
                dfs(w, v);
                low[v] = Math.min(low[v], low[w]);
                if (low[w] > order[v]) result.add(new Edge(v, w));
            } else if (w != parent) low[v] = Math.min(low[v], low[w]);

        }

    }

    public ArrayList<Edge> result() {
        return result;
    }

    public FindBridges(Graph G) {
        this.G = G;
        visited = new boolean[G.V()]; // design 默认初始值为false?
        order = new int[G.V()];
        low = new int[G.V()];
        result = new ArrayList<>();

        for (int v = 0; v < G.V(); v++) {
            if (!visited[v]) dfs(v, v);
        }
    }

    public static void main(String[] args) {
        Graph g = new Graph("src/g.txt");
        FindBridges findBridges = new FindBridges(g);
        System.out.println(findBridges.result());
    }

}

