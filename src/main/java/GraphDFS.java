import java.util.ArrayList;

public class GraphDFS {

    private Graph G;
    private boolean[] visited;

    private ArrayList<Integer> pre = new ArrayList<Integer>();
    private ArrayList<Integer> post = new ArrayList<Integer>();

    private void dfs(int v) {
        visited[v] = true;
        pre.add(v);
        for (int w : G.adj(v)) if (!visited[w]) dfs(w);
        post.add(v);
    }

    public GraphDFS(Graph G) {
        this.G = G;
        visited = new boolean[G.V()]; // design 默认初始值为false?
        for (int v = 0; v < G.V(); v++) {
            if (!visited[v]) dfs(v);

        }
    }

    public Iterable<Integer> pre() {
        return pre;
    }

    public Iterable<Integer> post() {
        return post;
    }

    public static void main(String[] args) {
        Graph g = new Graph("src/g.txt");
        GraphDFS graphDFS = new GraphDFS(g);
        System.out.println(graphDFS.pre());
        System.out.println(graphDFS.post());
    }

}

