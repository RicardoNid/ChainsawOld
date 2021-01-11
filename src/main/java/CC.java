import java.util.ArrayList;

public class CC {

    private Graph G;
    private int[] visited;
    private int ccCount;

    private void dfs(int v) {
        visited[v] = ccCount - 1;
        for (int w : G.adj(v))
            if (visited[w] == -1) dfs(w);
    }


    public int ccCount() {
        return ccCount;
    }

    public CC(Graph G) {
        this.G = G;
        visited = new int[G.V()]; // design 默认初始值为false?
        for (int i = 0; i < G.V(); i++) {
            visited[i] = -1;
        }
        for (int v = 0; v < G.V(); v++) {
            if (visited[v] == -1) {
                ccCount++;
                dfs(v);
            }

        }
    }

    public boolean isConnected(int v, int w) {
        G.validateVertex(v);
        G.validateVertex(w);
        return visited[v] == visited[w];
    }

    public ArrayList<Integer>[] components() {
        ArrayList<Integer>[] res = new ArrayList[ccCount];
        for (int i = 0; i < ccCount; i++) {
            res[i] = new ArrayList<>();
        }
        for (int v = 0; v < G.V(); v++) {
            res[visited[v]].add(v);
        }
        return res;
    }

    public static void main(String[] args) {
        Graph g = new Graph("src/g.txt");
        CC cc = new CC(g);
        System.out.println(cc.ccCount());

        System.out.println(cc.isConnected(0, 6));
        System.out.println(cc.isConnected(0, 5));

        ArrayList<Integer>[] comp = cc.components();
        for (int ccid = 0; ccid < comp.length; ccid++) {
            System.out.print(ccid + " : ");
            for (int w : comp[ccid]) {
                System.out.print(w + " ");
            }
            System.out.println();
        }
    }
}

