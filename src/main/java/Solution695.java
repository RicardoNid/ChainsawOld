import java.util.HashSet;

class Solution695 {

    private int[][] grid;
    private int R, C;

    private HashSet<Integer>[] G;
    private boolean[] visited;

    private boolean inArea(int r, int c) {
        return (r >= 0 && r < R && c >= 0 && c < C);
    }

    private void constructGraph() { // 从网格得到邻接表
        G = new HashSet[R * C];
        for (int i = 0; i < G.length; i++) {
            G[i] = new HashSet<>();
        }

        int[][] dirs = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // design 四连通位移矩阵

        for (int v = 0; v < G.length; v++) {
            int row = v / C;
            int col = v % C;
            if (grid[row][col] == 1) {
                for (int[] dir : dirs) {
                    int nextRow = row + dir[0];
                    int nextCol = col + dir[1];
                    if (inArea(nextRow, nextCol)) {
                        int next = nextRow * C + nextCol;
                        if (grid[nextRow][nextCol] == 1) {
                            G[v].add(next);
                            G[next].add(v);
                        }
                    }
                }
            }

        }
    }

    public int maxAreaOfIsland(int[][] grid) {
        if (grid == null) return 0;

        this.grid = grid;
        R = grid.length;
        C = grid[0].length;
        visited = new boolean[R * C];

        constructGraph();
        for (int i = 0; i < G.length; i++) {
            for (int elem : G[i]) {
                System.out.print(elem + " ");
            }
            System.out.println();
        }

        int res = 0;

        for (int v = 0; v < G.length; v++) {
            int row = v / C;
            int col = v % C;
            if (!visited[v] && grid[row][col] == 1)
                res = Math.max(res, dfs(v));
        }

        return res;
    }

    private int dfs(int v) {
        visited[v] = true;
        int res = 1;
        for (int w : G[v]) {
            if (!visited[w]) {
                res += dfs(w);
            }
        }
        return res;
    }
}