import java.util.LinkedList;
import java.util.Queue;

class Solution1091 {

    private int[][] grid;
    private int R, C;

    private boolean[][] visited;
    private Queue<Integer> queue = new LinkedList<>();
    private int[] dis;

    private int[][] connectivity = {{-1, 0}, {1, 0}, {0, -1}, {0, 1},
            {-1, -1}, {-1, 1}, {1, -1}, {1, 1}};

    private boolean inArea(int r, int c) {
        return (r >= 0 && r < R && c >= 0 && c < C);
    }

    public int shortestPathBinaryMatrix(int[][] grid) {
        this.grid = grid;
        R = grid.length;
        C = grid[0].length;

        if ((grid[0][0] == 1)) return -1;
        if (R == 1 && C == 1) {
            if (grid[0][0] == 0) return 1;
            if (grid[0][0] == 1) return -1;
        }

        visited = new boolean[R][C];
        dis = new int[R * C];
        for (int v = 0; v < R * C; v++) {
            dis[v] = -1;
        }

        int destination = R * C - 1;
        if (bfs(0, 0)) return dis[destination] + 1;
        else return -1;
    }

    public boolean bfs(int r, int c) {
        Queue<Integer> queue = new LinkedList<>(); // design Queue是interface,要为它挑选具体实现
        int source = r * C + c;

        queue.add(source);
        visited[r][c] = true;
        dis[source] = 0;

        while (!queue.isEmpty()) {
            int current = queue.remove();

            int rCurrent = current / C;
            int cCurrent = current % C;

            for (int[] dir : connectivity) {
                int rNext = rCurrent + dir[0];
                int cNext = cCurrent + dir[1];
                if (inArea(rNext, cNext))
                    if (!visited[rNext][cNext] && grid[rNext][cNext] == 0) {
                        int next = rNext * C + cNext;
                        queue.add(next);
                        visited[rNext][cNext] = true;
                        dis[next] = dis[current] + 1;
                        if (rNext == R - 1 && cNext == C - 1) return true;
                    }
            }
        }
        return false;
    }
}