import java.util.HashSet;

// design 不在显式建图,而是在grid上操作
// design FloodFill是一类将二维网格视为图然后遍历的算法
// 关于leetcode上的性能 https://coding.imooc.com/lesson/370.html#mid=27775
class FloodFill {

    private int[][] grid;
    private int R, C;

    private HashSet<Integer>[] G;
    private boolean[][] visited;

    private int[][] connectivity = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    private boolean inArea(int r, int c) {
        return (r >= 0 && r < R && c >= 0 && c < C);
    }

    public int maxAreaOfIsland(int[][] grid) {
        if (grid == null) return 0;

        this.grid = grid;
        R = grid.length;
        C = grid[0].length;
        visited = new boolean[R][C];

        int res = 0;

        for (int r = 0; r < R; r++) {
            for (int c = 0; c < C; c++) {
                if (!visited[r][c] && grid[r][c] == 1) res = Math.max(res, dfs(r, c));
            }
        }

        return res;
    }

    private int dfs(int r, int c) {
        visited[r][c] = true;
        int res = 1;
        for (int[] dir : connectivity) {
            int rNext = r + dir[0];
            int cNext = c + dir[1];
            if (inArea(rNext, cNext)) if (!visited[rNext][cNext] && grid[rNext][cNext] == 1) res += dfs(rNext, cNext);
        }
        return res;
    }
}