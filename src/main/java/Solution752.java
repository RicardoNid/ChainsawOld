import java.util.*;

// https://leetcode-cn.com/problems/open-the-lock/submissions/
// 转盘锁
// 图论-无向无权图的最短路径
class Solution752 {
    public int openLock(String[] deadends, String target) {

        // design 为了提高查找速度,使用HashSet
        // design Java的原始array([])没有集合方法,要先转换成集合类型
        HashSet<String> deadset = new HashSet<>();
        for (String s : deadends) deadset.add(s);

        int[][] connectivity = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1},
                {1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}};

        // 处理特殊状况
        if (deadset.contains(target)) return -1;
        if (deadset.contains("0000")) return -1;
        if (target.equals("0000")) return 0;

        Queue<String> queue = new LinkedList<>();
        // design 因为关心的是存在性,实际上visited和dis的信息都可以通过一个HashMap来包含
        HashMap<String, Integer> dis = new HashMap<>();
        queue.add("0000");
        dis.put("0000", 0);
        while (!queue.isEmpty()) {
            String current = queue.remove();
            char[] currentChars = current.toCharArray();

            ArrayList<String> nexts = new ArrayList<>();
            for (int i = 0; i < 4; i++) {
                char o = currentChars[i];
                currentChars[i] = Character.forDigit((currentChars[i] - '0' + 1) % 10, 10);
                nexts.add(new String(currentChars));
                currentChars[i] = o;
            }
            for (int i = 0; i < 4; i++) {
                char o = currentChars[i];
                currentChars[i] = Character.forDigit((currentChars[i] - '0' + 9) % 10, 10);
                nexts.add(new String(currentChars));
                currentChars[i] = o;
            }

            for (String next : nexts) {
                if (!deadset.contains(next) && !dis.containsKey(next)) {
                    queue.add(next);
                    dis.put(next, dis.get(current) + 1);
                    if (next.equals(target)) return dis.get(next);
                }
            }
        }
        return -1;
    }
}