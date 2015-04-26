import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Scanner;

public class CavityMap {
    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int n = Integer.parseInt(scanner.nextLine());
        int[][] map = new int[n][];
        for (int r = 0; r < n; r++) {
            map[r] = new int[n];
            String row = scanner.nextLine();
            for (int c = 0; c < n; c++) {
                map[r][c] = row.charAt(c) - '0';
            }
        }
        for (int r = 0; r < n; r++) {
            for (int c = 0; c < n; c++) {
                int depth = map[r][c];
                boolean onBorder = r == 0 || r == n - 1 || c == 0 || c == n - 1;
                if (!onBorder && depth > map[r - 1][c] && depth > map[r + 1][c] && depth > map[r][c - 1] && depth > map[r][c + 1]) {
                    stdout.print('X');
                } else {
                    stdout.print(depth);
                }
            }
            stdout.println();
        }
        stdout.flush();
    }

    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
