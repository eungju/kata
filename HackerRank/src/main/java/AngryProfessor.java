import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Scanner;

public class AngryProfessor {
    public boolean solve(int n, int k, int[] arrivals) {
        int attendants = 0;
        for (int dt: arrivals) {
            if (dt <= 0) {
                attendants++;
            }
        }
        return attendants < k;
    }

    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int tests = scanner.nextInt();
        AngryProfessor algorithm = new AngryProfessor();
        for (int t = 0; t < tests; t++) {
            int n = scanner.nextInt();
            int k = scanner.nextInt();
            int[] arrivals = new int[n];
            for (int i = 0; i < n; i++) {
                arrivals[i] = scanner.nextInt();
            }
            stdout.println(algorithm.solve(n, k, arrivals) ? "YES" : "NO");
        }
        stdout.flush();
    }

    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
