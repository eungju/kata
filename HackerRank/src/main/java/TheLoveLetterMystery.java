import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Scanner;

public class TheLoveLetterMystery {
    public int solve(String word) {
        int operations = 0;
        for (int i = 0; i < word.length() / 2; i++) {
            operations += Math.abs(word.charAt(i) - word.charAt(word.length() - i - 1));
        }
        return operations;
    }

    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int tests = Integer.parseInt(scanner.nextLine());
        TheLoveLetterMystery algorithm = new TheLoveLetterMystery();
        for (int t = 0; t < tests; t++) {
            String word = scanner.nextLine();
            stdout.println(algorithm.solve(word));
        }
        stdout.flush();
    }

    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
