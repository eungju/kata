import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Scanner;

public class LonelyInteger {
    public int solve(int[] numbers) {
        int[] lonely = new int[100];
        for (int i = 0; i < numbers.length; i++) {
            lonely[numbers[i]]++;
        }
        for (int i = 0; i < lonely.length; i++) {
            if (lonely[i] % 2 == 1) {
                return i;
            }
        }
        return -1;
    }

    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int n = scanner.nextInt();
        int[] numbers = new int[n];
        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = scanner.nextInt();
        }
        LonelyInteger algorithm = new LonelyInteger();
        stdout.println(algorithm.solve(numbers));
        stdout.flush();
    }

    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
