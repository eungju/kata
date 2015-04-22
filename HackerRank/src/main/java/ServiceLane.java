import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Scanner;

public class ServiceLane {
    private final int[] widths;

    public ServiceLane(int[] widths) {
        this.widths = widths;
    }

    public int minWidth(int from, int to) {
        int minWidth = Integer.MAX_VALUE;
        for (int k = from; k <= to; k++) {
            minWidth = Math.min(minWidth, widths[k]);
        }
        return minWidth;
    }

    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int length = scanner.nextInt();
        int tests = scanner.nextInt();
        int[] widths = new int[length];
        for (int k = 0; k < widths.length; k++) {
            widths[k] = scanner.nextInt();
        }
        ServiceLane serviceLane = new ServiceLane(widths);
        for (int t = 0; t < tests; t++) {
            int i = scanner.nextInt();
            int j = scanner.nextInt();
            int allowedVehicle = Math.min(serviceLane.minWidth(i, j), 3);
            stdout.println(allowedVehicle);
        }
        stdout.flush();
    }
    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
