import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class ChocolateFeastTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("3\n" +
                "10 2 5\n" +
                "12 4 4\n" +
                "6 2 2\n");
        ChocolateFeast.main(stdin, new PrintWriter(stdout));
        assertEquals("6\n" +
                "3\n" +
                "5\n", stdout.toString());
    }
}
