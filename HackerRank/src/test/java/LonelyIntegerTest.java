import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class LonelyIntegerTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("5\n" +
                "0 0 1 2 1\n\n");
        LonelyInteger.main(stdin, new PrintWriter(stdout));
        assertEquals("2\n", stdout.toString());
    }
}
