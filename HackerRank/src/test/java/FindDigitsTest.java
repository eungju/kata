import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class FindDigitsTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("2\n" +
                "12\n" +
                "1012\n");
        FindDigits.main(stdin, new PrintWriter(stdout));
        assertEquals("2\n" +
                "3\n", stdout.toString());
    }
}
