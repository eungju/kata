import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class TheLoveLetterMysteryTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("4\n" +
                "abc\n" +
                "abcba\n" +
                "abcd\n" +
                "cba\n");
        TheLoveLetterMystery.main(stdin, new PrintWriter(stdout));
        assertEquals("2\n" +
                "0\n" +
                "4\n" +
                "2\n", stdout.toString());
    }
}
