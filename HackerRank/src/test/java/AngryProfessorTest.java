import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class AngryProfessorTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("2\n" +
                "4 3\n" +
                "-1 -3 4 2\n" +
                "4 2\n" +
                "0 -1 2 1\n");
        AngryProfessor.main(stdin, new PrintWriter(stdout));
        assertEquals("YES\n" +
                "NO\n", stdout.toString());
    }
}
