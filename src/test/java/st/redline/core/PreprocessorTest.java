package st.redline.core;

import org.antlr.runtime.*;
import org.junit.Test;
import st.redline.compiler.PreProcLexer;

import java.io.IOException;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

public class PreprocessorTest {

	private PreProcLexer lexer;

	@Test
	public void preprocess() throws IOException, RecognitionException {
		String input = "\n" +
				"import: 'example'\n" +
				"Object < #Test\n" +
				"\n" +
				"+ initialize\n" +
				"  \"class method\"\n" +
				"  ^ self.\n" +
				"- yourself\n" +
				"  \"instance method\"\n" +
				"  ^ self.\n" +
				"- at: key put: value\n" +
				"  \"keyword method\"\n" +
				"  ^ self.\n" +
				"- * num\n" +
				"  \"binary selector method\"\n" +
				"  ^ self.\n" +
				"";
		String expected = "\n" +
				"self import: 'example'\n" +
				"Object < #Test\n" +
				"\n" +
				"Test class atSelector: #initialize put: [\n" +
				"  \"class method\"\n" +
				"  ^ self.\n" +
				"]. Test atSelector: #yourself put: [\n" +
				"  \"instance method\"\n" +
				"  ^ self.\n" +
				"]. Test atSelector: #at:put: put: [ :key :value |\n" +
				"  \"keyword method\"\n" +
				"  ^ self.\n" +
				"]. Test atSelector: #* put: [ :num |\n" +
				"  \"binary selector method\"\n" +
				"  ^ self.\n" +
				"].\n" +
				"Test initialize.\n";
		String result = preprocess(input);
		assertEquals(expected, result);
	}

	private String preprocess(String source) {
		try {
			lexer = new PreProcLexer();
			lexer.setCharStream(new ANTLRStringStream(source));
			lexer.initPreProc("Test");
			new CommonTokenStream(lexer);
			while (lexer.nextToken().getType() != Token.EOF) {
			}
			return lexer.getOutput();
		} catch (Exception e) {
			System.err.println("Preprocessor threw an exception:\n\n");
			throw new RuntimeException(e);
		}
	}
}
