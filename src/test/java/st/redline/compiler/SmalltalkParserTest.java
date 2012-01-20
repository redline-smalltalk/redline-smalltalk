/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

public class SmalltalkParserTest {

	private SmalltalkLexer lexer;

	@Test
	public void parseProgram() throws IOException, RecognitionException {
		String input = "| a |\n" +
				"'Hello'\n";
		Program program = createParser(input).program();
		assertFalse(program.temporaries().isEmpty());
		assertEquals(1, program.temporaries().size());
		assertEquals("a", program.temporaries().get(0).value());
		assertNotNull(program.statements());
		assertNoLexerExceptions();
	}

	@Test
	public void parseCascadedSimpleExpression() throws IOException, RecognitionException {
		String input = "self stream;\n" +
			" write: #sym;\n" +
			" write: $x;\n" +
			" reverse\n";
		SimpleExpression simpleExpression = createParser(input).simple_expression();
		assertNoLexerExceptions();
		assertEquals(1, simpleExpression.line());
		assertEquals("self", ((Identifier) simpleExpression.primary()).value());
		UnaryExpression unaryExpression = (UnaryExpression) simpleExpression.messageExpression();
		assertEquals("stream", unaryExpression.unarySelectors().get(0).value());
		assertEquals(1, unaryExpression.unarySelectors().size());
		assertEquals(3, simpleExpression.messageElements().size());
		KeywordMessageElement keywordMessageElement = (KeywordMessageElement) simpleExpression.messageElements().get(0);
		assertEquals("write:", keywordMessageElement.keywords());
		assertEquals(1, keywordMessageElement.binaryObjectDescriptions().size());
		assertEquals("sym", ((SymbolConstant) keywordMessageElement.binaryObjectDescriptions().get(0).primary()).value());
		assertTrue(keywordMessageElement.binaryObjectDescriptions().get(0).unarySelectors().isEmpty());
		assertTrue(keywordMessageElement.binaryObjectDescriptions().get(0).binarySelectorUnaryObjectDescriptions().isEmpty());
		keywordMessageElement = (KeywordMessageElement) simpleExpression.messageElements().get(1);
		assertEquals("write:", keywordMessageElement.keywords());
		assertEquals(1, keywordMessageElement.binaryObjectDescriptions().size());
		assertEquals("x", ((CharacterConstant) keywordMessageElement.binaryObjectDescriptions().get(0).primary()).value());
		assertTrue(keywordMessageElement.binaryObjectDescriptions().get(0).unarySelectors().isEmpty());
		assertTrue(keywordMessageElement.binaryObjectDescriptions().get(0).binarySelectorUnaryObjectDescriptions().isEmpty());
		UnarySelectorMessageElement unarySelectorMessageElement = (UnarySelectorMessageElement) simpleExpression.messageElements().get(2);
		assertEquals("reverse", unarySelectorMessageElement.unarySelector().value());
	}

	@Test
	public void parseUnaryMessageExpressionWithKeywordAndBinaryExpressionStatements() throws IOException, RecognitionException {
		String input = "self first at: 2 put: $c + $d";
		Statements statements = createParser(input).statements();
		BinaryObjectDescription binaryObjectDescription = sharedUnaryMessageExpressionAssertions(statements);
		assertTrue(binaryObjectDescription.unarySelectors().isEmpty());
		assertEquals(1, binaryObjectDescription.binarySelectorUnaryObjectDescriptions().size());
		BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription = binaryObjectDescription.binarySelectorUnaryObjectDescriptions().get(0);
		assertEquals("+", binarySelectorUnaryObjectDescription.binarySelector().value());
		assertEquals("d", ((CharacterConstant) binarySelectorUnaryObjectDescription.unaryObjectDescription().primary()).value());
		assertTrue(binarySelectorUnaryObjectDescription.unaryObjectDescription().unarySelectors().isEmpty());
		assertNoLexerExceptions();
	}

	@Test
	public void parseUnaryMessageExpressionWithKeywordStatements() throws IOException, RecognitionException {
		String input = "self first at: 2 put: $c";
		Statements statements = createParser(input).statements();
		BinaryObjectDescription binaryObjectDescription = sharedUnaryMessageExpressionAssertions(statements);
		assertTrue(binaryObjectDescription.unarySelectors().isEmpty());
		assertTrue(binaryObjectDescription.binarySelectorUnaryObjectDescriptions().isEmpty());
	}

	private BinaryObjectDescription sharedUnaryMessageExpressionAssertions(Statements statements) {
		assertEquals(1, statements.line());
		assertNotNull(statements.expression());
		SimpleExpression simpleExpression = (SimpleExpression) statements.expression();
		assertEquals("self", ((Identifier) simpleExpression.primary()).value());
		UnaryExpression unaryExpression = (UnaryExpression) simpleExpression.messageExpression();
		assertNotNull(unaryExpression);
		assertEquals("first", unaryExpression.unarySelectors().get(0).value());
		assertEquals(1, unaryExpression.unarySelectors().size());
		KeywordExpression keywordExpression = (KeywordExpression) unaryExpression.messageExpression();
		assertNotNull(keywordExpression);
		assertEquals("at:put:", keywordExpression.keywords());
		assertEquals(2, keywordExpression.binaryObjectDescriptions().size());
		BinaryObjectDescription binaryObjectDescription = keywordExpression.binaryObjectDescriptions().get(0);
		assertEquals("2", ((Number) binaryObjectDescription.primary()).value());
		assertTrue(binaryObjectDescription.unarySelectors().isEmpty());
		assertTrue(binaryObjectDescription.binarySelectorUnaryObjectDescriptions().isEmpty());
		binaryObjectDescription = keywordExpression.binaryObjectDescriptions().get(1);
		assertEquals("c", ((CharacterConstant) binaryObjectDescription.primary()).value());
		assertNoLexerExceptions();
		return binaryObjectDescription;
	}

	@Test
	public void parseAnsweredUnaryMessageExpressionStatements() throws IOException, RecognitionException {
		String input = "^ self name uppercase.";
		AnswerStatement answerStatement = (AnswerStatement) createParser(input).statements();
		assertEquals(1, answerStatement.line());
		SimpleExpression simpleExpression = (SimpleExpression) answerStatement.expression();
		assertEquals("self", ((Identifier) simpleExpression.primary()).value());
		UnaryExpression unaryExpression = (UnaryExpression) simpleExpression.messageExpression();
		assertEquals("name", unaryExpression.unarySelectors().get(0).value());
		assertEquals("uppercase", unaryExpression.unarySelectors().get(1).value());
		assertNull(unaryExpression.messageExpression());
		assertNoLexerExceptions();
	}

	@Test
	public void parseSimpleMessageExpression() throws IOException, RecognitionException {
		String input = "12.34 round floor.";
		SimpleExpression simpleExpression = createParser(input).simple_expression();
		assertEquals("12.34", ((Number) simpleExpression.primary()).value());
		assertEquals(2, ((UnaryExpression) simpleExpression.messageExpression()).unarySelectors().size());
		assertNull(((UnaryExpression) simpleExpression.messageExpression()).messageExpression());
		assertEquals("round", ((UnaryExpression) simpleExpression.messageExpression()).unarySelectors().get(0).value());
		assertEquals("floor", ((UnaryExpression) simpleExpression.messageExpression()).unarySelectors().get(1).value());
		assertNoLexerExceptions();
	}

	@Test
	public void parseAnswerNumberWithDecimalStatement() throws IOException, RecognitionException {
		String input = "^12.23.";
		AnswerStatement answerStatement = (AnswerStatement) createParser(input).statements();
		assertEquals("12.23", ((Number) ((SimpleExpression) answerStatement.expression()).primary()).value());
		assertNoLexerExceptions();
	}

	@Test
	public void parseAnswerNumberWithoutDecimalStatement() throws IOException, RecognitionException {
		String input = "^-12.";
		AnswerStatement answerStatement = (AnswerStatement) createParser(input).statements();
		assertEquals("-12", ((Number) ((SimpleExpression) answerStatement.expression()).primary()).value());
		assertNoLexerExceptions();
	}

	@Test
	public void parseAssignmentOfPrimaryAsExpression() throws IOException, RecognitionException {
		String input = "var := 5";
		AssignmentExpression assignmentExpression = (AssignmentExpression) createParser(input).expression();
		assertEquals("var", assignmentExpression.variableName());
		assertEquals(1, assignmentExpression.line());
		assertTrue(assignmentExpression.expression() instanceof SimpleExpression);
		assertEquals("5", ((Number) ((SimpleExpression) assignmentExpression.expression()).primary()).value());
		assertNoLexerExceptions();
	}

	@Test
	public void parseMultipleAssignmentOfPrimaryAsExpression() throws IOException, RecognitionException {
		String input = "var1 := var2 := 'str'";
		AssignmentExpression assignmentExpression = (AssignmentExpression) createParser(input).expression();
		assertEquals("var1", assignmentExpression.variableName());
		assertEquals(1, assignmentExpression.line());
		assertTrue(assignmentExpression.expression() instanceof AssignmentExpression);
		assertEquals("var2", ((AssignmentExpression) assignmentExpression.expression()).variableName());
		SimpleExpression rightHandSide = (SimpleExpression) ((AssignmentExpression) assignmentExpression.expression()).expression();
		assertEquals("str", ((StringConstant) rightHandSide.primary()).value());
		assertNoLexerExceptions();
	}

	@Test
	public void parseEmptyBlockAsPrimary() throws IOException, RecognitionException {
		String input = "[]";
		Block block = (Block) createParser(input).primary();
		assertEquals(1, block.line());
		assertNoLexerExceptions();
	}

	@Test
	public void parseSimpleAnswerNumberBlockAsPrimary() throws IOException, RecognitionException {
		String input = "[^ 32]";
		Block block = (Block) createParser(input).primary();
		assertEquals(1, block.line());
		assertNotNull(block.statements());
		AnswerStatement answerStatement = (AnswerStatement) block.statements();
		assertEquals("32", ((Number) ((SimpleExpression) answerStatement.expression()).primary()).value());
		assertNoLexerExceptions();
	}

	@Test
	public void parseBlockWithBlockArgumentsAsPrimary() throws IOException, RecognitionException {
		String input = "[:foo :bar|]";
		Block block = (Block) createParser(input).primary();
		assertEquals(1, block.line());
		assertEquals(2, block.arguments().size());
		assertEquals("foo", block.arguments().get(0).value());
		assertTrue(block.arguments().get(0) instanceof BlockArgument);
		assertEquals("bar", block.arguments().get(1).value());
		assertTrue(block.arguments().get(1) instanceof BlockArgument);
		assertNoLexerExceptions();
	}

	@Test
	public void parseBlockWithTemporariesAsPrimary() throws IOException, RecognitionException {
		String input = "[ | var1 var2 |]";
		Block block = (Block) createParser(input).primary();
		assertEquals(1, block.line());
		assertEquals(2, block.temporaries().size());
		assertEquals("var1", block.temporaries().get(0).value());
		assertTrue(block.temporaries().get(0) instanceof Identifier);
		assertEquals("var2", block.temporaries().get(1).value());
		assertTrue(block.temporaries().get(1) instanceof Identifier);
		assertNoLexerExceptions();
	}

	@Test
	public void parseBlockWithBlockArgumentsAndTemporariesAsPrimary() throws IOException, RecognitionException {
		String input = "[:foo :bar || var1 var2 |]";
		Block block = (Block) createParser(input).primary();
		assertEquals(1, block.line());
		assertEquals(2, block.arguments().size());
		assertEquals(2, block.temporaries().size());
		assertEquals("foo", block.arguments().get(0).value());
		assertTrue(block.arguments().get(0) instanceof BlockArgument);
		assertEquals("bar", block.arguments().get(1).value());
		assertTrue(block.arguments().get(1) instanceof BlockArgument);
		assertEquals("var1", block.temporaries().get(0).value());
		assertTrue(block.temporaries().get(0) instanceof Identifier);
		assertEquals("var2", block.temporaries().get(1).value());
		assertTrue(block.temporaries().get(1) instanceof Identifier);
		assertNoLexerExceptions();
	}

	@Test
	public void parseIdentifierAsPrimary() throws IOException, RecognitionException {
		// Identifier is any letter followed by a 0 or more letters or digits.
		// Letters are A..Z or a..Z
		String input = "a1";
		Identifier identifier = (Identifier) createParser(input).primary();
		assertEquals(input, identifier.value());
		assertEquals(1, identifier.line());
		assertNoLexerExceptions();
	}

	@Test
	public void parseNumbersAsPrimary() throws IOException, RecognitionException {
		String[] numbers = {"32", "-16", "3.142", "8e-2", "6r-3.21e-9"};
		// "4r1" "2r-3e4" "9e4"  <-- numbers we ARE having trouble parsing.
		for (String input : numbers) {
			Number number = (Number) createParser(input).primary();
			assertEquals(input, number.value());
			assertEquals(1, number.line());
			assertNoLexerExceptions();
		}
	}

	@Test
	public void parseSymbolConstantsAsPrimary() throws IOException, RecognitionException {
		String[] symbols = {"#ab", "#+", "#-", "#--", "#at:", "#at:put:"};
		for (String input : symbols) {
			SymbolConstant symbolConstant = (SymbolConstant) createParser(input).primary();
			// Note: # is stripped.
			assertEquals(input.substring(1), symbolConstant.value());
			assertEquals(1, symbolConstant.line());
			assertNoLexerExceptions();
		}
	}

	@Test
	public void parseCharacterConstantsAsPrimary() throws IOException, RecognitionException {
		String[] characters = {"$a", "$'", "$\"", "$5", "$B", "$;", "$$", "$~", "$>"};
		for (String input : characters) {
			CharacterConstant characterConstant = (CharacterConstant) createParser(input).primary();
			// Note: $ is stripped.
			assertEquals(input.substring(1), characterConstant.value());
			assertEquals(1, characterConstant.line());
			assertNoLexerExceptions();
		}
	}

	@Test
	public void parseStringAsPrimary() throws IOException, RecognitionException {
		String[] strings = {"'str'", "''", "'\"'", "'cat on mat'"};
		for (String input : strings) {
			StringConstant stringConstant = (StringConstant) createParser(input).primary();
			// Note: leading ' and trailing ' are stripped.
			assertEquals(input.substring(1, input.length()-1), stringConstant.value());
			assertEquals(1, stringConstant.line());
			assertNoLexerExceptions();
		}
	}

	@Test
	public void parseStringWithNestedSingleQuoteAsPrimary() throws IOException, RecognitionException {
		StringConstant stringConstant = (StringConstant) createParser("'I don''t fail'").primary();
		assertEquals("I don't fail", stringConstant.value());
		assertEquals(1, stringConstant.line());
		assertNoLexerExceptions();
	}

	@Test
	public void parseArrayConstantAsPrimary() throws IOException, RecognitionException {
		String input = "#(12.3 Abc + at:put: 'str' $c () ('nested'))";
		ArrayConstant arrayConstant = (ArrayConstant) createParser(input).primary();
		assertEquals(1, arrayConstant.line());
		Array array = arrayConstant.array();
		assertNotNull(array);
		assertEquals(8, array.size());
		assertEquals("12.3", array.get(0).value());
		assertTrue(array.get(0) instanceof Number);
		assertEquals("Abc", array.get(1).value());
		assertTrue(array.get(1) instanceof Symbol);
		assertEquals("+", array.get(2).value());
		assertTrue(array.get(2) instanceof Symbol);
		assertEquals("at:put:", array.get(3).value());
		assertTrue(array.get(3) instanceof Symbol);
		assertEquals("str", array.get(4).value());
		assertTrue(array.get(4) instanceof StringConstant);
		assertEquals("c", array.get(5).value());
		assertTrue(array.get(5) instanceof CharacterConstant);
		assertEquals("<array>", array.get(6).value());
		assertTrue(array.get(6) instanceof Array);
		assertEquals(0, ((Array) array.get(6)).size());
		assertEquals("<array>", array.get(7).value());
		assertTrue(array.get(7) instanceof Array);
		assertEquals(1, ((Array) array.get(7)).size());
		assertTrue(((Array) array.get(7)).get(0) instanceof StringConstant);
		assertEquals("nested", ((Array) array.get(7)).get(0).value());
		assertNoLexerExceptions();
	}

	private void assertNoLexerExceptions() {
		assertTrue(lexer.getExceptions().isEmpty());
	}

	SmalltalkParser createParser(String input) throws IOException {
		CharStream stream = new ANTLRStringStream(input);
		// keep lexor so we can inspect it for errors,
		lexer = new SmalltalkLexer(stream);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		SmalltalkParser parser = new SmalltalkParser(tokens);
		return parser;
	}
}
