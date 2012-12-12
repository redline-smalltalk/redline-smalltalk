/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class AnalyserTest {

	static String CLASS_NAME = "Example";
	static String PACKAGE_NAME = "com.domain";
	static String SOURCE_PATH = "";

	Analyser analyser;
	ProgramAnalyser delegate;

	@Before
	public void setup() {
		analyser = new Analyser(CLASS_NAME, PACKAGE_NAME, SOURCE_PATH, false);
		delegate = mock(ProgramAnalyser.class);
		analyser.currentDelegate(delegate);
	}

	@Test
	public void shouldDelegateVisitPrimitiveNode() {
		Primitive primitive = new Primitive("primitive:", 2, "43");
		primitive.accept(analyser);
		verify(delegate).visit(primitive, "primitive:", 2, "43");
	}

	@Test
	public void shouldDelegateVisitOfUnarySelectorMessageElementNode() {
		UnarySelector unarySelector = mock(UnarySelector.class);
		when(unarySelector.value()).thenReturn("yourself");
		when(unarySelector.line()).thenReturn(32);
		UnarySelectorMessageElement unarySelectorMessageElement = new UnarySelectorMessageElement(unarySelector);
		unarySelectorMessageElement.accept(analyser);
		verify(delegate).visit(unarySelectorMessageElement, "yourself", 32);
	}

	@Test
	public void shouldDelegateVisitOfUnaryObjectDescriptionNode() {
		Primary primary = mock(Primary.class);
		UnaryObjectDescription unaryObjectDescription = new UnaryObjectDescription(primary);
		unaryObjectDescription.accept(analyser);
		verify(delegate).visit(unaryObjectDescription);
	}

	@Test
	public void shouldDelegateVisitOfSymbolNode() {
		Symbol symbol = new Symbol();
		symbol.index(2);
		symbol.valueAndLine("sym", 42);
		symbol.accept(analyser);
		verify(delegate).visit(symbol, "sym", 2, false, 42);
	}

	@Test
	public void shouldDelegateVisitOfStringConstantNode() {
		StringConstant stringConstant = new StringConstant("'foo'", 42);
		stringConstant.index(2);
		stringConstant.accept(analyser);
		verify(delegate).visit(stringConstant, "foo", 2, false, 42);
	}

	@Test
	public void shouldDelegateVisitOfPrimaryStatementsNode() {
		Statements statements = mock(Statements.class);
		when(statements.line()).thenReturn(12);
		PrimaryStatements primaryStatements = new PrimaryStatements(statements);
		primaryStatements.accept(analyser);
		verify(delegate).visit(primaryStatements, 12);
	}

	@Test
	public void shouldDelegateVisitOfPrimaryExpressionNode() {
		Expression expression = mock(Expression.class);
		when(expression.line()).thenReturn(12);
		PrimaryExpression primaryExpression = new PrimaryExpression(expression);
		primaryExpression.accept(analyser);
		verify(delegate).visit(primaryExpression, 12);
	}

	@Test
	public void shouldDelegateVisitOfNumberNode() {
		Number number = new Number(null, null, null, "123", 2, null, null, null, null);
		number.accept(analyser);
		verify(delegate).visit(number, "123", 0, false, 2);
	}

	@Test
	public void shouldDelegateVisitOfKeywordMessageElementNode() {
		KeywordMessageElement keywordMessageElement = new KeywordMessageElement("at:", 32, mock(BinaryObjectDescription.class));
		keywordMessageElement.add("put:", 33, mock(BinaryObjectDescription.class));
		keywordMessageElement.accept(analyser);
		verify(delegate).visitBegin(keywordMessageElement, "at:put:", 2, 32);
		verify(delegate).visitEnd(keywordMessageElement, "at:put:", 2, 32);
	}

	@Test
	public void shouldDelegateVisitOfKeywordExpressionNode() {
		KeywordExpression keywordExpression = new KeywordExpression();
		keywordExpression.add("at:", 32, mock(BinaryObjectDescription.class));
		keywordExpression.add("put:", 33, mock(BinaryObjectDescription.class));
		keywordExpression.accept(analyser);
		verify(delegate).visitBegin(keywordExpression, "at:put:", 2, 32);
		verify(delegate).visitEnd(keywordExpression, "at:put:", 2, 32);
	}

	@Test
	public void shouldDelegateVisitOfJVMNode() {
		JVM jvm = new JVM();
		jvm.accept(analyser, 32);
		verify(delegate).visit(jvm, 32);
	}

	@Test
	public void shouldDelegateVisitOfCharacterConstantNode() {
		CharacterConstant characterConstant = new CharacterConstant("z", 42);
		characterConstant.index(2);
		characterConstant.accept(analyser);
		verify(delegate).visit(characterConstant, "z", 2, false, 42);
	}

	@Test
	public void shouldDelegateVisitOfBlockArgumentsNode() {
		BlockArgument blockArgument = mock(BlockArgument.class);
		List<BlockArgument> blockArgumentList = new ArrayList<BlockArgument>();
		blockArgumentList.add(blockArgument);
		BlockArguments blockArguments = new BlockArguments(blockArgumentList);
		blockArguments.accept(analyser);
		verify(delegate).visitBegin(blockArguments, 1);
		verify(delegate).visitEnd(blockArguments, 1);
	}

	@Test
	public void shouldDelegateVisitOfBlockArgumentNode() {
		BlockArgument blockArgument = new BlockArgument("arg", 42);
		blockArgument.accept(analyser);
		verify(delegate).visit(blockArgument, "arg", 42);
	}

	@Test
	public void shouldDelegateVisitOfBlockNode() {
		Block block = new Block(42, mock(BlockArguments.class), mock(Temporaries.class), mock(Statements.class));
		block.accept(analyser);
		verify(delegate).visitBegin(block, 42);
		verify(delegate).visitEnd(block, 42);
	}

	@Test
	public void shouldDelegateVisitOfBinarySelectorMessageElementNode() {
		BinarySelector binarySelector = new BinarySelector();
		binarySelector.add("/", 42);
		UnaryObjectDescription unaryObjectDescription = mock(UnaryObjectDescription.class);
		BinarySelectorMessageElement binarySelectorMessageElement = new BinarySelectorMessageElement(binarySelector, unaryObjectDescription);
		binarySelectorMessageElement.accept(analyser);
		verify(delegate).visitBegin(binarySelectorMessageElement, "/", 42);
		verify(delegate).visitEnd(binarySelectorMessageElement, "/", 42);
	}

	@Test
	public void shouldDelegateVisitOfBinarySelectorNode() {
		BinarySelector binarySelector = new BinarySelector();
		binarySelector.add("+", 42);
		binarySelector.accept(analyser);
		verify(delegate).visit(binarySelector, "+", 42);
	}

	@Test
	public void shouldDelegateVisitOfBinaryObjectDescriptionNode() {
		Primary primary = mock(Primary.class);
		BinaryObjectDescription binaryObjectDescription = new BinaryObjectDescription(primary);
		binaryObjectDescription.accept(analyser);
		verify(delegate).visit(binaryObjectDescription);
	}

	@Test
	public void shouldDelegateVisitOfBinaryExpressionNode() {
		BinaryExpression binaryExpression = new BinaryExpression();
		binaryExpression.accept(analyser);
		verify(delegate).visitBegin(binaryExpression);
		verify(delegate).visitEnd(binaryExpression);
	}

	@Test
	public void shouldDelegateVisitOfArrayConstantNode() {
		ArrayConstant arrayConstant = new ArrayConstant(mock(Array.class),  42);
		arrayConstant.accept(analyser);
		verify(delegate).visit(arrayConstant, 42);
	}

	@Test
	public void shouldDelegateVisitOfArrayNode() {
		Array array = new Array();
		array.accept(analyser);
		verify(delegate).visitBegin(array);
		verify(delegate).visitEnd(array);
	}

	@Test
	public void shouldDelegateVisitOfAssignmentExpressionNode() {
		AssignmentExpression assignmentExpression = new AssignmentExpression(mock(Identifier.class), mock(Expression.class));
		assignmentExpression.accept(analyser);
		verify(delegate).visitBegin(assignmentExpression);
		verify(delegate).visitEnd(assignmentExpression);
	}

	@Test
	public void shouldDelegateVisitOfUnaryExpressionNode() {
		UnaryExpression unaryExpression = new UnaryExpression();
		unaryExpression.accept(analyser);
		verify(delegate).visitBegin(unaryExpression);
		verify(delegate).visitEnd(unaryExpression);
	}

	@Test
	public void shouldDelegateVisitOfUnarySelectorNode() {
		UnarySelector unarySelector = new UnarySelector("yourself", 32);
		analyser.visit(unarySelector, "yourself", 32);
		verify(delegate).visit(unarySelector, "yourself", 32);
	}

	@Test
	public void shouldDefaultToProgramAnalyserDelegate() {
		assertTrue(new Analyser(CLASS_NAME, PACKAGE_NAME, SOURCE_PATH, false).currentDelegate() instanceof ProgramAnalyser);
	}

	@Test
	public void shouldDefaultToTracingAnalyserDelegateWhenVerbose() {
		assertTrue(new Analyser(CLASS_NAME, PACKAGE_NAME, SOURCE_PATH, true).currentDelegate() instanceof TracingAnalyser);
	}

	@Test
	public void shouldDelegateGetOfClassBytes() {
		analyser.classBytes();
		verify(delegate).classBytes();
	}

	@Test
	public void shouldDelegateVisitOfSelfNode() {
		// Self node is a synthetic node generated by Identifier.
		Self self = new Self();
		analyser.visit(self, 21);
		verify(delegate).visit(self, 21);
	}

	@Test
	public void shouldDelegateVisitOfSuperNode() {
		// Super node is a synthetic node generated by Identifier.
		Super aSuper = new Super();
		analyser.visit(aSuper, 21);
		verify(delegate).visit(aSuper, 21);
	}

	@Test
	public void shouldDelegateVisitOfTrueNode() {
		// True node is a synthetic node generated by Identifier.
		True aTrue = new True();
		analyser.visit(aTrue, 21);
		verify(delegate).visit(aTrue, 21);
	}

	@Test
	public void shouldDelegateVisitOfFalseNode() {
		// False node is a synthetic node generated by Identifier.
		False aFalse = new False();
		analyser.visit(aFalse, 21);
		verify(delegate).visit(aFalse, 21);
	}

	@Test
	public void shouldDelegateVisitOfNilNode() {
		// Nil node is a synthetic node generated by Identifier.
		Nil nil = new Nil();
		analyser.visit(nil, 21);
		verify(delegate).visit(nil, 21);
	}

	@Test
	public void shouldDelegateVisitOfIdentifierNode() {
		Identifier identifier = new Identifier("val", 32);
		analyser.visit(identifier, "val", 32);
		verify(delegate).visit(identifier, "val", 32);
	}

	@Test
	public void shouldDelegateVisitOfCascadeNode() {
		// Cascade node is a synthetic node not generated by Parser.
		Cascade cascade = new Cascade();
		analyser.visitBegin(cascade);
		analyser.visitEnd(cascade);
		verify(delegate).visitBegin(cascade);
		verify(delegate).visitEnd(cascade);
	}

	@Test
	public void shouldDelegateVisitOfSimpleExpressionNode() {
		SimpleExpression simpleExpression = new SimpleExpression();
		simpleExpression.accept(analyser);
		verify(delegate).visitBegin(simpleExpression);
		verify(delegate).visitEnd(simpleExpression);
	}

	@Test
	public void shouldDelegateVisitOfStatementsNode() {
		Statements statements = new Statements(null, null);
		statements.accept(analyser);
		verify(delegate).visitBegin(statements);
		verify(delegate).visitEnd(statements);
	}

	@Test
	public void shouldDelegateVisitOfProgramNode() {
		Program program = new Program(null, null, null);
		program.accept(analyser);
		verify(delegate).visitBegin(program);
		verify(delegate).visitEnd(program);
	}

	@Test
	public void shouldDelegateVisitOfTemporaryNode() {
		Temporary temporary = new Temporary("x", 32);
		temporary.accept(analyser);
		verify(delegate).visit(temporary, "x", 32);
	}
}
