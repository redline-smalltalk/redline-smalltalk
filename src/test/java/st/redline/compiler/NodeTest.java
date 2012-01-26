/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;

import java.util.ArrayList;
import java.util.List;

import static junit.framework.Assert.*;
import static org.mockito.Mockito.*;

public class NodeTest {

	NodeVisitor visitor;

	@Before
	public void setup() {
		visitor = mock(NodeVisitor.class);
	}

	@Test
	public void unaryObjectDescriptionNodeShouldBeVisitable() {
		Primary primary = mock(Primary.class);
		UnarySelector unarySelector = mock(UnarySelector.class);
		UnaryObjectDescription unaryObjectDescription = new UnaryObjectDescription(primary);
		unaryObjectDescription.add(unarySelector);
		unaryObjectDescription.accept(visitor);
		verify(visitor).visit(unaryObjectDescription);
		verify(primary).accept(visitor);
		verify(unarySelector).accept(visitor);
	}

	@Test
	public void symbolConstantNodeShouldBeVisitable() {
		SymbolConstant symbolConstant = new SymbolConstant("sym", 32);
		symbolConstant.accept(visitor);
		verify(visitor).visit(symbolConstant, "sym", 32);
	}

	@Test
	public void symbolNodeShouldBeVisitable() {
		Symbol symbol = new Symbol();
		symbol.index(2);
		symbol.valueAndLine("sym", 42);
		symbol.accept(visitor);
		verify(visitor).visit(symbol, "sym", 2, 42);
	}

	@Test
	public void stringConstantNodeShouldBeVisitable() {
		StringConstant stringConstant = new StringConstant("'str'", 42);
		stringConstant.index(2);
		stringConstant.accept(visitor);
		verify(visitor).visit(stringConstant, "str", 2, 42);
	}

	@Test
	public void primaryStatementsNodeShouldBeVisitable() {
		Statements statements = mock(Statements.class);
		when(statements.line()).thenReturn(32);
		PrimaryStatements primaryStatements = new PrimaryStatements(statements);
		primaryStatements.accept(visitor);
		verify(visitor).visit(primaryStatements, 32);
		verify(statements).accept(visitor);
	}

	@Test
	public void primaryExpressionConstructionShouldLeaveResultOnStack() {
		Expression expression = mock(Expression.class);
		new PrimaryExpression(expression);
		verify(expression).leaveResultOnStack();
	}

	@Test
	public void primaryExpressionNodeShouldBeVisitable() {
		Expression expression = mock(Expression.class);
		when(expression.line()).thenReturn(32);
		PrimaryExpression primaryExpression = new PrimaryExpression(expression);
		primaryExpression.accept(visitor);
		verify(visitor).visit(primaryExpression, 32);
		verify(expression).accept(visitor);
	}

	@Test
	public void numberNodeShouldBeVisitable() {
		Number number = new Number(null, null, null, "123", 2, "45", null, null, null);
		number.accept(visitor);
		verify(visitor).visit(number, "123.45", 2);
	}

	@Test
	public void keywordMessageElementNodeShouldBeVisitable() {
		BinaryObjectDescription binaryObjectDescription = mock(BinaryObjectDescription.class);
		KeywordMessageElement keywordMessageElement = new KeywordMessageElement("at:", 32, binaryObjectDescription);
		keywordMessageElement.accept(visitor);
		verify(visitor).visitBegin(keywordMessageElement, "at:", 1, 32);
		verify(visitor).visitEnd(keywordMessageElement, "at:", 1, 32);
	}

	@Test
	public void keywordExpressionNodeShouldBeVisitable() {
		BinaryObjectDescription binaryObjectDescription = mock(BinaryObjectDescription.class);
		KeywordExpression keywordExpression = new KeywordExpression();
		keywordExpression.add("at:", 32, binaryObjectDescription);
		keywordExpression.accept(visitor);
		verify(visitor).visitBegin(keywordExpression, "at:", 1, 32);
		verify(visitor).visitEnd(keywordExpression, "at:", 1, 32);
	}

	@Test
	public void characterConstantNodeShouldBeVisitable() {
		CharacterConstant characterConstant = new CharacterConstant("a", 42);
		characterConstant.index(2);
		characterConstant.accept(visitor);
		verify(visitor).visit(characterConstant, "a", 2, 42);
	}

	@Test
	public void blockArgumentsNodeShouldBeVisitable() {
		BlockArgument blockArgument = mock(BlockArgument.class);
		List<BlockArgument> blockArgumentsList = new ArrayList<BlockArgument>();
		blockArgumentsList.add(blockArgument);
		BlockArguments blockArguments = new BlockArguments(blockArgumentsList);
		blockArguments.accept(visitor);
		verify(visitor).visitBegin(blockArguments, 1);
		verify(blockArgument).accept(visitor);
		verify(visitor).visitEnd(blockArguments, 1);
	}

	@Test
	public void blockArgumentNodeShouldBeVisitable() {
		BlockArgument blockArgument = new BlockArgument("arg", 42);
		blockArgument.accept(visitor);
		verify(visitor).visit(blockArgument, "arg", 42);
	}

	@Test
	public void blockNodeShouldBeVisitable() {
		BlockArguments blockArguments = mock(BlockArguments.class);
		Temporaries temporaries = mock(Temporaries.class);
		Statements statements = mock(Statements.class);
		Block block = new Block(42, blockArguments, temporaries, statements);
		block.accept(visitor);
		verify(visitor).visitBegin(block, 42);
		verify(blockArguments).accept(visitor);
		verify(temporaries).accept(visitor);
		verify(statements).accept(visitor);
		verify(visitor).visitEnd(block, 42);
	}

	@Test
	public void binarySelectorUnaryObjectDescriptionNodeShouldBeVisitable() {
		BinarySelector binarySelector = mock(BinarySelector.class);
		UnaryObjectDescription unaryObjectDescription = mock(UnaryObjectDescription.class);
		BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription = new BinarySelectorUnaryObjectDescription(binarySelector, unaryObjectDescription);
		binarySelectorUnaryObjectDescription.accept(visitor);
		verify(unaryObjectDescription).accept(visitor);
		verify(binarySelector).accept(visitor);
	}

	@Test
	public void binarySelectorMessageElementNodeShouldBeVisitable() {
		BinarySelector binarySelector = new BinarySelector();
		binarySelector.add("+", 32);
		UnaryObjectDescription unaryObjectDescription = mock(UnaryObjectDescription.class);
		BinarySelectorMessageElement binarySelectorMessageElement = new BinarySelectorMessageElement(binarySelector, unaryObjectDescription);
		binarySelectorMessageElement.accept(visitor);
		verify(visitor).visitBegin(binarySelectorMessageElement, "+", 32);
		verify(unaryObjectDescription).accept(visitor);
		verify(visitor).visitEnd(binarySelectorMessageElement, "+", 32);
	}

	@Test
	public void binarySelectorNodeShouldBeVisitable() {
		BinarySelector binarySelector = new BinarySelector();
		binarySelector.add("-", 32);
		binarySelector.add("-", 32);
		binarySelector.accept(visitor);
		verify(visitor).visit(binarySelector, "--", 32);
	}

	@Test
	public void binaryObjectDescriptionNodeShouldAddBinarySelectorAndUnaryObjectDescription() {
		Primary primary = mock(Primary.class);
		BinarySelector binarySelector = mock(BinarySelector.class);
		UnaryObjectDescription unaryObjectDescription = mock(UnaryObjectDescription.class);
		BinaryObjectDescription binaryObjectDescription = new BinaryObjectDescription(primary);
		binaryObjectDescription.add(binarySelector, unaryObjectDescription);
		assertEquals(binarySelector, binaryObjectDescription.binarySelectorUnaryObjectDescriptions().get(0).binarySelector());
		assertEquals(unaryObjectDescription, binaryObjectDescription.binarySelectorUnaryObjectDescriptions().get(0).unaryObjectDescription());
	}

	@Test
	public void binaryObjectDescriptionNodeShouldAddUnarySelectors() {
		Primary primary = mock(Primary.class);
		UnarySelector unarySelector = mock(UnarySelector.class);
		BinaryObjectDescription binaryObjectDescription = new BinaryObjectDescription(primary);
		binaryObjectDescription.add(unarySelector);
		assertEquals(unarySelector, binaryObjectDescription.unarySelectors().get(0));
	}

	@Test
	public void binaryObjectDescriptionNodeShouldBeVisitable() {
		Primary primary = mock(Primary.class);
		UnarySelector unarySelector = mock(UnarySelector.class);
		BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription = mock(BinarySelectorUnaryObjectDescription.class);
		BinaryObjectDescription binaryObjectDescription = new BinaryObjectDescription(primary);
		binaryObjectDescription.add(unarySelector);
		binaryObjectDescription.binarySelectorUnaryObjectDescriptions().add(binarySelectorUnaryObjectDescription);
		binaryObjectDescription.accept(visitor);
		verify(visitor).visit(binaryObjectDescription);
		verify(primary).accept(visitor);
		verify(unarySelector).accept(visitor);
		verify(binarySelectorUnaryObjectDescription).accept(visitor);
	}

	@Test
	public void binaryExpressionNodeShouldBeVisitable() {
		BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription = mock(BinarySelectorUnaryObjectDescription.class);
		KeywordExpression keywordExpression = mock(KeywordExpression.class);
		BinaryExpression binaryExpression = new BinaryExpression();
		binaryExpression.add(keywordExpression);
		binaryExpression.binarySelectorUnaryObjectDescriptions().add(binarySelectorUnaryObjectDescription);
		binaryExpression.accept(visitor);
		verify(visitor).visitBegin(binaryExpression);
		verify(keywordExpression).accept(visitor);
		verify(binarySelectorUnaryObjectDescription).accept(visitor);
		verify(visitor).visitEnd(binaryExpression);
	}

	@Test
	public void binaryExpressionNodeShouldAddBinarySelectorAndUnaryObjectDescription() {
		BinarySelector binarySelector = mock(BinarySelector.class);
		UnaryObjectDescription unaryObjectDescription = mock(UnaryObjectDescription.class);
		BinaryExpression binaryExpression = new BinaryExpression();
		binaryExpression.add(binarySelector, unaryObjectDescription);
		assertEquals(binarySelector, binaryExpression.binarySelectorUnaryObjectDescriptions().get(0).binarySelector());
		assertEquals(unaryObjectDescription, binaryExpression.binarySelectorUnaryObjectDescriptions().get(0).unaryObjectDescription());
	}

	@Test
	public void arrayConstantNodeShouldBeVisitable() {
		Array array = mock(Array.class);
		ArrayConstant arrayConstant = new ArrayConstant(array, 42);
		arrayConstant.accept(visitor);
		verify(visitor).visit(arrayConstant, 42);
		verify(array).accept(visitor);
	}

	@Test
	public void arrayNodeShouldIndexElementsWhenAdded() {
		ArrayElement arrayElement = mock(ArrayElement.class);
		Array array = new Array();
		array.add(arrayElement);
		array.add(arrayElement);
		verify(arrayElement).index(1);
		verify(arrayElement).index(2);
	}

	@Test
	public void arrayNodeShouldBeVisitable() {
		ArrayElement arrayElement = mock(ArrayElement.class);
		Array array = new Array();
		array.add(arrayElement);
		array.accept(visitor);
		verify(visitor).visitBegin(array);
		verify(arrayElement).accept(visitor);
		verify(visitor).visitEnd(array);
	}

	@Test
	public void answerStatementNodeConstructionShouldTellExpressionToLeaveResultOnStack() {
		SimpleExpression simpleExpression = mock(SimpleExpression.class);
		new AnswerStatement(42, simpleExpression);
		verify(simpleExpression).leaveResultOnStack();
	}

	@Test
	public void answerStatementNodeShouldBeVisitable() {
		SimpleExpression simpleExpression = mock(SimpleExpression.class);
		AnswerStatement answerStatement = new AnswerStatement(42, simpleExpression);
		answerStatement.accept(visitor);
		verify(visitor).visitBegin(answerStatement);
		verify(simpleExpression).accept(visitor);
		verify(visitor).visitEnd(answerStatement);
	}

	@Test
	public void assignmentExpressionNodeConstructionShouldTellIdentifierItsOnLoadSideOfExpressionAndExpressionToLeaveResultOnStack() {
		Identifier identifier = mock(Identifier.class);
		SimpleExpression simpleExpression = mock(SimpleExpression.class);
		new AssignmentExpression(identifier, simpleExpression);
		verify(identifier).onStoreSideOfExpression();
		verify(simpleExpression).leaveResultOnStack();
	}

	@Test
	public void assignmentExpressionNodeShouldBeVisitable() {
		Identifier identifier = mock(Identifier.class);
		SimpleExpression simpleExpression = mock(SimpleExpression.class);
		AssignmentExpression assignmentExpression = new AssignmentExpression(identifier, simpleExpression);
		assignmentExpression.accept(visitor);
		verify(visitor).visitBegin(assignmentExpression);
		verify(simpleExpression).accept(visitor);
		verify(identifier).accept(visitor);
		verify(visitor).visitEnd(assignmentExpression);
	}

	@Test
	public void unaryExpressionNodeShouldBeVisitable() {
		UnarySelector unarySelector = mock(UnarySelector.class);
		MessageExpression messageExpression = mock(MessageExpression.class);
		UnaryExpression unaryExpression = new UnaryExpression();
		unaryExpression.add(unarySelector);
		unaryExpression.add(messageExpression);
		unaryExpression.accept(visitor);
		verify(visitor).visitBegin(unaryExpression);
		verify(unarySelector).accept(visitor);
		verify(messageExpression).accept(visitor);
		verify(visitor).visitEnd(unaryExpression);
	}

	@Test
	public void unarySelectorNodeShouldBeVisitable() {
		UnarySelector unarySelector = new UnarySelector("yourself", 1);
		unarySelector.accept(visitor);
		verify(visitor).visit(unarySelector, "yourself", 1);
	}

	@Test
	public void shouldVisitSelfSyntheticNodeWhenIdentifierValueIsSelf() {
		Identifier identifier = new Identifier("self", 1);
		identifier.accept(visitor);
		verify(visitor).visit((Self) argThat(new IsClass(Self.class)), anyInt());
	}

	@Test
	public void shouldVisitSuperSyntheticNodeWhenIdentifierValueIsSuper() {
		Identifier identifier = new Identifier("super", 1);
		identifier.accept(visitor);
		verify(visitor).visit((Super) argThat(new IsClass(Super.class)), anyInt());
	}

	@Test
	public void shouldVisitTrueSyntheticNodeWhenIdentifierValueIsTrue() {
		Identifier identifier = new Identifier("true", 1);
		identifier.accept(visitor);
		verify(visitor).visit((True) argThat(new IsClass(True.class)), anyInt());
	}

	@Test
	public void shouldVisitFalseSyntheticNodeWhenIdentifierValueIsFalse() {
		Identifier identifier = new Identifier("false", 1);
		identifier.accept(visitor);
		verify(visitor).visit((False) argThat(new IsClass(False.class)), anyInt());
	}

	@Test
	public void shouldVisitNilSyntheticNodeWhenIdentifierValueIsNil() {
		Identifier identifier = new Identifier("nil", 1);
		identifier.accept(visitor);
		verify(visitor).visit((Nil) argThat(new IsClass(Nil.class)), anyInt());
	}

	@Test
	public void shouldVisitJVMSyntheticNodeWhenIdentifierValueIsJVM() {
		Identifier identifier = new Identifier("JVM", 1);
		identifier.accept(visitor);
		verify(visitor).visit((JVM) argThat(new IsClass(JVM.class)), anyInt());
	}

	@Test
	public void simpleExpressionNodeWithCascadeShouldBeVisitable() {
		Cascade cascade = mock(Cascade.class);
		Primary primary = mock(Primary.class);
		MessageExpression messageExpression = mock(MessageExpression.class);
		MessageElement messageElement = mock(MessageElement.class);
		SimpleExpression.cascade = cascade;
		SimpleExpression simpleExpression = new SimpleExpression();
		simpleExpression.add(primary);
		simpleExpression.add(messageExpression);
		simpleExpression.add(messageElement);
		simpleExpression.add(messageElement);
		simpleExpression.accept(visitor);
		verify(visitor).visitBegin(simpleExpression);
		verify(primary).accept(visitor);
		verify(messageExpression).accept(visitor);
		verify(cascade, times(2)).begin(visitor);
		verify(messageElement, times(2)).accept(visitor);
		verify(cascade).end(visitor);
		verify(visitor).visitEnd(simpleExpression);
	}

	@Test
	public void statementsNodeShouldBeVisitable() {
		Expression expression = mock(Expression.class);
		Statements nestedStatements = mock(Statements.class);
		Statements statements = new Statements(expression, nestedStatements);
		statements.accept(visitor);
		verify(visitor).visitBegin(statements);
		verify(expression).accept(visitor);
		verify(nestedStatements).accept(visitor);
		verify(visitor).visitEnd(statements);
	}

	@Test
	public void programNodeShouldBeVisitable() {
		Temporaries temporaries = mock(Temporaries.class);
		Statements statements = mock(Statements.class);
		Program program = new Program(temporaries, statements);
		program.accept(visitor);
		verify(visitor).visitBegin(program);
		verify(temporaries).accept(visitor);
		verify(statements).accept(visitor);
		verify(visitor).visitEnd(program);
	}

	@Test
	public void temporariesNodeShouldBeVisitable() {
		Temporary temporary = mock(Temporary.class);
		List<Temporary> list = new ArrayList<Temporary>();
		list.add(temporary);
		Temporaries temporaries = new Temporaries(list);
		assertEquals(1, temporaries.size());
		assertFalse(temporaries.isEmpty());
		assertEquals(temporary, temporaries.get(0));
		temporaries.accept(visitor);
		verify(visitor).visitBegin(temporaries);
		verify(temporary).accept(visitor);
		verify(visitor).visitEnd(temporaries);
	}

	@Test
	public void temporaryNodeShouldBeVisitable() {
		Temporary temporary = new Temporary("x", 42);
		temporary.accept(visitor);
		verify(visitor).visit(temporary, "x", 42);
	}

	class IsClass extends ArgumentMatcher {
		private Class aClass;
		public IsClass(Class aClass) {
			this.aClass = aClass;
		}
		public boolean matches(Object anObject) {
			return aClass.equals(anObject.getClass());
		}
	}
}
