/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import st.redline.core.RedlineException;
import st.redline.core.SmalltalkEnvironment;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.*;

public class ProgramAnalyserTest {

	ProgramAnalyser analyser;
	ClassBytecodeWriter writer;
	Analyser parent;

	@Before
	public void setup() {
		writer = mock(ClassBytecodeWriter.class);
		parent = mock(Analyser.class);
		analyser = new ProgramAnalyser(parent, writer, false, "", "");
	}

	@Test
	public void shouldInvokePrimitives() {
		Primitive primitive = new Primitive("primitive:", 2, "43");
		primitive.accept(analyser);
		verify(writer).invokePrimitive(2, "43");
	}

	@Test
	public void shouldRegisterAndInvokeCompilerOfBlockWhenVisitBeginOfBlock() {
		Block block = mock(Block.class);
		when(parent.className()).thenReturn("SomeClass");
		when(parent.packageName()).thenReturn("st.redline");
		ProgramAnalyser spy = spy(analyser);
		SmalltalkEnvironment smalltalkEnvironment = mock(SmalltalkEnvironment.class);
		doReturn(smalltalkEnvironment).when(spy).smalltalkEnvironment();
		spy.visitBegin(block, 1);
		verify(smalltalkEnvironment).registerBlockToBeCompiled(block, "st.redline.SomeClass$M1");
		verify(writer).invokeObjectCompileBlock("st.redline.SomeClass$M1", 1);
	}

	@Test
	public void shouldAlwaysSkipBlockVisits() {
		Assert.assertTrue(analyser.skipBlockVisit(null));
	}

	@Test
	public void shouldInvokePrimObjectArrayWhenVisitArray() {
		Array array = mock(Array.class);
		when(array.size()).thenReturn(2);
		analyser.visitBegin(array);
		verify(writer).invokeObjectArray(2);
	}

	@Test
	public void shouldInvokePrimObjectSymbolWhenVisitSymbolConstant() {
		SymbolConstant symbolConstant = mock(SymbolConstant.class);
		analyser.visit(symbolConstant, "sym", 1);
		verify(writer).invokeObjectSymbol("sym", 1);
	}

	@Test
	public void shouldInvokeArrayPutWhenVisitSymbolInsideAnArray() {
		Symbol symbol = mock(Symbol.class);
		analyser.visit(symbol, "sym", 32, true, 1);
		verify(writer).invokeObjectSymbol("sym", 1);
		verify(writer).invokeArrayPutAt(32, 1);
	}

	@Test
	public void shouldInvokePrimObjectSymbolWhenVisitSymbol() {
		Symbol symbol = mock(Symbol.class);
		analyser.visit(symbol, "sym", 32, false, 1);
		verify(writer).invokeObjectSymbol("sym", 1);
	}

	@Test
	public void shouldInvokeArrayPutWhenVisitCharacterConstantInsideAnArray() {
		CharacterConstant characterConstant = mock(CharacterConstant.class);
		analyser.visit(characterConstant, "c", 32, true, 1);
		verify(writer).invokeObjectCharacter("c", 1);
		verify(writer).invokeArrayPutAt(32, 1);
	}

	@Test
	public void shouldInvokePrimObjectCharacterWhenVisitCharacterConstant() {
		CharacterConstant characterConstant = mock(CharacterConstant.class);
		analyser.visit(characterConstant, "c", 32, false, 1);
		verify(writer).invokeObjectCharacter("c", 1);
	}

	@Test
	public void shouldInvokeArrayPutWhenVisitNumberInsideAnArray() {
		Number number = mock(Number.class);
		analyser.visit(number, "16", 32, true, 1);
		verify(writer).invokeObjectNumber("16", 1);
		verify(writer).invokeArrayPutAt(32, 1);
	}

	@Test
	public void shouldInvokePrimObjectNumberWhenVisitNumber() {
		Number number = mock(Number.class);
		analyser.visit(number, "16", 32, false, 1);
		verify(writer).invokeObjectNumber("16", 1);
	}

	@Test
	public void shouldInvokeArrayPutWhenVisitStringConstantInsideAnArray() {
		StringConstant stringConstant = mock(StringConstant.class);
		analyser.visit(stringConstant, "hello", 32, true, 1);
		verify(writer).invokeObjectString("hello", 1);
		verify(writer).invokeArrayPutAt(32, 1);
	}

	@Test
	public void shouldInvokePrimObjectStringWhenVisitStringConstant() {
		StringConstant stringConstant = mock(StringConstant.class);
		analyser.visit(stringConstant, "hello", 0, false, 1);
		verify(writer).invokeObjectString("hello", 1);
	}

	@Test
	public void shouldInvokeVariableAtWhenVisitIdentifierOnLoadSideOfExpression() {
		Identifier identifier = mock(Identifier.class);
		when(identifier.isOnLoadSideOfExpression()).thenReturn(true);
		analyser.visit(identifier, "var", 3);
		verify(writer).invokeVariableAt("var", 3);
	}

	@Test
	public void shouldPushArgumentWhenVisitArgumentOnLoadSideOfExpression() {
		Identifier identifier = mock(Identifier.class);
		when(identifier.isOnLoadSideOfExpression()).thenReturn(true);
		analyser.initializeBlockArgumentsRegistration();
		analyser.argumentsRegistry().put("arg", 1);
		analyser.visit(identifier, "arg", 3);
		verify(writer).pushArgument(1);
	}

	@Test
	public void shouldPushTemporaryWhenVisitTemporaryOnLoadSideOfExpression() {
		Identifier identifier = mock(Identifier.class);
		when(identifier.isOnLoadSideOfExpression()).thenReturn(true);
		analyser.initializeTemporariesRegistration();
		analyser.temporariesRegistry().put("tmp", 1);
		analyser.visit(identifier, "tmp", 3);
		verify(writer).pushTemporary(1);
	}

	@Test (expected = RedlineException.class)
	public void shouldNotAllowStoreIntoMethodOrBlockArgument() {
		Identifier identifier = mock(Identifier.class);
		when(identifier.isOnLoadSideOfExpression()).thenReturn(false);
		analyser.initializeBlockArgumentsRegistration();
		analyser.argumentsRegistry().put("arg", 1);
		analyser.initializeTemporariesRegistration();
		analyser.visit(identifier, "arg", 3);
	}

	@Test
	public void shouldStoreTemporaryWhenVisitTemporaryOnStoreSideOfExpression() {
		Identifier identifier = mock(Identifier.class);
		when(identifier.isOnLoadSideOfExpression()).thenReturn(false);
		analyser.initializeTemporariesRegistration();
		analyser.temporariesRegistry().put("tmp", 1);
		analyser.visit(identifier, "tmp", 3);
		verify(writer).storeTemporary(1);
	}

	@Test
	public void shouldInvokePerformWhenVisitEndsOfKeywordExpression() {
		KeywordExpression keywordExpression = mock(KeywordExpression.class);
		analyser.visitEnd(keywordExpression, "at:", 1, 3);
		verify(writer).invokeObjectPerform("at:", 1, false);
	}

	@Test
	public void shouldInvokePerformWhenVisitEndsOfKeywordMessageElementNode() {
		KeywordMessageElement keywordMessageElement = mock(KeywordMessageElement.class);
		analyser.visitEnd(keywordMessageElement, "at:put:", 2, 3);
		verify(writer).invokeObjectPerform("at:put:", 2, false);
	}

	@Test
	public void shouldInvokePerformWhenVisitingUnarySelectorMessageElementNode() {
		UnarySelector unarySelector = new UnarySelector("yourself", 42);
		UnarySelectorMessageElement unarySelectorMessageElement = new UnarySelectorMessageElement(unarySelector);
		analyser.visit(unarySelectorMessageElement, "yourself", 42);
		verify(writer).visitLine(42);
		verify(writer).invokeObjectPerform("yourself", 0, false);
	}

	@Test
	public void shouldInvokePerformWhenVisitingUnarySelectorNode() {
		UnarySelector unarySelector = new UnarySelector("yourself", 42);
		analyser.visit(unarySelector, "yourself", 42);
		verify(writer).visitLine(42);
		verify(writer).invokeObjectPerform("yourself", 0, false);
	}

	@Test
	public void shouldPushReceiverWhenVisitingSelfNode() {
		Self self = mock(Self.class);
		analyser.visit(self, 32);
		verify(writer).visitLine(32);
		verify(writer).pushReceiver();
	}

	@Test
	public void shouldPushNilPrimObjectFieldWhenVisitingNilNode() {
		Nil nil = mock(Nil.class);
		analyser.visit(nil, 32);
		verify(writer).visitLine(32);
		verify(writer).pushObjectStaticField("NIL");
	}

	@Test
	public void shouldPushTruePrimObjectFieldWhenVisitingTrueNode() {
		True aTrue = mock(True.class);
		analyser.visit(aTrue, 32);
		verify(writer).visitLine(32);
		verify(writer).pushObjectStaticField("TRUE");
	}

	@Test
	public void shouldPushFalsePrimObjectFieldWhenVisitingFalseNode() {
		False aFalse = mock(False.class);
		analyser.visit(aFalse, 32);
		verify(writer).visitLine(32);
		verify(writer).pushObjectStaticField("FALSE");
	}

	@Test
	public void shouldPopResultFromStackWhenSimpleExpressionResultIsNotToBeLeftOnStack() {
		SimpleExpression simpleExpression = mock(SimpleExpression.class);
		when(simpleExpression.isResultLeftOnStack()).thenReturn(false);
		when(simpleExpression.isResultDuplicatedOnStack()).thenReturn(false);
		analyser.visitEnd(simpleExpression);
		verify(writer).pop();
	}

	@Test
	public void shouldDuplicateResultOnStackWhenSimpleExpressionResultIsToBeDuplicatedOnStack() {
		SimpleExpression simpleExpression = mock(SimpleExpression.class);
		when(simpleExpression.isResultDuplicatedOnStack()).thenReturn(true);
		when(simpleExpression.isResultLeftOnStack()).thenReturn(false);
		analyser.visitEnd(simpleExpression);
		verify(writer).pushDuplicate();
	}

	@Test
	public void shouldDuplicateStackWhenCascadeBegins() {
		analyser.visitBegin(mock(Cascade.class));
		verify(writer).pushDuplicate();
	}

	@Test
	public void shouldPopStackWhenCascadeEnds() {
		analyser.visitEnd(mock(Cascade.class));
		verify(writer).pop();
	}

	@Test
	public void shouldHaveNoTemporariesRegistryWhenCreated() {
		ProgramAnalyser programAnalyser = new ProgramAnalyser(parent, "foo", "com.domain", "", false);
		assertNull(programAnalyser.temporariesRegistry());
		assertEquals(0, programAnalyser.temporariesIndex());
	}

	@Test
	public void shouldHaveNoArgumentsRegistryWhenCreated() {
		ProgramAnalyser programAnalyser = new ProgramAnalyser(parent, "foo", "com.domain", "", false);
		assertNull(programAnalyser.argumentsRegistry());
		assertEquals(0, programAnalyser.argumentsIndex());
	}

	@Test
	public void shouldCreateClassBytecodeWriter() {
		assertNotNull(new ProgramAnalyser(parent, "foo", "com.domain", "", false).classBytecodeWriter());
	}

	@Test
	public void shouldDelegateClassBytesToClassBytecodeWriter() {
		analyser.classBytes();
		verify(writer).contents();
	}

	@Test
	public void shouldOpenClassWhenVisitingProgramNodeBegins() {
		analyser.visitBegin(mock(Program.class));
		verify(writer).openClass();
	}

	@Test
	public void shouldCloseClassWhenVisitingProgramNodeEnds() {
		analyser.visitEnd(mock(Program.class));
		verify(writer).closeClass();
	}

	@Test
	public void shouldInitializeArgumentRegistryWhenVisitingBlockArguments() {
		BlockArguments blockArguments = mock(BlockArguments.class);
		analyser.visitBegin(blockArguments, 1);
		assertNotNull(analyser.argumentsRegistry());
		assertEquals(0, analyser.argumentsRegistry().size());
	}

	@Test
	public void shouldInitializeTemporaryRegistryWhenVisitingTemporaries() {
		Temporaries temporaries = mock(Temporaries.class);
		analyser.visitBegin(temporaries);
		assertNotNull(analyser.temporariesRegistry());
		assertEquals(0, analyser.temporariesRegistry().size());
	}

	@Test
	public void shouldInitializeContextTemporariesWhenVisitingTemporaries() {
		Temporaries temporaries = mock(Temporaries.class);
		when(temporaries.size()).thenReturn(2);
		analyser.visitBegin(temporaries);
		verify(writer).invokeContextTemporariesInit(2);
	}

	@Test
	public void shouldRegisterTemporaryWhenVisitingTemporary() {
		Temporaries temporaries = mock(Temporaries.class);
		Temporary temporary = mock(Temporary.class);
		analyser.visitBegin(temporaries);
		analyser.visit(temporary, "temp", 1);
		assertNotNull(analyser.temporariesRegistry());
		assertEquals(1, analyser.temporariesRegistry().size());
		assertEquals(0, (int) analyser.temporariesRegistry().get("temp"));
		assertEquals(1, analyser.temporariesIndex());
	}

	@Test
	public void shouldRegisterArgumentWhenVisitingBlockArgument() {
		BlockArguments blockArguments = mock(BlockArguments.class);
		BlockArgument blockArgument = mock(BlockArgument.class);
		analyser.visitBegin(blockArguments, 1);
		analyser.visit(blockArgument, "arg", 1);
		assertNotNull(analyser.argumentsRegistry());
		assertEquals(1, analyser.argumentsRegistry().size());
		assertEquals(0, (int) analyser.argumentsRegistry().get("arg"));
		assertEquals(1, analyser.argumentsIndex());
	}
}
