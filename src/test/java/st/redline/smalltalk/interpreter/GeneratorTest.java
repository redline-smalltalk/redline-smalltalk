/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package st.redline.smalltalk.interpreter;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class GeneratorTest implements Opcodes {

	private static final String CLASS_NAME = "Model";
	private static final String PACKAGE_INTERNAL_NAME = "app/data";
	private static final String CLASS_FULLY_QUALIFIED_NAME = PACKAGE_INTERNAL_NAME + File.separator + CLASS_NAME;
	private static final String SUPERCLASS_FULLY_QUALIFIED_NAME = "st/redline/smalltalk/RObject";
	private static final String UNARY_SELECTOR = "unarySelector";
	private static final String UNARY_METHOD_DESCRIPTOR = "(Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;";
	private static final String BINARY_SELECTOR = "+";
	private static final String BINARY_METHOD_DESCRIPTOR = "(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;";
	private static final String SMALLTALK_CLASS = "st/redline/smalltalk/Smalltalk";
	private static final String KEYWORD_SELECTOR = "at:put:";
	private static final int KEYWORD_ARGUMENT_COUNT = 2;
	private static final String KEYWORD_METHOD_DESCRIPTOR =
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;";
	private static final String PRIMITIVE_BY_NUMBER_METHOD_DESCRIPTOR =
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;)Lst/redline/smalltalk/RObject;";
	private static final int LINE_NUMBER= 42;
	private static final int ARGUMENT_COUNT = 2;
	private static final int TEMPORARIES_COUNT = 3;

	@Mock ClassWriter classWriter;
	@Mock MethodVisitor methodVisitor;

	private Generator generator;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		generator = new Generator(false);
		generator.initialize(classWriter);
		when(classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)).thenReturn(methodVisitor);
		generator.openClass(CLASS_NAME, PACKAGE_INTERNAL_NAME);
	}

	@Test public void shouldWriteClassAndInitMethodWhenClassOpened() {
		verify(classWriter).visit(V1_5, ACC_PUBLIC + ACC_SUPER, CLASS_FULLY_QUALIFIED_NAME, null, SUPERCLASS_FULLY_QUALIFIED_NAME, null);
		verify(classWriter).visitSource("Model.st", null);
		verify(methodVisitor).visitCode();
		verifyInvokeOfSuperclassInitMethod();
	}

	@Test public void shouldEndWritingClassAndInitMethodWhenClassClosed() {
		generator.closeClass();
		verifyInitMethodClosed();
		verify(classWriter).visitEnd();
	}

	@Test public void shouldGenerateClassLookup() {
		generator.classLookup(CLASS_NAME, LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitLdcInsn(CLASS_NAME);
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "primitiveAt", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGenerateUnarySend() {
		generator.unarySend(UNARY_SELECTOR, LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verify(methodVisitor).visitLdcInsn(UNARY_SELECTOR);
		verify(methodVisitor).visitMethodInsn(INVOKESTATIC, CLASS_FULLY_QUALIFIED_NAME, "send", UNARY_METHOD_DESCRIPTOR);
	}

	@Test public void shouldGenerateBinarySend() {
		generator.binarySend(BINARY_SELECTOR, LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verify(methodVisitor).visitLdcInsn(BINARY_SELECTOR);
		verify(methodVisitor).visitMethodInsn(INVOKESTATIC, CLASS_FULLY_QUALIFIED_NAME, "send", BINARY_METHOD_DESCRIPTOR);
	}

	@Test public void shouldGeneratePrimitiveNumberConversion() {
		generator.primitiveNumberConversion("64", LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitLdcInsn("64");
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "numberFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGeneratePrimitiveStringConversion() {
		generator.primitiveStringConversion("'a-string'", LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitLdcInsn("a-string");
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "stringFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGeneratePrimitiveCharacterConversion() {
		generator.primitiveCharacterConversion("$a", LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitLdcInsn("a");
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "characterFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldHandleStringConversionOfEmptyString() {
		generator.primitiveStringConversion("''", LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitLdcInsn("");
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "stringFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGeneratePrimitiveSymbolConversion() {
		generator.primitiveSymbolConversion("symbol", LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitLdcInsn("symbol");
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "symbolFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGeneratePrimitiveStringSymbolConversion() {
		generator.primitiveSymbolConversion("'symbol'", LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitLdcInsn("symbol");
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "symbolFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldHandleEmptyStringSymbolConversion() {
		generator.primitiveSymbolConversion("''", LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitLdcInsn("");
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "symbolFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGenerateKeywordSend() {
		generator.keywordSend(KEYWORD_SELECTOR, KEYWORD_ARGUMENT_COUNT, LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verify(methodVisitor).visitLdcInsn(KEYWORD_SELECTOR);
		verify(methodVisitor).visitMethodInsn(INVOKESTATIC, CLASS_FULLY_QUALIFIED_NAME, "send", KEYWORD_METHOD_DESCRIPTOR);
	}

	@Test public void shouldGeneratePushOfReceiver() {
		generator.pushReceiver();
		verify(methodVisitor).visitVarInsn(ALOAD, 1); // receiver/self is always first argument.
	}

	@Test public void shouldGenerateInstanceMethodBinding() {
		reset(methodVisitor);
		generator.methodBinding(CLASS_NAME, UNARY_SELECTOR, CLASS_NAME + "_" + UNARY_SELECTOR, false);
		verifyMethodBindingCall(false);
	}

	@Test public void shouldGenerateClassMethodBinding() {
		reset(methodVisitor);
		generator.methodBinding(CLASS_NAME, UNARY_SELECTOR, CLASS_NAME + "_" + UNARY_SELECTOR, true);
		verifyMethodBindingCall(true);
	}

	@Test public void shouldGenerateTrueLookup() {
		generator.trueLookup(LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "trueInstance", "()Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGenerateFalseLookup() {
		generator.falseLookup(LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "falseInstance", "()Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGenerateNilLookup() {
		generator.nilLookup(LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "nilInstance", "()Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGenerateArrayCreation() {
		generator.createArray(LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verifySmalltalkInstanceFetch();
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "arrayInstance", "()Lst/redline/smalltalk/RObject;");
	}

	@Test public void shouldGenerateCallToPrimitiveByNumber() {
		int primitiveResultLocalVariableIndex = ARGUMENT_COUNT + 2 + TEMPORARIES_COUNT; // +2 because 0 = this and 1 = receiver.
		generator.callToPrimitiveByNumber(ARGUMENT_COUNT, TEMPORARIES_COUNT, "32", LINE_NUMBER);
		verify(methodVisitor, times(3)).visitLabel((Label) notNull());
		verify(methodVisitor).visitLineNumber(eq(LINE_NUMBER), (Label) notNull());
		verify(methodVisitor).visitVarInsn(ALOAD, 1);   // Nb - index 1 is 'receiver' passed to method.
		verify(methodVisitor).visitVarInsn(ALOAD, 2);   // first argument
		verify(methodVisitor).visitVarInsn(ALOAD, 3);   // second argument
		verify(methodVisitor, times(8)).visitInsn(ACONST_NULL);   // ... upto 10 arguments.
		verify(methodVisitor).visitMethodInsn(INVOKESTATIC, CLASS_FULLY_QUALIFIED_NAME, "primitive_32", PRIMITIVE_BY_NUMBER_METHOD_DESCRIPTOR);
		// Store result of primitive call into local variable.
		verify(methodVisitor).visitVarInsn(ASTORE, primitiveResultLocalVariableIndex);
		// Do a "if (result != null) return result;"
		verify(methodVisitor, times(2)).visitVarInsn(ALOAD, primitiveResultLocalVariableIndex);
		verify(methodVisitor).visitJumpInsn(eq(IFNULL), (Label) notNull());
		verify(methodVisitor).visitInsn(ARETURN);
		// fixup frame
		verify(methodVisitor).visitLineNumber(eq(LINE_NUMBER + 1), (Label) notNull());
		verify(methodVisitor).visitFrame(Opcodes.F_APPEND, 1, new Object[] {"st/redline/smalltalk/RObject"}, 0, null);
	}

	@Test public void shouldGenerateCallToPrimitiveByString() {
		generator.callToPrimitiveByString(ARGUMENT_COUNT, 2, "string", LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
	}

	@Test public void shouldGenerateCallToPrimitiveByModule() {
		generator.callToPrimitiveByModule(ARGUMENT_COUNT, 2, "string", "module", LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
	}

	private void verifyMethodBindingCall(boolean isClassMethod) {
		verify(methodVisitor).visitLdcInsn(CLASS_NAME);
		verify(methodVisitor).visitLdcInsn(UNARY_SELECTOR);
		verify(methodVisitor).visitLdcInsn(CLASS_NAME + "_" + UNARY_SELECTOR);
		if (isClassMethod)
			verify(methodVisitor).visitInsn(ICONST_1);
		else
			verify(methodVisitor).visitInsn(ICONST_0);
		verify(methodVisitor).visitMethodInsn(INVOKESTATIC, CLASS_FULLY_QUALIFIED_NAME, "bindMethod", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Z)V");
	}

	@Test (expected=IllegalArgumentException.class)
	public void shouldGenerateExceptionWhenMoreThanTenKeywordArguments() {
		generator.keywordSend(KEYWORD_SELECTOR, 12, LINE_NUMBER);
	}

	@Test (expected=IllegalArgumentException.class)
	public void shouldGenerateExceptionWhenMoreThanTenMethodArguments() {
		generator.openMethod(12);
	}

	private void verifyLineNumber(int lineNumber) {
		verify(methodVisitor).visitLabel((Label) notNull());
		verify(methodVisitor).visitLineNumber(eq(lineNumber), (Label) notNull());
	}

	private void verifyInvokeOfSuperclassInitMethod() {
		verify(methodVisitor).visitVarInsn(ALOAD, 0);
		verify(methodVisitor).visitMethodInsn(INVOKESPECIAL, SUPERCLASS_FULLY_QUALIFIED_NAME, "<init>", "()V");
	}

	private void verifyInitMethodClosed() {
		verify(methodVisitor).visitInsn(RETURN);
		verify(methodVisitor).visitMaxs(1, 1);
		verify(methodVisitor).visitEnd();
	}

	private void verifySmalltalkInstanceFetch() {
		// Note: im not sure wht times(2) is required here!
		verify(methodVisitor, times(2)).visitVarInsn(ALOAD, 0);
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, CLASS_FULLY_QUALIFIED_NAME, "smalltalk", "()Lst/redline/smalltalk/Smalltalk;");
	}
}
