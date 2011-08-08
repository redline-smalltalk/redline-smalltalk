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

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline.compiler;

public class MethodBytecodeWriter extends ClassBytecodeWriter {

	private static final String SUPERCLASS = "st/redline/ProtoMethod";
	private static final String[] APPLY_TO_SIGNATURES = {
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
	};

	private final int countOfArguments;

	public MethodBytecodeWriter(String className, String packageName, int countOfArguments) {
		super(className, packageName);
		System.out.println("MethodBytecodeWriter() " + className + " " + packageName + " " + countOfArguments);
		this.countOfArguments = countOfArguments;
	}

	protected String superclass() {
		return SUPERCLASS;
	}

	protected void openApplyToMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, "applyTo", APPLY_TO_SIGNATURES[countOfArguments], null, null);
		mv.visitCode();
	}

	protected void writeInitializeMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, INIT, INIT_SIGNATURE, null, null);
		mv.visitCode();
		stackPushThis();
		mv.visitMethodInsn(INVOKESPECIAL, superclass(), INIT, INIT_SIGNATURE);
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();
	}
}
