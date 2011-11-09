/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BlockBytecodeWriter extends MethodBytecodeWriter {

	private static final String SUPERCLASS = "st/redline/ProtoBlock";

	public BlockBytecodeWriter(String className, String packageName, int countOfArguments) {
		super(className, packageName, countOfArguments);
//		System.out.println("BlockBytecodeWriter() " + className + " " + packageName + " " + countOfArguments);
	}

	protected String superclass() {
		return SUPERCLASS;
	}
}
