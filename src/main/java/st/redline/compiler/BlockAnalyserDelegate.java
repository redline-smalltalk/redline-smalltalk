/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BlockAnalyserDelegate extends MethodAnalyser {

	private Block thisBlock;

	public BlockAnalyserDelegate(String className, String packageName, int countOfArguments, boolean isClassMethod, Analyser containingAnalyser, Block thisBlock) {
		super(className, packageName, countOfArguments, isClassMethod, containingAnalyser);
		this.thisBlock = thisBlock;
//		System.out.println("BlockAnalyser " + className);
	}

	protected void initialize() {
		classBytecodeWriter = new BlockBytecodeWriter(className, packageName, countOfArguments);
	}

	public boolean continueBlockVisit() {
		return true;
	}

	public void visit(Block block) {
//		System.out.println("BlockAnalyser.Block() Analysis begin " + block + " " + thisBlock);
		if (block == thisBlock)
			classBytecodeWriter.openClass();
		else
			super.visit(block);
	}

	public void visitEnd(Block block) {
		if (block == thisBlock) {
			if (!block.hasStatements())
				classBytecodeWriter.stackPushNil(block.line());
			classBytecodeWriter.closeClass();
		} else {
			super.visitEnd(block);
		}
	}

	public void visit(BlockVariableName blockVariableName, String value, int line) {
//		System.out.println("Blockvisit(BlockVariableName) " + value);
		registerMethodArgument(blockVariableName);
	}

	protected boolean isMethodArgument(String value) {
//		System.out.println("BlockAnalyser.isMethodArgument() " + value + " " + super.isMethodArgument(value));
		return super.isMethodArgument(value);
	}
}
