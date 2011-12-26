/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BlockAnalyserDelegate extends MethodAnalyser {

	protected final Block thisBlock;
	protected final BlockAnalyser blockAnalyser;

	public BlockAnalyserDelegate(BlockAnalyser blockAnalyser, String className, String packageName, int countOfArguments, boolean isClassMethod, Analyser containingAnalyser, Block thisBlock) {
		super(className, packageName, countOfArguments, isClassMethod, containingAnalyser);
		this.thisBlock = thisBlock;
		this.blockAnalyser = blockAnalyser;
//		System.out.println("BlockAnalyserDelegate " + className + " " + thisBlock);
	}

	protected void initialize() {
		classBytecodeWriter = new BlockBytecodeWriter(className, packageName, countOfArguments);
	}

	public boolean continueBlockVisit() {
		return true;
	}

	public void visit(AnswerExpression answerExpression) {
//		System.out.println("BlockAnalyserDelegate.visit(AnswerExpression)");
		super.visit(answerExpression);
	}

	public void visitEnd(AnswerExpression answerExpression) {
//		System.out.println("BlockAnalyserDelegate.visitEnd(AnswerExpression)");
		classBytecodeWriter.callPrimitiveBlockAnswer();
	}

	public void visit(Block block) {
//		System.out.println("BlockAnalyserDelegate.Block() Analysis begin " + block + " " + thisBlock);
		if (block == thisBlock)
			classBytecodeWriter.openClass();
		else {
			// visiting block within a block, so we want to tell it to compile and then
			// ignore its internals - until the block ends.
			super.visit(block);
			blockAnalyser.useNoOpDelegate();
		}
	}

	public void visitEnd(Block block) {
//		System.out.println("BlockAnalyserDelegate.Block() Analysis end " + block + " " + thisBlock);
		if (block == thisBlock) {
			if (!block.hasStatements())
				classBytecodeWriter.stackPushNil(block.line());
			classBytecodeWriter.closeClass();
		} else {
			super.visitEnd(block);
		}
	}

	public void visit(SelfReservedWord selfReservedWord, int line) {
//		System.out.println("visit(Self) in block ");
		classBytecodeWriter.stackPushOuterReceiver(line);
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
