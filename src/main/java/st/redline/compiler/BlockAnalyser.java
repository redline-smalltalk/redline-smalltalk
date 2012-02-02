/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BlockAnalyser extends ProgramAnalyser implements AnalyserDelegate {

	private Block thisBlock;

	BlockAnalyser(Analyser analyser, String className, String packageName, boolean verbose, Block block) {
		this(analyser, new BlockBytecodeWriter(className, packageName, verbose), verbose, block);
	}

	BlockAnalyser(Analyser analyser, ClassBytecodeWriter classBytecodeWriter, boolean verbose, Block block) {
		super(analyser, classBytecodeWriter, verbose);
		this.thisBlock = block;
	}

	public void visitBegin(Block block, int line) {
		if (block == thisBlock)
			writer.openClass();
		else
			throw new IllegalStateException("Handle visiting new block.");
	}

	public void visitEnd(Block block, int line) {
		if (block != thisBlock)
			throw new IllegalStateException("Expected visitEnd of own block.");
		writer.closeClass();
		analyser.previousDelegate();
	}
}
