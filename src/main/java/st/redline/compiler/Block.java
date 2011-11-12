/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class Block extends BasePrimary {

	private final List<BlockVariableName> blockVariableNames;
	private Temporaries temporaries;
	private Statements statements;
	private Analyser analyser;

	public Block() {
		this.blockVariableNames = new ArrayList<BlockVariableName>();
	}

	public void add(BlockVariableName blockVariableName) {
		blockVariableNames.add(blockVariableName);
	}

	public void add(Temporaries temporaries) {
		this.temporaries = temporaries;
	}

	public void add(Statements statements) {
		this.statements = statements;
	}

	public boolean hasStatements() {
		return statements != null;
	}

	public int argumentCount() {
		return blockVariableNames.size();
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		// we don't visit the rest during class / method analysis, but we do during block analysis.
		if (!visitor.continueBlockVisit())
			return;
		for (BlockVariableName blockVariableName : blockVariableNames)
			blockVariableName.accept(visitor);
		if (temporaries != null)
			temporaries.accept(visitor);
		if (statements != null)
			statements.accept(visitor);
		visitor.visitEnd(this);
	}

	// analyser is passed through to Block compilation.

	public Analyser analyser() {
		return analyser;
	}

	public void analyser(Analyser analyser) {
		this.analyser = analyser;
	}
}
