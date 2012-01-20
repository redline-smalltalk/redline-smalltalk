/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class Block extends Primary {

	private final int line;
	private final BlockArguments blockArguments;
	private final Temporaries temporaries;
	private final Statements statements;

	Block(int line, BlockArguments blockArguments, Temporaries temporaries, Statements statements) {
		this.line = line;
		this.blockArguments = blockArguments;
		this.temporaries = temporaries;
		this.statements = statements;
	}

	Statements statements() {
		return statements;
	}

	int line() {
		return line;
	}

	BlockArguments arguments() {
		return blockArguments;
	}

	Temporaries temporaries() {
		return temporaries;
	}
}
