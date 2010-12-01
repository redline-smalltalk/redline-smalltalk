package st.redline.compiler;

public class PrimaryBlock extends Primary {

	private final Block block;

	public PrimaryBlock(Block block) {
		this.block = block;
	}

	public Block block() {
		return block;
	}

	public boolean hasBlockWithAnswer() {
		return block.hasAnsweredExpression();
	}
}
