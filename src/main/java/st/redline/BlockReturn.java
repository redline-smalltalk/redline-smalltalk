/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class BlockReturn extends RuntimeException {

	private ProtoObject answer = ProtoObject.NIL;

	public BlockReturn(ProtoObject answer) {
		System.out.println("BlockReturn() " + answer);
		if (answer != null)
			this.answer = answer;
	}

	public ProtoObject answer() {
		return answer;
	}
}
