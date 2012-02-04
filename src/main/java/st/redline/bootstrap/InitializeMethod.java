package st.redline.bootstrap;

import st.redline.PrimContext;
import st.redline.PrimObject;

public class InitializeMethod extends PrimObject {
	public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
		return receiver;
	}
}
