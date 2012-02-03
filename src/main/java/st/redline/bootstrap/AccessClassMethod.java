package st.redline.bootstrap;

import st.redline.PrimContext;
import st.redline.PrimObject;
import st.redline.PrimObjectClass;

public class AccessClassMethod extends PrimObject {
	public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
		return receiver.cls();
	}
}
