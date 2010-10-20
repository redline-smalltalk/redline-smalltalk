package st.redline;

import java.lang.reflect.Method;

public abstract class JavaObject {

	protected abstract ProtoObject clas$();

	protected Method findMethod(String selector) {
		return null;
	}
}