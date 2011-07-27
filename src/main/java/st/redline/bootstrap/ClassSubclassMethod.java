package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class ClassSubclassMethod extends ProtoMethod {
	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject argument) {
		System.out.println("ClassSubclassMethod applyTo() " + String.valueOf(argument.javaValue()));
		return null;
	}
}
