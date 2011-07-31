package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class ClassSubclassMethod extends ProtoMethod {

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject argument) {
		ProtoObject subclass = ProtoObject.primitiveCreateSubclass(receiver);
		ProtoObject.primitiveRegisterAs(subclass, fullyQualifiedClassName(argument));
		return subclass;
	}

	private String fullyQualifiedClassName(ProtoObject argument) {
		String name = String.valueOf(argument.javaValue());
		String packageName = ProtoObject.primitivePackageRegistryCurrent();
		return packageName == null ? name : packageName + "." + name;
	}
}
