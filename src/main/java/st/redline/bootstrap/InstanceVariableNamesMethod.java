package st.redline.bootstrap;

import st.redline.core.PrimContext;
import st.redline.core.PrimObject;
import st.redline.core.PrimObjectClass;

public class InstanceVariableNamesMethod extends PrimObject {

    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
        PrimObjectClass aClass = (PrimObjectClass) receiver;
        for (String name : names(primContext).split(" "))
            aClass.addVariableNamed(name);
        return receiver;
    }

    String names(PrimContext primContext) {
        return (String) primContext.argumentAt(0).javaValue();
    }
}
