package st.redline.compiler;

import org.objectweb.asm.Opcodes;
import st.redline.ClassPathUtilities;
import st.redline.PrimObject;
import st.redline.RedlineException;

import java.util.HashMap;
import java.util.Map;

public class BlockReturnTypeCreator implements Opcodes {

    private final static Map<String, String> registry = new HashMap<String, String>();

    private final String fullyQualifiedClassName;

    public BlockReturnTypeCreator(String className, String packageName) {
        fullyQualifiedClassName = ClassPathUtilities.classNameToFullyQualifiedClassName(packageName, className);
    }

    void create() {
        System.out.println("***********************");
        System.out.println("BlockReturnTypeCreator creating: " + fullyQualifiedClassName);
        if (registry.containsKey(fullyQualifiedClassName))
            return;
        registry.put(fullyQualifiedClassName, fullyQualifiedClassName);
        loadClass(createClass());
    }

    private void loadClass(byte[] aClass) {
        try {
            PrimObject.smalltalkClassLoader().defineClass(aClass).newInstance();
        } catch (Exception e) {
            throw new RedlineException(e);
        }
    }

    private byte[] createClass() {
        return null;
    }
}
