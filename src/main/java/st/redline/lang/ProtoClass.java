/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

import java.util.concurrent.atomic.AtomicLongFieldUpdater;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

public class ProtoClass extends ProtoObject {

    static volatile Object CLASS_MODIFICATIONS = new Object();  // for guarding caches against class mutation

    public ProtoObject superclass = PrimNil.PRIM_NIL;
    protected ProtoObject thisclass;
    public Map<String, ProtoObject> methods = new HashMap<String, ProtoObject>();
    protected Map<String, String> imports;
    protected Map<String, Integer> variableIndexes;

    private ProtoObject sendMessagesResult;

    public String toString() {
        return String.valueOf(thisclass != null ? thisclass.name + "-Meta" : name) + "@" + hashCode();
    }

    public ProtoClass create(String name) {
        ProtoClass metaclass = new ProtoClass();
        metaclass.name = name;
        metaclass.selfclass = this;
        this.thisclass = metaclass;
        return metaclass;
    }

    public ProtoClass subclass() {
        ProtoClass subclass = new ProtoClass();
        subclass.superclass = this;
        return subclass;
    }

    protected ProtoObject superclass() {
        return superclass;
    }

    public void superclass(ProtoObject aClass) {
        superclass = aClass;
    }

    public boolean includesSelector(String selector) {
        return methods.containsKey(selector);
    }

    public ProtoObject methodFor(String selector) {
        return methods.get(selector);
    }

    public void addMethod(String selector, ProtoObject method) {
        methods.put(selector, method);
    }

    public void sendMessagesResult(ProtoObject result) {
        sendMessagesResult = result;
    }

    public ProtoObject sendMessagesResult() {
        return sendMessagesResult;
    }

    public int indexOfVariable(String name) {
        if (variableIndexes != null) {
            Integer index = variableIndexes.get(name);
            if (index != null)
                return index;
        }
        if (superclass != null)
            return superclass.indexOfVariable(name);
        return 0;  // Smalltalk indexes can't be zero (0).
    }

    public void importAtPut(String className, String fullyQualifiedName) {
        if (imports == null)
            imports = new Hashtable<String, String>();
        if (imports.containsKey(className)) {
            if (!imports.get(className).equals(fullyQualifiedName))
                throw new IllegalStateException("'" + className + "' already registered to package: '" + imports.get(className) + "'.");
        } else
            imports.put(className, fullyQualifiedName);
    }

    public String importFor(String name) {
        String fullyQualifiedName;
        if (imports != null && (fullyQualifiedName = imports.get(name)) != null) {
            return fullyQualifiedName;
        }
        if (superclass != null && (fullyQualifiedName = superclass.importFor(name)) != null) {
            return fullyQualifiedName;
        }
        return null;
    }

    public void methodAtPut(String selector, ProtoObject method) {
        methods.put(selector, method);
    }
}
