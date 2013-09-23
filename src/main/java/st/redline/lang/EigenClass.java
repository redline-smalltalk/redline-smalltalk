/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

import st.redline.RedlineFile;

public class EigenClass extends ProtoClass {

    private String packageName;
    private String className;

    public void initialize(String className, String packagePath) {
        this.className = className;
        this.packageName = packagePath.replace(RedlineFile.separator, ".");
        initializeHierarchy();
        register();
    }

    private void initializeHierarchy() {
        try {
            ProtoObject protoObject = classLoader().loadSmalltalkClass("ProtoObject");
            ProtoObject protoObjectMetaclass = protoObject.selfclass;
            ProtoClass eigenMetaclass = new EigenClass();
            eigenMetaclass.superclass = protoObjectMetaclass;
            eigenMetaclass.thisclass = this;
            this.name = "Eigen" + className;
            this.superclass = protoObject;
            this.selfclass = eigenMetaclass;
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private void register() {
        classLoader().currentEigenClass(this);
    }

    public String packageName() {
        return packageName;
    }

    public void uninitialize() {
        deregister();
    }

    private void deregister() {
        classLoader().removeEigenClass();
    }

    protected ProtoObject _sendMessages_(ProtoObject receiver, PrimContext context) {
        return this;
    }
}
