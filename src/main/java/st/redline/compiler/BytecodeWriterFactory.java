/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;

import java.io.PrintWriter;

public class BytecodeWriterFactory {

    private final boolean traceBytecode;

    public BytecodeWriterFactory(boolean traceBytecode) {
        this.traceBytecode = traceBytecode;
    }

    public ClassBytecodeWriter createClassBytecodeWriter(String className, String packageName) {
        ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        ClassVisitor classVisitor = createVisitor(classWriter);
        return new ClassBytecodeWriter(classVisitor, classWriter, className, packageName);
    }

    private ClassVisitor createVisitor(ClassWriter classWriter) {
        return traceBytecode ? tracingClassVisitor(classWriter) : nonTracingClassVisitor(classWriter);
    }

    private ClassWriter nonTracingClassVisitor(ClassWriter classWriter) {
        return classWriter;
    }

    private ClassVisitor tracingClassVisitor(ClassWriter classWriter) {
        return new TracingClassVisitor(classWriter, new PrintWriter(System.out));
    }

    public BlockBytecodeWriter createBlockBytecodeWriter(String className, String packageName) {
        ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        ClassVisitor classVisitor = createVisitor(classWriter);
        return new BlockBytecodeWriter(classVisitor, classWriter, className, packageName);
    }
}
