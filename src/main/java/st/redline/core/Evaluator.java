/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

public class Evaluator {

    public static PrimObject evaluate(PrimObject source) {
        return evaluate((String) source.javaValue());
    }

    public static PrimObject evaluate(String source) {
        Class cls = SmalltalkEnvironment.exposedClassLoader().classFrom(new InMemorySourceFile(source));
        try {
            return ((PrimObject) cls.newInstance()).sendMessagesResult();
        } catch (Exception e) {
            throw new RedlineException(e);
        }
    }
}
