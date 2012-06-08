/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class BaseSignatureBuilder {

    private static final Map<String, String> TYPE_MAP = new HashMap<String, String>();
    static {
        TYPE_MAP.put(BigDecimal.class.getName(), "Integer");
        TYPE_MAP.put(String.class.getName(), "String");
    }
    private final ArrayList<PrimObject> args;
    private final StringBuilder signature = new StringBuilder();

    public BaseSignatureBuilder(String prefix, ArrayList<PrimObject> args) {
        signature.append(prefix);
        this.args = args;
    }

    public String build() {
        for (int i = 1; i < args.size(); i++)  // skip slot 0, not used in Smalltalk - indexes from 1.
            appendSignaturePart(args.get(i));
        signature.append(':');
        return signature.toString();
    }

    private void appendSignaturePart(PrimObject arg) {
        String name = arg.javaValue().getClass().getName();
        signature.append(TYPE_MAP.get(name));
    }
}
