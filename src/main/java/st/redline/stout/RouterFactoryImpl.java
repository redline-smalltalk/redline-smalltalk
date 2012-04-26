/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.PrimObject;
import st.redline.PrimObjectBlock;

import java.io.IOException;
import java.io.Writer;

public class RouterFactoryImpl implements RouterFactory {

    private RequestPathSpecificationFactory requestPathSpecificationFactory;

    private static final ResponseSerializer TO_STRING_SERIALIZER = new ResponseSerializer() {
        public void serializeOn(PrimObject object, Writer writer) throws IOException {
            writer.write(object.perform("asString").toString());
        }
    };

    private static final ResponseSerializer JSON_SERIALIZER = new ResponseSerializer() {

        public void serializeOn(PrimObject object, Writer writer) throws IOException {
            object.perform(createStreamOn(writer), "storeOn:");
        }

        private PrimObject createStreamOn(Writer writer) {
            PrimObject stream = newJSONStream();
            stream.javaValue(writer);
            return stream;
        }

        private PrimObject newJSONStream() {
            return PrimObject.CLASSES.get("st.redline.stout.JSONStream").perform("new");
        }
    };

    public RouterFactoryImpl(RequestPathSpecificationFactory requestPathSpecificationFactory) {
        this.requestPathSpecificationFactory = requestPathSpecificationFactory;
    }

    /*
      type parameter should be used to determine which implementation of the st.redline.stout.ResponseSerializer to be used.
     */
    public Router create(String requestPathSpec, String type, PrimObject block) {
        if (type.equalsIgnoreCase("application/json"))
            return new RouterImpl(JSON_SERIALIZER, type, block, requestPathSpecificationFactory.create(requestPathSpec));
        return new RouterImpl(TO_STRING_SERIALIZER, type, block, requestPathSpecificationFactory.create(requestPathSpec));
    }
}
