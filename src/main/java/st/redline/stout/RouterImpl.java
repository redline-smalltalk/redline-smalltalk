/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.PrimObject;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Map;

public class RouterImpl implements Router {

    private ResponseSerializer responseSerializer;
    private String acceptType;
    private PrimObject block;
    private RequestPathSpecification requestPathSpecification;

    public RouterImpl(ResponseSerializer responseSerializer, String acceptType, PrimObject block,
                      RequestPathSpecification requestPathSpecification) {
        this.responseSerializer = responseSerializer;
        this.acceptType = acceptType;
        this.block = block;
        this.requestPathSpecification = requestPathSpecification;
    }

    public void dispatchToBlock(PrimObject servletResponse, PrimObject requestPath) throws IOException {
        dispatchToBlock((HttpServletResponse) servletResponse.javaValue(), (String) requestPath.javaValue());
    }

    public void dispatchToBlock(HttpServletResponse servletResponse, String requestPath) throws IOException {
        System.out.println("dispatchToBlock() " + servletResponse + " " + requestPath);
        Map<String, String> parameters = retrieveParametersAccordingToSpecification(requestPath);
        PrimObject responseValue = invokeTargetBlock(parameters);
        serializeResponseOn(responseValue, servletResponse);
    }

    public boolean canHandleRequest(String requestPath, String acceptType) {
        return requestPathSpecification.isPathMatching(requestPath)
                && acceptType.indexOf(this.acceptType) != -1;
    }

    private void serializeResponseOn(PrimObject response, HttpServletResponse servletResponse) throws IOException {
        Writer writer = null;
        try {
            writer = servletResponse.getWriter();
            serializeResponseOn(response, writer);
        } finally {
            if (writer != null)
                writer.close();
        }
    }

    private void serializeResponseOn(PrimObject response, Writer writer) throws IOException {
        try {
            responseSerializer.serializeOn(response, writer);
        } catch (Exception e) {
            e.printStackTrace(new PrintWriter(writer));
        }
    }

    private PrimObject invokeTargetBlock(Map<String, String> parameters) {
        System.out.println("invokeTargetBlock() " + parameters);
        PrimObject[] arguments = new PrimObject[parameters.size()];
        int index = 0;
        for (String value : parameters.values())
            arguments[index++] = PrimObject.string(value);
        switch (arguments.length) {
            case 0:
                return block.perform("value");
            case 1:
                return block.perform(arguments[0], "value:");
            case 2:
                return block.perform(arguments[0], arguments[1], "value:value:");
            case 3:
                return block.perform(arguments[0], arguments[1], arguments[2], "value:value:value:");
            case 4:
                return block.perform(arguments[0], arguments[1], arguments[2], arguments[3], "value:value:value:value:");
            case 5:
                return block.perform(arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], "value:value:value:value:value:");
            default:
                throw new IllegalStateException("invokeTargetBlock: Can't handle parameter count.");
        }
    }

    private Map<String, String> retrieveParametersAccordingToSpecification(String requestPath) {
        return requestPathSpecification.parseParameters(requestPath);
    }
}
