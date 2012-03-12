/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.PrimObject;
import st.redline.PrimObjectBlock;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.Writer;
import java.util.Map;

public class RouterImpl implements Router {

    private ResponseSerializer responseSerializer;
    private PrimObject block;
    private RequestPathSpecification requestPathSpecification;

    public RouterImpl(ResponseSerializer responseSerializer, PrimObject block,
                      RequestPathSpecification requestPathSpecification) {
        this.responseSerializer = responseSerializer;
        this.block = block;
        this.requestPathSpecification = requestPathSpecification;
    }

    public void dispatchToBlock(PrimObject servletResponse, PrimObject requestPath) throws IOException {
        dispatchToBlock((HttpServletResponse) servletResponse.javaValue(), (String) requestPath.javaValue());
    }

    public void dispatchToBlock(HttpServletResponse servletResponse, String requestPath) throws IOException {
        System.out.println("dispatchToBlock() " + servletResponse + " " + requestPath);
        Map<String, String> parameters = retrieveParametersAccordingToSpecification(requestPath);
        Object responseValue = invokeTargetBlock(parameters);
        String response = serializeResponse(responseValue);
        sendClientResponse(servletResponse, response);
    }

    public boolean canHandleRequest(String requestPath) {
        return requestPathSpecification.isPathMatching(requestPath);
    }

    private void sendClientResponse(HttpServletResponse servletResponse, String response) throws IOException {
        Writer writer = servletResponse.getWriter();
        writer.write(response);
        writer.close();
    }

    private String serializeResponse(Object response) {
        return responseSerializer.serialize(response);
    }

    private Object invokeTargetBlock(Map<String, String> parameters) {
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
