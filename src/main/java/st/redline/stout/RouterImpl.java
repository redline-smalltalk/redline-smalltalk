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
        System.out.println("dispatchToBlock()" + servletResponse + " " + requestPath);
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
	    System.out.println("**** TODO: ADD parameters to block value call ****");
        return block.perform("value");
    }

    private Map<String, String> retrieveParametersAccordingToSpecification(String requestPath) {
        return requestPathSpecification.parseParameters(requestPath);
    }
}
