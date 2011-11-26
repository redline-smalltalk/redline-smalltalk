/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import org.easymock.EasyMockSupport;
import org.junit.Test;
import st.redline.ProtoBlock;
import st.redline.ProtoObject;
import st.redline.stout.Block;
import st.redline.stout.RequestPathSpecification;
import st.redline.stout.ResponseSerializer;
import st.redline.stout.RouterImpl;

import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static org.easymock.EasyMock.expect;
import static org.junit.Assert.assertEquals;

public class RouterImplTest extends EasyMockSupport {

	// TODO - uncomment and fix test.
//    @Test
//    public void shouldBeAbleToDispatchToBlockWithParameters() throws Exception {
//        String requestPath = createRandomString();
//        String serializedReturnedValue = createRandomString();
//        ProtoObject returnedValue = new ProtoObject();
//        Map<String, String> parameters = createMapWithRandomValues();
//        RequestPathSpecification requestPathSpecification = createMock(RequestPathSpecification.class);
//        ResponseSerializer responseSerializer = createMock(ResponseSerializer.class);
//        ProtoBlock block = createMock(ProtoBlock.class);
//        HttpServletResponse servletResponse = createMock(HttpServletResponse.class);
//        PrintWriter writer = createMock(PrintWriter.class);
//
//        expect(requestPathSpecification.parseParameters(requestPath)).andReturn(parameters);
//        expect(block.applyTo(null, null)).andReturn(returnedValue);
//        expect(responseSerializer.serialize(returnedValue)).andReturn(serializedReturnedValue);
//        expect(servletResponse.getWriter()).andReturn(writer);
//        writer.write(serializedReturnedValue);
//        writer.close();
//        replayAll();
//
//        RouterImpl router = new RouterImpl(responseSerializer, block, requestPathSpecification);
//
//        router.dispatchToBlock(servletResponse, requestPath);
//
//        verifyAll();
//
//    }

    @Test
    public void shouldBeAbleToCheckWhetherRequestPathIsMatchingWithSpecification() {
        String requestPath = createRandomString();
        boolean canHandleRequest = true;

        RequestPathSpecification requestPathSpecification = createMock(RequestPathSpecification.class);

        expect(requestPathSpecification.isPathMatching(requestPath)).andReturn(canHandleRequest);

        replayAll();

        RouterImpl router = new RouterImpl(null, null, requestPathSpecification);
        assertEquals(canHandleRequest, router.canHandleRequest(requestPath));

        verifyAll();
    }

    private String createRandomString() {
        return UUID.randomUUID().toString();
    }

    private HashMap<String, String> createMapWithRandomValues() {
        HashMap<String, String> map = new HashMap<String, String>();
        map.put(createRandomString(), createRandomString());
        return map;
    }
}
