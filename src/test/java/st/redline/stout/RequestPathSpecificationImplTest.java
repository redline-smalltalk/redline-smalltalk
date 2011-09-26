/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import org.junit.Test;
import st.redline.stout.RequestPathSpecificationImpl;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

public class RequestPathSpecificationImplTest {

    @Test
    public void shouldBeAbleToSeeRequestPathMatchesSpecification() {
        String specification = "/users\\/:id/:account_number";
        String requestMatchesSpecification = "/users\\/23/HOM1234645";

        RequestPathSpecificationImpl requestSpecification = new RequestPathSpecificationImpl(specification);

        assertTrue(requestSpecification.isPathMatching(requestMatchesSpecification));
    }

    @Test
    public void shouldBeAbleToSeeRequestPathOnlyMatchesThePrefixOfSpecification() {
        String specification = "/users\\/:id/:account_number";
        String request = "/users\\/23/";

        RequestPathSpecificationImpl requestSpecification = new RequestPathSpecificationImpl(specification);

        assertFalse(requestSpecification.isPathMatching(request));
    }

    @Test
    public void shouldBeAbleToSeeRequestPathOnlyMatchesTheSuffixOfSpecification() {
        String specification = "/users\\/:id/:account_number";
        String request = "/u/23/21";

        RequestPathSpecificationImpl requestSpecification = new RequestPathSpecificationImpl(specification);

        assertFalse(requestSpecification.isPathMatching(request));
    }

    @Test
    public void shouldBeAbleToParseParametersFromRequest() {
        String specification = "/us\\ers/:id/:account_number";
        String request = "/us\\ers/5/234H";

        Map<String, String> expectedParameters = new HashMap<String, String>();
        expectedParameters.put("id", "5");
        expectedParameters.put("account_number", "234H");

        RequestPathSpecificationImpl requestSpecification = new RequestPathSpecificationImpl(specification);

        assertEquals(expectedParameters, requestSpecification.parseParameters(request));
    }
}
