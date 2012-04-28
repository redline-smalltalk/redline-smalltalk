/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import static org.mockito.Matchers.eq;

import org.mockito.ArgumentMatcher;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import st.redline.core.PrimObjectBlock;

import java.util.UUID;

import static org.junit.Assert.assertSame;

public class RouterRegistryImplTest {
    
    private RouterRegistryImpl routerRegistry;
    private RouterFactory routerFactory;

    @Test
    public void shouldBeAbleLookupRouterByDifferentPaths() {
        String spec1 = createRandomString();
        String expectedPathMatchesWithSpec1 = createRandomString();
        String spec2 = createRandomString();
        String expectedPathMatchesWithSpec2 = createRandomString();
        String type = createRandomString();
        String method = createRandomString();
        PrimObjectBlock block1 = mock(PrimObjectBlock.class);
        PrimObjectBlock block2 = mock(PrimObjectBlock.class);

        Router expectedRouterForSpec1 = expectCreateRouter(spec1, type, block1, expectedPathMatchesWithSpec1);
        Router expectedRouterForSpec2 = expectCreateRouter(spec2, type, block2, expectedPathMatchesWithSpec2);

        assertSame(expectedRouterForSpec1, routerRegistry.register(spec1, type, method, block1));
        assertSame(expectedRouterForSpec1, routerRegistry.lookup(expectedPathMatchesWithSpec1, method, type));

        assertSame(expectedRouterForSpec2, routerRegistry.register(spec2, type, method, block2));
        assertSame(expectedRouterForSpec2, routerRegistry.lookup(expectedPathMatchesWithSpec2, method, type));
    }

    @Test
    public void shouldBeAbleToLookupRouterByDifferentMethod() {
        String spec1 = createRandomString();
        String spec2 = createRandomString();
        String expectedPathMatchesWithSpec = createRandomString();
        String type = createRandomString();
        String method1 = createRandomString();
        String method2 = createRandomString();
        PrimObjectBlock block1 = mock(PrimObjectBlock.class);
        PrimObjectBlock block2 = mock(PrimObjectBlock.class);

        Router expectedRouterForMethod1 = expectCreateRouter(spec1, type, block1, expectedPathMatchesWithSpec);
        Router expectedRouterForMethod2 = expectCreateRouter(spec2, type, block2, expectedPathMatchesWithSpec);

        routerRegistry.register(spec1, type, method1, block1);
        assertSame(expectedRouterForMethod1, routerRegistry.lookup(expectedPathMatchesWithSpec, method1, type));

        routerRegistry.register(spec2, type, method2, block2);
        assertSame(expectedRouterForMethod2, routerRegistry.lookup(expectedPathMatchesWithSpec, method2, type));
    }

    @Test
    public void shouldHaveNoOpRouterForRequestThatDoNotHaveRouterRegistered() {
        assertSame(NoOpRouter.INSTANCE, routerRegistry.lookup(createRandomString(), createRandomString(), createRandomString()));
    }

    private Router expectCreateRouter(String spec, String type, PrimObjectBlock block, String expectedPathMatchesWithSpec) {
        Router router = mock(Router.class);

        when(routerFactory.create(spec, type, block)).thenReturn(router);
        when(router.canHandleRequest(expectedPathMatchesWithSpec, type)).thenReturn(true);
// JCL - not sure how to do this with mockito.
//        when(router.canHandleRequest(not expectedPathMatchesWithSpec)).thenReturn(false);

        return router;
    }

    private String createRandomString() {
        return UUID.randomUUID().toString();
    }

    @Before
    public void setUp() {
        routerFactory = mock(RouterFactory.class);
        routerRegistry = new RouterRegistryImpl(routerFactory);
    }

    @After
    public void tearDown() {
        routerRegistry = null;
    }

    class NotEquals extends ArgumentMatcher {
        private String value;
        public NotEquals(String value) {
            this.value = value;
        }
        public boolean matches(Object o) {
            if (o instanceof String)
                return !value.equals(o);
            return false;
        }
    }
}
