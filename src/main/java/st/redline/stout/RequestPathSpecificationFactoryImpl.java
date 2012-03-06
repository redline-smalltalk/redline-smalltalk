/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

public class RequestPathSpecificationFactoryImpl implements RequestPathSpecificationFactory {
    public RequestPathSpecification create(String specification) {
        return new RequestPathSpecificationImpl(specification);
    }
}
