package st.redline.core;

public class PrimMethod extends PrimObject {

    public PrimMethod(LambdaBlock lambdaBlock) {
        this.javaValue(lambdaBlock);
    }

    public String toString() {
        return "(PrimMethod) " + javaValue().toString();
    }

    protected PrimObject invoke(PrimObject receiver, PrimContext context) {
        System.out.println("PrimMethod invoke");
        // We send receiver as first _and_ second argument as LambdaBlock's are static
        // and we need receiver in argument slot 1
        return ((LambdaBlock) javaValue()).apply(receiver, receiver, context);
    }
}
