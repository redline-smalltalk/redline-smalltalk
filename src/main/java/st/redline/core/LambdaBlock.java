package st.redline.core;

public interface LambdaBlock {

    PrimObject apply(PrimObject thiz, PrimObject receiver, PrimContext context);
}
