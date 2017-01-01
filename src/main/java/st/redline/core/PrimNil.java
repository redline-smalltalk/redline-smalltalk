package st.redline.core;

import static st.redline.core.PrimDoesNotUnderstand.PRIM_DOES_NOT_UNDERSTAND;

// Essentially a PrimClass but with it's own Class name to make Java Debugging easier.

public class PrimNil extends PrimClass {

    public static final PrimNil PRIM_NIL = new PrimNil("PrimNil");

    public PrimNil(String name) {
        super(name);
    }

    public boolean includesSelector(String selector) {
        return true;
    }

    public PrimObject methodFor(String selector) {
        return PRIM_DOES_NOT_UNDERSTAND;
    }
}
