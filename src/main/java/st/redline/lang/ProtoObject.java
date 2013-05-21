/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

import st.redline.classloader.SmalltalkClassLoader;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class ProtoObject {

    public ProtoObject selfclass = PrimNil.PRIM_NIL;
    protected String name;
    protected Object javaValue;
    protected ProtoObject[] attributes;
    protected boolean trueness = false;
    protected boolean falseness = false;

    protected ProtoObject superclass() {
        throw new IllegalStateException("This receiver should not have received this message.");
    }

    public String toString() {
        if (javaValue != null)
            return javaValue.toString();
        if (this.name != null)
            return String.valueOf(this.name) + "@" + hashCode();
        return String.valueOf(this.selfclass.name) + "@" + hashCode();
    }

    public Object javaValue() {
        return javaValue;
    }

    public void javaValue(Object value) {
        javaValue = value;
    }

    public boolean isMethodBlock() {
        return false;
    }

    public ProtoObject markTrueness() {
        trueness = true;
        return this;
    }

    public ProtoObject markFalseness() {
        falseness = true;
        return this;
    }

    public boolean isTrueObject() {
        return trueness;
    }

    public boolean isFalseObject() {
        return falseness;
    }

    public ProtoObject outerReceiver() {
        return this;
    }

    public ProtoObject nil() {
        return classLoader().NIL;
    }

    public SmalltalkClassLoader classLoader() {
        return (SmalltalkClassLoader) currentThread().getContextClassLoader();
    }

    public Thread currentThread() {
        return Thread.currentThread();
    }

    public ProtoObject resolveObject(String name) {
        SmalltalkClassLoader smalltalkClassLoader = classLoader();
        try {
            ProtoObject object = smalltalkClassLoader.loadSmalltalkClass(name, false);
            if (object != null)
                return object;
            if (Character.isUpperCase(name.charAt(0))) {
                String fullyQualifiedName = importFor(name);
                if (fullyQualifiedName != null) {
                    if (fullyQualifiedName.startsWith("#")) {
                        // fullyQualifiedName here is actually Java class name.
//                        return adaptorClassForJavaClassNamed(fullyQualifiedName.substring(1));
                        throw new IllegalStateException("TODO - JCL");
                    } else
                        return resolveObject(fullyQualifiedName);
                }
            }
            // It is expected the loading of an object results in the registering of
            // a Smalltalk class in the class registry.
            object = smalltalkClassLoader.loadSmalltalkClass(name, true);
            if (object != null)
                return smalltalkClassLoader.loadSmalltalkClass(name, false);
            throw new IllegalStateException("Error: Class '" + name + "' should have been resolved by here.");
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    public String importFor(String name) {
        if (selfclass != null)
            return selfclass.importFor(name);
        return null;
    }

    public static ProtoObject putAt(ProtoObject receiver, ProtoObject object, int index) {
        receiver.perform(receiver.smalltalkNumber(String.valueOf(index)), object, "at:put:");
        return receiver;
    }

    public ProtoObject smalltalkNumber(int value) {
        return instanceOfWith("Integer", new BigDecimal(value));
    }

    public ProtoObject smalltalkNumber(Object javaValue) {
        return instanceOfWith("Integer", new BigDecimal((String) javaValue));
    }

    public ProtoObject smalltalkString(Object javaValue) {
        return instanceOfWith("String", javaValue);
    }

    public ProtoObject smalltalkArray(int size) {
        List<ProtoObject> array = new ArrayList<ProtoObject>();
        ProtoObject initialElement = nil();
        array.add(initialElement); // we add NIL at index 0 because smalltalk indexes start at 1.
        while (array.size() < size + 1)
            array.add(initialElement);
        return instanceOfWith("Array", array);
    }

    public ProtoObject smalltalkSymbol(Object javaValue) {
        String symbol = (String) javaValue;
        SmalltalkClassLoader smalltalkClassLoader = classLoader();
        if (smalltalkClassLoader.isInternedSymbol(symbol))
            return smalltalkClassLoader.internedSymbolAt(symbol);
        ProtoObject symbolObject = instanceOfWith("Symbol", symbol);
        smalltalkClassLoader.internSymbolAtPut(symbol, symbolObject);
        return symbolObject;
    }

    protected ProtoObject instanceOfWith(String type, Object javaValue) {
        ProtoObject anInstance = instanceOf(type);
        anInstance.javaValue(javaValue);
        return anInstance;
    }

    protected ProtoObject instanceOf(String type) {
        return isBootstrapping() ? new ProtoObject() : resolveObject(type).perform("new");
    }

    protected ProtoObject smalltalkBoolean(boolean value) {
        return value ? classLoader().TRUE : classLoader().FALSE;
    }

    protected boolean isBootstrapping() {
        return classLoader().isBootstrapping();
    }

    public void sendMessagesResult(ProtoObject result) {
        // should only be sent to subclass.
    }

    public ProtoObject sendMessagesResult() {
        // should only be sent to subclass.
        return null;
    }

    public ProtoObject block(String name, PrimContext context) {
        return classLoader().loadSmalltalkBlock(name, context);
    }

    public static ProtoObject blockAnswer(ProtoObject answer, ProtoBlock block, String blockReturnType) {
        return block.answer(answer, blockReturnType);
    }

    public ProtoObject variableAt(String name) {
        int index = selfclass.indexOfVariable(name);
        if (index != 0)
            return attributes[index];
        return resolveObject(name);
    }

    public ProtoObject variableAtPut(String name, ProtoObject object) {
        int index = selfclass.indexOfVariable(name);
        if (index != 0) {
            attributes[index] = object;
            return this;
        }
        throw new IllegalStateException("Slot for '" + name + "' not found.");
    }

    protected int indexOfVariable(String name) {
        // Will be overridden in Class.
        return 0;
    }

    // ------
    // NOTE: Having perform bundle arguments and call perform0 simplifies the call process in the compiler.
    // A future optimisation will be to remove this bundling, as creating an array and setting values is
    // expensive (compared to not doing it).

    public ProtoObject perform(String selector) {
        return perform0(selector);
    }

    public ProtoObject superPerform(PrimContext context, String selector) {
        return perform0s(context, selector);
    }

    public ProtoObject perform(ProtoObject arg1, String selector) {
        return perform0(selector, arg1);
    }

    public ProtoObject perform(ProtoObject arg1, ProtoObject selector) {
        return perform0((String) selector.javaValue(), arg1);
    }

    public ProtoObject superPerform(PrimContext context, ProtoObject arg1, String selector) {
        return perform0s(context, selector, arg1);
    }

    public ProtoObject perform(ProtoObject arg1, ProtoObject arg2, String selector) {
        return perform0(selector, arg1, arg2);
    }

    public ProtoObject superPerform(PrimContext context, ProtoObject arg1, ProtoObject arg2, String selector) {
        return perform0s(context, selector, arg1, arg2);
    }

    public ProtoObject perform(ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, String selector) {
        return perform0(selector, arg1, arg2, arg3);
    }

    public ProtoObject superPerform(PrimContext context, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, String selector) {
        return perform0s(context, selector, arg1, arg2, arg3);
    }

    public ProtoObject perform(ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, String selector) {
        return perform0(selector, arg1, arg2, arg3, arg4);
    }

    public ProtoObject superPerform(PrimContext context, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, String selector) {
        return perform0s(context, selector, arg1, arg2, arg3, arg4);
    }

    public ProtoObject perform(ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, String selector) {
        return perform0(selector, arg1, arg2, arg3, arg4, arg5);
    }

    public ProtoObject superPerform(PrimContext context, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, String selector) {
        return perform0s(context, selector, arg1, arg2, arg3, arg4, arg5);
    }

    protected ProtoObject perform0(String selector, ProtoObject ... arguments) {
        return perform0(selfclass, selector, arguments);
    }

    protected ProtoObject perform0s(PrimContext context, String selector, ProtoObject ... arguments) {
        return perform0(context.lookupClass.superclass(), selector, arguments);
    }

    protected ProtoObject perform0(ProtoObject foundInClass, String selector, ProtoObject... arguments) {
        ProtoObject cls = foundInClass;
        while (!cls.includesSelector(selector))
            cls = cls.superclass();
        return apply(cls.methodFor(selector), cls, selector, arguments);
    }

    protected ProtoObject methodFor(String selector) {
        return PrimDoesNotUnderstand.DOES_NOT_UNDERSTAND;
    }

    protected ProtoObject apply(ProtoObject method, ProtoObject foundInClass, String selector, ProtoObject... arguments) {
//        System.out.println("apply: " + selector + " to " + this + " found in " + foundInClass);
        return method.invoke(this, new PrimContext(this, foundInClass, selector, arguments));
    }

    protected ProtoObject invoke(ProtoObject receiver, PrimContext context) {
        return this;
    }

    protected boolean includesSelector(String selector) {
        return true;
    }

    // --------------------------------------------------
    // - Primitives -
    // The Blue Book chapter 29 provides a formal specification of the Primitve Methods.
    // These primitive methods are numbered 1 through 127.
    // Where the primitives make sense to Redline they are implemented below.
    // There are also new primitives (>127) that apply just to Redline Smalltalk.
    //

    public ProtoObject p1(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p2(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p3(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p4(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p5(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p6(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p7(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p8(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p9(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p10(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p11(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p12(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p13(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p14(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p15(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p16(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p17(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p18(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p19(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p20(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p21(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p22(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p23(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p24(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p25(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p26(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p27(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p28(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p29(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p30(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p31(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p32(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p33(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p34(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p35(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p36(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p37(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p38(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p39(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p40(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p41(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p42(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p43(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p44(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p45(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p46(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p47(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p48(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p49(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p50(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p51(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p52(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p53(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p54(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p55(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p56(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p57(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p58(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p59(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p60(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p61(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p62(ProtoObject receiver, PrimContext context) {
        // Object size.
        Object value = receiver.javaValue();
        if (value instanceof List)
            return smalltalkNumber(((List) value).size());
        return smalltalkNumber("0");
    }

    public ProtoObject p63(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p64(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p65(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p66(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p67(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p68(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p69(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p70(ProtoObject receiver, PrimContext context) {
        // The primitiveNew routine creates a new instance of the receiver (a class)
        ProtoClass aClass = (ProtoClass) receiver;
        ProtoObject anInstance = new ProtoObject();
        anInstance.selfclass = aClass;
        return anInstance;
    }

    public ProtoObject p71(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p72(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p73(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p74(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p75(ProtoObject receiver, PrimContext context) {
        // Object hash.
        return smalltalkNumber(new BigDecimal(receiver.hashCode()));
    }

    public ProtoObject p76(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p77(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p78(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p79(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p80(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p81(ProtoObject receiver, PrimContext context) {
        // [] value, value:, value:value: etc
        return receiver.invoke(receiver, context);
    }

    public ProtoObject p82(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p83(ProtoObject receiver, PrimContext context) {
        // Object perform: selector.
        return receiver.perform((String) context.argumentAt(0).javaValue());
    }

    public ProtoObject p84(ProtoObject receiver, PrimContext context) {
        // Object perform: selector with: anObject.
        return receiver.perform(context.argumentAt(1), (String) context.argumentAt(0).javaValue());
    }

    public ProtoObject p85(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p86(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p87(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p88(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p89(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p90(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p91(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p92(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p93(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p94(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p95(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p96(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p97(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p98(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p99(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p100(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p101(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p102(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p103(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p104(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p105(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p106(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p107(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p108(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p109(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p110(ProtoObject receiver, PrimContext context) {
        // Object ==
        return smalltalkBoolean(receiver.equals(context.argumentAt(0)));
    }

    public ProtoObject p111(ProtoObject receiver, PrimContext context) {
        // class - Answer the object which is the receiver's class.
        return receiver.selfclass;
    }

    public ProtoObject p112(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p113(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p114(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p115(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p116(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p117(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p118(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p119(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p120(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p121(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p122(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p123(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p124(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p125(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p126(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p127(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    // --------------------------------------
    // -- Primitive added be Redline below --

    public ProtoObject p128(ProtoObject receiver, PrimContext context) {
        return new PrimSubclass().invoke(receiver, context);
    }

    public ProtoObject p129(ProtoObject receiver, PrimContext context) {
        // atSelector: selector put: method.
        // TODO.JCL - don't create method each time. Cache PrimAtSelectorPutMethod
        return new PrimAtSelectorPutMethod().invoke(receiver, context);
    }

    public ProtoObject p130(ProtoObject receiver, PrimContext context) {
        // Behavior -> includesSelector: aSymbol
        String selector = (String) context.argumentAt(0).javaValue();
        return smalltalkBoolean(receiver.includesSelector(selector));
    }

    public ProtoObject p131(ProtoObject receiver, PrimContext context) {
        // Object -> error: aString.
        throw new RuntimeException(context.argumentAt(0).javaValue().toString());
    }

    public ProtoObject p132(ProtoObject receiver, PrimContext context) {
        // Behavior -> superclass.
//        System.out.println("superclass: " + receiver + " is " + receiver.superclass());
        return receiver.superclass();
    }

    public ProtoObject p133(ProtoObject receiver, PrimContext context) {
        // BlockClojure -> whileFalse
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p134(ProtoObject receiver, PrimContext context) {
        // BlockClojure -> whileFalse: aBlock
        // Evaluate the argument, aBlock, as long as the value of the receiver is false.
        ProtoObject aBlockResult = nil();
        ProtoObject aBlock = context.argumentAt(0);
        ProtoObject receiverResult = receiver.perform("value");
        while (receiverResult.isFalseObject()) {
            aBlockResult = aBlock.perform("value");
            receiverResult = receiver.perform("value");
        }
        return aBlockResult;
    }

    public ProtoObject p135(ProtoObject receiver, PrimContext context) {
        // BlockClojure -> whileNil: aBlock
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p136(ProtoObject receiver, PrimContext context) {
        // BlockClojure -> whileNotNil: aBlock
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p137(ProtoObject receiver, PrimContext context) {
        // BlockClojure -> whileTrue
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p138(ProtoObject receiver, PrimContext context) {
        // BlockClojure -> whileTrue: aBlock
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p139(ProtoObject receiver, PrimContext context) {
        // Behavior -> canUnderstand: aSymbol.
        String selector = (String) context.argumentAt(0).javaValue();
        ProtoObject aClass = receiver;
        boolean included = false;
        while (aClass.selfclass != null && (included = aClass.includesSelector(selector)) == false)
            aClass = aClass.superclass();
        return smalltalkBoolean(included);
    }

    public ProtoObject p140(ProtoObject receiver, PrimContext context) {
        // Object perform: aSymbol with: firstObject with: secondObject
        return receiver.perform(context.argumentAt(1), context.argumentAt(2), (String) context.argumentAt(0).javaValue());
    }

    public ProtoObject p141(ProtoObject receiver, PrimContext context) {
        // Object perform: aSymbol with: firstObject with: secondObject with: thirdObject
        return receiver.perform(context.argumentAt(1), context.argumentAt(2), context.argumentAt(3), (String) context.argumentAt(0).javaValue());
    }

    public ProtoObject p142(ProtoObject receiver, PrimContext context) {
        // Object perform: aSymbol withArguments: array
        return receiver.perform0((String) context.argumentAt(0).javaValue(), context.arrayArgumentAt(1));
    }

    public ProtoObject p143(ProtoObject receiver, PrimContext context) {
        // Object at: index put: anObject
        int offset = context.intArgumentAt(0);
        List<ProtoObject> array = (List<ProtoObject>) receiver.javaValue();
        if (offset > 0 && offset < array.size()) {
            ProtoObject anObject = context.argumentAt(1);
            array.set(offset, anObject);
            return anObject;
        } else
            return receiver.perform(context.argumentAt(0), "errorSubscriptBounds:");
    }

    public ProtoObject p144(ProtoObject receiver, PrimContext context) {
        // Object at: index
        int offset = context.intArgumentAt(0);
        List<ProtoObject> array = (List<ProtoObject>) receiver.javaValue();
        if (offset > 0 && offset < array.size()) {
            return array.get(offset);
        } else
            return receiver.perform(context.argumentAt(0), "errorSubscriptBounds:");
    }

    public ProtoObject p145(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p146(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p147(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p148(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p149(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p150(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p151(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p152(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p153(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p154(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p155(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p156(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p157(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p158(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p159(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p160(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p161(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p162(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p163(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p164(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p165(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p166(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p167(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p168(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p169(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p170(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p171(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p172(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p173(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p174(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p175(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p176(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p177(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p178(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p179(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p180(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p181(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p182(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p183(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p184(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p185(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p186(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p187(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p188(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p189(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p190(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p191(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p192(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p193(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p194(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p195(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p196(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p197(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p198(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p199(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }

    public ProtoObject p200(ProtoObject receiver, PrimContext context) {
        throw new IllegalStateException("Implement primitive.");
    }
}
