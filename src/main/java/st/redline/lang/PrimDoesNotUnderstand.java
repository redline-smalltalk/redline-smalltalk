/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

public class PrimDoesNotUnderstand extends ProtoObject {

    protected static final ProtoObject DOES_NOT_UNDERSTAND = new PrimDoesNotUnderstand();

    protected ProtoObject invoke(ProtoObject receiver, PrimContext context) {
        if (!"basicDoesNotUnderstand:".equals(context.selector))
            return receiver.perform0("basicDoesNotUnderstand:", copySelectorAndArguments(context));
        outputDoesNotUnderstandError(receiver, context);
        return PrimNil.PRIM_NIL;
    }

    private void outputDoesNotUnderstandError(ProtoObject receiver, PrimContext context) {
        StringBuilder message = new StringBuilder();
        message.append("Object '").append(receiver)
               .append("' does not understand '")
               .append(context.arguments[0].javaValue).append("'");
//        if (context.arguments.length > 1) {
//            message.append(" with arguments:\n");
//            for (int i = 1; i < context.arguments.length; i++)
//                message.append(i).append("\t").append(context.arguments[i]).append(" ")
//                        .append(context.arguments[i].javaValue()).append("\n");
//        } else
//            message.append(".");
//        System.out.println();
//        System.out.println();
//        System.out.println("Receiver       " + receiver);
//        if (receiver instanceof ProtoClass)
//            System.out.println("Methods " + ((ProtoClass) receiver).methods);
        throw new DoesNotUnderstandException(message.toString());
    }

    private ProtoObject[] copySelectorAndArguments(PrimContext context) {
        ProtoObject selectorObject = new ProtoObject();
        selectorObject.javaValue = context.selector;
        ProtoObject[] argumentsWithSelector = new ProtoObject[context.arguments.length + 1];
        argumentsWithSelector[0] = selectorObject;
        System.arraycopy(context.arguments, 0, argumentsWithSelector, 1, context.arguments.length);
        return argumentsWithSelector;
    }
}
