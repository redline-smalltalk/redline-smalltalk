/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

class PrimBasicDoesNotUnderstand extends PrimObject {

    protected PrimObject invoke(PrimObject receiver, PrimContext context) {
        if (!"basicDoesNotUnderstand:".equals(context.selector))
            return receiver.perform0("basicDoesNotUnderstand:", copySelectorAndArguments(context));
        outputDoesNotUnderstandError(receiver, context);
        return PRIM_NIL;
    }

    private void outputDoesNotUnderstandError(PrimObject receiver, PrimContext context) {
        StringBuilder message = new StringBuilder();
        message.append("Object '").append(receiver).append("' (").append(receiver.cls()).append(")")
                .append(" does not understand '")
                .append(context.arguments[0].javaValue).append("'");
        if (context.arguments.length > 1) {
            message.append(" with arguments:\n");
            for (int i = 1; i < context.arguments.length; i++)
                message.append(i).append("\t").append(context.arguments[i]).append(" ")
                        .append(context.arguments[i].javaValue()).append("\n");
        } else
            message.append(".");
        System.out.println();
        System.out.println();
        System.out.println(receiver);
        System.out.println(receiver.cls());
        System.out.println(receiver.cls().superclass());
        throw new RedlineException(message.toString());
    }

    private PrimObject[] copySelectorAndArguments(PrimContext context) {
        PrimObject selectorObject = new PrimObject();
        selectorObject.javaValue(context.selector);
        PrimObject[] argumentsWithSelector = new PrimObject[context.arguments.length + 1];
        argumentsWithSelector[0] = selectorObject;
        System.arraycopy(context.arguments, 0, argumentsWithSelector, 1, context.arguments.length);
        return argumentsWithSelector;
    }
}
