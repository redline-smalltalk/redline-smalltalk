"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

self import: 'st.redline.test.TestCase'.

TestCase subclass: #ObjectTest.

- test
    self testHasInitializeMethods.
    self testComparingMethods.
    self testEvaluationMethods.
    self testAccessingMethods.
    self testClassMembershipMethods.
    self testTestingMethods.
    self testMessageHandlingMethods.

- testHasInitializeMethods
    "Assert Object has the expected initialize class and instance methods."
    self assertTrue: (Object includesSelector: #initialize) withMessage: 'should have initialize method.'.
    self assertTrue: (Object class includesSelector: #initialize) withMessage: 'should have initialize method.'.

- testComparingMethods
    "Assert Object has the expected comparing methods."
    self assertTrue: (Object == Object) withMessage: 'should be equal (==).'.
    self assertTrue: (Object = Object) withMessage: 'should be equal (=).'.
    self assertTrue: (Object ~= String) withMessage: 'should not be equal (~=).'.
    self assertTrue: (Object ~~ String) withMessage: 'should not be equal (~~).'.
    "self assertTrue: (Object identityHash ~= 0) withMessage: 'should not be 0.'.
    self assertTrue: (Object hash ~= 0) withMessage: 'should not be 0.'."

- testEvaluationMethods
    "Assert Object has the expected evaluation methods."
    self assertTrue: (Object value == Object) withMessage: 'should evaluate to self.'.

- testAccessingMethods
    "Assert Object has the expected accessing methods."
    self assertTrue: (Object yourself == Object) withMessage: 'should evaluate to self.'.
    self assertTrue: ('' class == String) withMessage: 'should have class.'.
    "self assertTrue: (Object size == 0) withMessage: 'should be size 0.'.
    self assertTrue: (Object basicSize == 0) withMessage: 'should be size 0.'."

- testClassMembershipMethods
    "Assert Object has the expected class membership methods."
    self assertTrue: (String isKindOf: Object) withMessage: 'should be kind of Object.'.
    "self assertFalse: (String isKindOf: Number) withMessage: 'should not be kind of Number.'."
    self assertTrue: (String isMemberOf: String class) withMessage: 'should be String.'.
    self assertFalse: (String isMemberOf: Object class) withMessage: 'should not be String.'.
    self assertTrue: (Object respondsTo: #isKindOf:) withMessage: 'should respond to #isKindOf:.'.
    self assertFalse: (Object respondsTo: #not) withMessage: 'should not respond to #not.'.

- testTestingMethods
    "Assert Object has the expected testing methods."
    self assertFalse: (Object isNil) withMessage: 'should not be Nil.'.
    self assertTrue: (Object notNil) withMessage: 'should not be Nil.'.

- testMessageHandlingMethods
    "Assert Object has the expected message handling methods."
    self assertTrue: (Object perform: #notNil) withMessage: 'should handle perform and receive true as answer.'.
    self assertTrue: (Object perform: #respondsTo: with: #isKindOf:) withMessage: 'should handle perform with anObject argument.'.
    "self assertTrue: (Object perform: #???: with: ??? with: ???) withMessage: 'should handle perform with firstObject and secondObject arguments.'."
    "self assertTrue: (Object perform: #???: with: ??? with: ??? with: ???) withMessage: 'should handle perform with 3 arguments.'."
    self assertFalse: (Object perform: #== withArguments: #(Object)) withMessage: 'should handle perform with arguments.'.
