" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Stout.

- initialize
    Transcript show: 'stout initialize'; cr.
    super initialize.
    "create router registry."
    JVM aload: 1;
        new: 'st/redline/stout/RouterRegistryImpl';
        dup;
        new: 'st/redline/stout/RouterFactoryImpl';
        dup;
        new: 'st/redline/stout/RequestPathSpecificationFactoryImpl';
        dup;
        invokeSpecial: 'st/redline/stout/RequestPathSpecificationFactoryImpl' method: '<init>' returning: '()V';
        invokeSpecial: 'st/redline/stout/RouterFactoryImpl' method: '<init>' returning: '(Lst/redline/stout/RequestPathSpecificationFactory;)V';
        invokeSpecial: 'st/redline/stout/RouterRegistryImpl' method: '<init>' returning: '(Lst/redline/stout/RouterFactory;)V';
        invokeVirtual: 'st/redline/PrimObject' method: 'javaValue' returning: '(Ljava/lang/Object;)Lst/redline/PrimObject;'.

- handle: action on: target with: request and: response and: dispatch
    Transcript show: 'stout handle'; cr.
    "handle http request type 'action' on 'target' uri."
    JVM aload: 1;
        invokeVirtual: 'st/redline/PrimObject' method: 'javaValue' returning: '()Ljava/lang/Object;';
        checkcast: 'st/redline/stout/RouterRegistry';
        arg: 3;
        arg: 2;
        invokeInterface: 'st/redline/stout/RouterRegistry' method: 'lookup' returning: '(Lst/redline/PrimObject;Lst/redline/PrimObject;)Lst/redline/stout/Router;';
        arg: 5;
        arg: 2;
        invokeInterface: 'st/redline/stout/Router' method: 'dispatchToBlock' returning: '(Lst/redline/PrimObject;Lst/redline/PrimObject;)V';
        aload: 1.

- get: target do: aBlock
    Transcript show: 'stout get'; cr.
    "register aBlock to be executed when a HTTP GET of target is received."
    JVM aload: 1;
        invokeVirtual: 'st/redline/PrimObject' method: 'javaValue' returning: '()Ljava/lang/Object;';
        checkcast: 'st/redline/stout/RouterRegistry';
        arg: 0;
        ldc: 'JSON';
        ldc: 'GET';
        arg: 1;
        invokeInterface: 'st/redline/stout/RouterRegistry' method: 'register' returning: '(Lst/redline/PrimObject;Ljava/lang/String;Ljava/lang/String;Lst/redline/PrimObject;)Lst/redline/stout/Router;';
        aload: 1.
