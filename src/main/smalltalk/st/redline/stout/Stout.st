" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Stout.

- initialize
    super initialize.
    "create router registry."
    JVM aload: 1;
        new: 'st/redline/stout/RouterRegistryImpl';
        dup;
        new: 'st/redline/stout/RouterFactoryImpl';
        dup;
        new: 'st/redline/stout/RequestPathSpecificationFactoryImpl';
        dup;
        invokeSpecial: 'st/redline/stout/RequestPathSpecificationFactoryImpl' method: '<init>' matching: '()V';
        invokeSpecial: 'st/redline/stout/RouterFactoryImpl' method: '<init>' matching: '(Lst/redline/stout/RequestPathSpecificationFactory;)V';
        invokeSpecial: 'st/redline/stout/RouterRegistryImpl' method: '<init>' matching: '(Lst/redline/stout/RouterFactory;)V';
        invokeVirtual: 'st/redline/PrimObject' method: 'javaValue' matching: '(Ljava/lang/Object;)Lst/redline/PrimObject;'.
    self route.

- handle: action on: target with: request and: response and: dispatch
    "handle http request type 'action' on 'target' uri."
    JVM aload: 1;
        invokeVirtual: 'st/redline/PrimObject' method: 'javaValue' matching: '()Ljava/lang/Object;';
        checkcast: 'st/redline/stout/RouterRegistry';
        arg: 1;
        arg: 0;
        invokeInterface: 'st/redline/stout/RouterRegistry' method: 'lookup' matching: '(Lst/redline/PrimObject;Lst/redline/PrimObject;)Lst/redline/stout/Router;';
        arg: 3;
        arg: 1;
        invokeInterface: 'st/redline/stout/Router' method: 'dispatchToBlock' matching: '(Lst/redline/PrimObject;Lst/redline/PrimObject;)V';
        aload: 1.

- defaultAcceptType
    "answer the default accepted content type."
    ^ #JSON.

- route
    "setup the routes for the Web App."
    ^ self.

- onGet: target do: aBlock
    "register aBlock to be executed when a HTTP GET of 'target' accepting JSON is received."
    self when: #GET on: target accepting: (self defaultAcceptType) do: aBlock.

- onPost: target do: aBlock
    "register aBlock to be executed when a HTTP POST to 'target' accepting JSON is received."
    self when: #POST on: target accepting: (self defaultAcceptType) do: aBlock.

- onPut: target do: aBlock
    "register aBlock to be executed when a HTTP PUT to 'target' accepting JSON is received."
    self when: #PUT on: target accepting: (self defaultAcceptType) do: aBlock.

- when: action on: target accepting: type do: aBlock
    "register aBlock to be executed when a HTTP 'action' on 'target' accepting 'type' is received."
    JVM aload: 1;
        invokeVirtual: 'st/redline/PrimObject' method: 'javaValue' matching: '()Ljava/lang/Object;';
        checkcast: 'st/redline/stout/RouterRegistry';
        arg: 1;
        arg: 2;
        arg: 0;
        arg: 3;
        invokeInterface: 'st/redline/stout/RouterRegistry' method: 'register' matching: '(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;)Lst/redline/stout/Router;';
        aload: 1.
