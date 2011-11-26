" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Stout

- initialize
    super initialize.
    "create router registry."
    <varInsn: ALOAD, 1>.
    <typeInsn: NEW, 'st/redline/stout/RouterRegistryImpl'>.
    <insn: DUP>.
    <typeInsn: NEW, 'st/redline/stout/RouterFactoryImpl'>.
    <insn: DUP>.
    <typeInsn: NEW, 'st/redline/stout/RequestPathSpecificationFactoryImpl'>.
    <insn: DUP>.
    <methodInsn: INVOKESPECIAL, 'st/redline/stout/RequestPathSpecificationFactoryImpl', '<init>', '()V'>.
    <methodInsn: INVOKESPECIAL, 'st/redline/stout/RouterFactoryImpl', '<init>', '(Lst/redline/stout/RequestPathSpecificationFactory;)V'>.
    <methodInsn: INVOKESPECIAL, 'st/redline/stout/RouterRegistryImpl', '<init>', '(Lst/redline/stout/RouterFactory;)V'>.
    <methodInsn: INVOKEVIRTUAL, 'st/redline/ProtoObject', 'javaValue', '(Ljava/lang/Object;)Lst/redline/ProtoObject;'>.

- handle: action on: target with: request and: response and: dispatch
    "handle http request type 'action' on 'target' uri."
    <varInsn: ALOAD, 1>.
    <methodInsn: INVOKEVIRTUAL, 'st/redline/ProtoObject', 'javaValue', '()Ljava/lang/Object;'>.
    <typeInsn: CHECKCAST, 'st/redline/stout/RouterRegistry'>.
    <varInsn: ALOAD, 4>.
    <varInsn: ALOAD, 3>.
    <methodInsn: INVOKEINTERFACE, 'st/redline/stout/RouterRegistry', 'lookup', '(Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/stout/Router;'>.
    <varInsn: ALOAD, 6>.
    <varInsn: ALOAD, 3>.
    <methodInsn: INVOKEINTERFACE, 'st/redline/stout/Router', 'dispatchToBlock', '(Lst/redline/ProtoObject;Lst/redline/ProtoObject;)V'>.
    <varInsn: ALOAD, 1>.

- get: target do: aBlock
    "register aBlock to be executed when a HTTP GET of target is received."
    <varInsn: ALOAD, 1>.
    <methodInsn: INVOKEVIRTUAL, 'st/redline/ProtoObject', 'javaValue', '()Ljava/lang/Object;'>.
    <typeInsn: CHECKCAST, 'st/redline/stout/RouterRegistry'>.
    <varInsn: ALOAD, 3>.
    <ldcInsn: 'JSON'>.
    <ldcInsn: 'GET'>.
    <varInsn: ALOAD, 4>.
    <methodInsn: INVOKEINTERFACE, 'st/redline/stout/RouterRegistry', 'register', '(Lst/redline/ProtoObject;Ljava/lang/String;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/stout/Router;'>.
    <varInsn: ALOAD, 1>.
