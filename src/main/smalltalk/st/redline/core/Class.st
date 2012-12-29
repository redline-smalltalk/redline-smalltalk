" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

ClassDescription < #Class.

- < name
    ^ self subclass: name instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' classInstanceVariableNames: ''.

- subclass: name
    "Create a new subclass of the receiver.  This is a convenience binary message that can be used when only the
     subclass name needs to specified.  It delegates to the default
     subclass:instanceVariableNames:classVariableNames:poolDictionaries: message"
    ^ self subclass: name instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' classInstanceVariableNames: ''.

- subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: poolDicts classInstanceVariableNames: classInstVarNames
    "create a new subclass of the receiver. Nb: Primitive 128 is added by Redline (not in Blue Book)."
    <primitive: 128>

- subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: poolDicts category: category
    "Provides compatability for other Smalltalks. Nb: we drop category."
    ^ self subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries classInstanceVariableNames: ''.

- instanceVariableNames: names
    <primitive: 131>

- classInstanceVariableNames: names
    <primitive: 132>

- classVariableNames: names
    <primitive: 133>

- poolDictionaries: dictionaries
    <primitive: 134>

- category: name
    <primitive: 135>

- fullyQualifiedName
    "Answer the fully qualified name (FQN) of the receiver or Nil. The FQN is the package name
    followed by the Class name separated by a period, for example: 'st.redline.Class'."
    <primitive: 228>
