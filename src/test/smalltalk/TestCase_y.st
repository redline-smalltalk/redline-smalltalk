resources
        | allResources resourceQueue |
        allResources := Set new.
        resourceQueue := OrderedCollection new.
        resourceQueue addAll: self class resources.
        [resourceQueue isEmpty] whileFalse: [
                | next |
                next := resourceQueue removeFirst.
                allResources add: next.
                resourceQueue addAll: next resources].
        ^allResources
                        ! !

