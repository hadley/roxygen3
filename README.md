
The generic function `defaultTag` provides an object specific way of adding default tags to a block. This mechanism is used to automatically add `@name`, `@rdname`, `@usage`, `@docType`, `@defaultExport` for all objects, and for some objects is used to add `@format` and `@aliases`.

(Note that this is a special dispatch - the tag object will never be supplied.)