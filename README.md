Roxygen3 is a ground-up rewrite of roxygen2 aiming for a design that is simpler, more modular and easier to understand so that bugs can be fixed more rapidly and others can extend roxygen to meet their specific needs.  

It is likely that roxygen3 will never be released on CRAN, but instead will be merged in roxygen2.

To try it out:

      # install.packages("devtools")
      library(devtools)
      install_github("devtools") # you need the latest version
      install_github("roxygen3")
      
      library(roxygen3)
      roxygenise("path/to/my/package")

# New features

* `@export` is much smarter - it will automatically figure out if you have an
  S3 method. It will also automatically export all the methods for a S3
  generic.

* `@autoImports` will automatically generate the appropriate import directives
  for all functions that your function uses. It will respect `@importFrom`
  directives if you need to manually resolve conflicts.

# Custom behaviours

Roxygen3 has behaviour objects, which captures exactly what tags, processors and writers are applied to the roxygen blocks. By modifying the `default_behaviour()` you can turn off tags that you don't want, or restrict the output to only Rd files, the `NAMESPACE` or the `DESCRIPTION`.

# Developers guide

One of the main motivations for this rewrite of roxygen is to make it much easier to extend (and make it easier to fix bugs!). It has been rewritten to use S4, along with a much stronger object system that should make it easier to understand what's going on. The following section gives an overview of the important classes and generics. More detail will eventually be available in the class and generic documentation.

The best way to learn is by example - so make sure you have a source version of roxygen3 installed 

Naming conventions: 

* S4 classes and constructors: `AClass`
* S4 methods: `aMethod`
* functions `a_function`

## Class overview

* The `Block` encapsulates information about all expressions in a package
  and the the documentation associated with them. Note that all top-level
  calls get roxygen block even if they don't have documentation. A block
  consists of a list of `@tags`, an `@object` that represents the object being
  documented, and a `@srcref` that provides the location of the documentation
  block.

  * A `Tag` object corresponds to a single roxygen tag: `#' @tag text`. Tag
    objects are created by the `build_tag` function and come preloaded with a
    `@text` slot. Tags that need richer data structures should define
    additional slots and fill them up with the `procTag` or `procBlock`
    methods. Tags are also the unit at which output functions operator. See
    the Tags section for more details.

  * A `Object` represents the object that a block documents. It has
    subclasses `NullObject` (for blocks that don't document an object),
    `FunctionObject`, `DataObject`, `PackageObject`, `S3GenericObject`,
    `S3MethodObject`, `S4GenericObject`, `S4MethodObject`, `S4ClassObject` and
    `R5ClassObject`. These are created from the call that the block documents
    in `object_from_call`

  * A `srcref` is the S3 `srcref` object which stores a file and location of
    source code

* A `Bundle` stores a list of blocks. There are currently two subclasses:
  `DirectoryBundle` and `PackageBundle` for representing all the blocks in a directory
  or package respectively. The main different between a package and directory
  is that a package has a name, and getting to the R code from the base paths
  are slightly different.

* A `Behaviour` stores a list of `Tag`s, a list of processors functions
  and a list of writer functions. These are used to control the output, and if
  you extend roxygen with new processors or writers, you will need to provide
  some way for users to easily create behaviours that use them.

  * Processors and writers are just functions that should accept a
    `Bundle` as the first argument. Processors should return a
    `Bundle` and writers should be run only for their side effects:
    creating files on disk. Writers will generally be S4 generics that break
    apart the bundle into blocks and tags - see the code for `writeRd`,
    `writeNamespace` and `writeDescription` for examples.

## Tags

To implement a new tag for roxygen, you need to subclass `Tag` and implement (at least) some output methods.

Tag processing:

* `procTag` is called with a single tag, and should return a
  modified tag. Use this for simple tag behaviour that depends only on the
  text of one tag.

* `procBlock` is called with the tag and the whole block and should return a
  block. Use this is the tag needs to add multiple tags to the output, or
  needs to access the object or srcref associated with the block.

Default tags:

* `defaultTag` provides an object specific way of adding default tags to a
  block. It is called with a tag and an object, and should return a new tag
  that will be added to the block if a user supplied tag of that name is not
  already present. This mechanism is used to automatically add `@name`,
  `@rdname`, `@usage`, `@docType`, and `@defaultExport` for all objects. (Note
  that this method is called using special dispatch and the tag object will
  never be supplied.)

Output:

`writeRd`, `writeNamespace`, and `writeDescription` return objects that will
be used to modify Rd files, the `NAMESPACE` and `DESCRIPTION` respectively.

* `writeRd` methods should return an Rd command object

* `writeNamespace` methods should return a character vector of namespace
  directives

* `writeDescription` should return a named list containing character vectors
  of length one.
