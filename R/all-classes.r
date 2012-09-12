setOldClass("srcref")
setClass("NullSrcref", contains = "srcref")

#' @rdname Tag
setClass("Tag", contains = "VIRTUAL",
  representation(
    text = "character",
    srcref = "srcref"
  ),
  prototype(
    srcref = new("NullSrcref")
  )
)

#' @rdname Object
setClass("Object",
  representation(
    name = "character",
    value = "ANY",
    srcref = "srcref",
    docType = "character"
  )
)
setClass("NullObject", contains = "Object")

#' @rdname Usage
setClass("Usage")
setClass("NullUsage", contains = "Usage")

#' @rdname Block
setClass("Block",
  representation(
    tags = "list",
    srcref = "srcref",
    object = "Object"
  ),
  prototype(
    tags = list(),
    srcref = new("NullSrcref"),
    object = new("NullObject")
  )
)

# A behaviour object describes what tags, processors and writers should be
# applied to a block
#' @rdname Behaviour
setClass("Behaviour",
  representation(
    tags = "character",
    processors = "character",
    writers = "character"
  )
)

#' @rdname Bundle
setClass("Bundle",
  representation(
    blocks = "list",
    behaviour = "Behaviour"
  )
)

#' @rdname DirectoryBundle
setClass("DirectoryBundle", contains = "Bundle",
  representation(
    path = "character"
  )
)

#' @rdname PackageBundle
setClass("PackageBundle", contains = "DirectoryBundle",
  representation(
    name = "character"
  )
)
