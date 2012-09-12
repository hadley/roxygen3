setOldClass("srcref")
setClass("NullSrcref", contains = "srcref")

setClass("Tag", contains = "VIRTUAL",
  representation(
    text = "character",
    srcref = "srcref"
  ),
  prototype(
    srcref = new("NullSrcref")
  )
)

setClass("Object", 
  representation(
    name = "character",
    value = "ANY",
    srcref = "srcref",
    docType = "character"
  )
)
setClass("NullObject", contains = "Object")

setClass("Usage")
setClass("NullUsage", contains = "Usage")

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
setClass("Behaviour", 
  representation(
    tags = "character",
    processors = "character",
    writers = "character"
  )
)

setClass("Bundle", 
  representation(
    blocks = "list",
    behaviour = "Behaviour"
  )
)

setClass("DirectoryBundle", contains = "Bundle", 
  representation(
    path = "character"
  )
)

setClass("PackageBundle", contains = "DirectoryBundle", 
  representation(
    name = "character"
  )
)
