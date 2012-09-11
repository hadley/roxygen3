setOldClass("srcref")
setClass("SrcrefNull", contains = "srcref")

setClass("Tag", 
  contains = "VIRTUAL",
  prototype = list(srcref = new("SrcrefNull")),
  representation(
    text = "character",
    srcref = "srcref"))

setClass("RoxyObject", representation(
  name = "character",
  value = "ANY",
  srcref = "srcref",
  docType = "character"))

setClass("Usage")
setClass("NullUsage", contains = "Usage")

setClass("RoxyBlock", representation(
  tags = "list",
  srcref = "srcref",
  object = "RoxyObject"))

# A behaviour object describes what tags, processors and writers should be
# applied to a block
setClass("RoxyBehaviour", representation(
  tags = "character",
  processors = "character",
  writers = "character"
))

# A bundle represents a list of blocks and how their behaviour
setClass("RoxyBundle", representation(
  blocks = "list",
  behaviour = "RoxyBehaviour"))