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

# A bundle represents a list of blocks and how they should be processed
# and output.
setClass("RoxyBundle", representation(
  blocks = "list",
  processors = "list",
  writers = "list"))