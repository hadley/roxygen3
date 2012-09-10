setOldClass("srcref")

setClass("Tag", representation(
  text = "character",
  srcref = "srcref",
  "VIRTUAL"))

setClass("RoxyObject", representation(
  name = "character",
  value = "ANY",
  srcref = "srcref",
  docType = "character"))

setClass("RoxyBlock", representation(
  tags = "list",
  srcref = "srcref",
  object = "RoxyObject"))

# A list of rocblocks - will usually create subclass
setClass("RoxyBundle", representation(
  blocks = "list",
  processors = "character"))