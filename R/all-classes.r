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

# A bundle represents a list of blocks and how they should be processed
# and output.
setClass("RoxyBundle", representation(
  blocks = "list",
  processors = "list",
  writers = "list"))