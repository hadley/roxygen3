setGeneric("roxyProcess", function(input) {
  standardGeneric("roxyProcess")
})

setMethod("roxyProcess", "RoxyPackage", function(input) {
  # Process each block individually for local tags
  input@blocks <- lapply(input@blocks, roxyProcess)
  
  # # Process the roccers, tags with global behaviour
  # for (roccer in input@roccers) {
  #   input@blocks <- procPackage(roccer, package = input)
  # }

  input
})

setMethod("roxyProcess", "RoxyBlock", function(input) {
  for (tag in names(input@tags)) {    
    input@tags[[tag]] <- procTag(input@tags[[tag]])
    input <- procBlock(input@tags[[tag]], block = input)
  }
  input
})

setGeneric("procPackage", function(roccer, package) {
  standardGeneric("procPackage")
}, valueClass = "RoxyPackage")
setGeneric("procBlock", function(tag, block) {
  standardGeneric("procBlock")
}, valueClass = "RoxyBlock")
setGeneric("procTag", function(tag) {
  standardGeneric("procTag")
}, valueClass = "Tag")

setGeneric("procText", function(tag, text) {
  standardGeneric("procText")
}, valueClass = "character")
