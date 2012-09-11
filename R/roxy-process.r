setMethod("process", "RoxyBlock", function(input) {
  for (tag in seq_along(input@tags)) {
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
