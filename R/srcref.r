setClass("SrcrefNull", contains = "srcref")
setMethod("isNull", "SrcrefNull", function(x) TRUE)
