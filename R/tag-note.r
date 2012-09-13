setClass("NoteTag", contains = "Tag")
setMethod("writeRd", "NoteTag", function(object) {
  RdCommand("note", object@text)
})
