setClass("NoteTag", contains = "Tag")
setMethod("writeRd", "NoteTag", function(object) {
  new_command("note", object@text)
})
