#' @@slot: document the slots of a S4 class.
#'
#' Many S4 slots are considered internal implementation details, so slots
#' (unlike params) are not documented by default. You can use the \code{@@slot}
#' and \code{@@autoSlots} tags to document them. An example of the output is
#' include below.
#'
#' If a name but no description is provided for \code{@@slot}, the description
#' will just contain a link to the documentation for the type of object that
#' goes in that slot.  This is what is used for all slots if you use
#' \code{@@autoSlots}.
#'
#' @tagUsage
#'   @@slot name
#'   @@slot name description
#'   @@autoSlots
#' @rdname slots
#' @autoSlots
setClass("SlotTag", contains = "Tag", representation(
  slots = "character",
  classname = "character"))

setMethod("value", "SlotTag", function(tag) tag@slots)
setMethod("value<-", "SlotTag", function(tag, value) {
  pieces <- str_split_fixed(value, "[[:space:]]+", 2)
  tag@slots <- setNames(pieces[, 2], pieces[, 1])
  tag
})

setMethod("process", "SlotTag", function(input, block) {
  if (!is(block@object, "S4ClassObject")) {
    message("@slot only valid for documenting S4 classes ", location(block))
    return(block)
  }

  empty <- input@slots == ""
  if (all(!empty)) return(block)

  slots <- names(input@slots)[empty]

  input@slots[empty] <- describe_slots(block@object@value, slots)
  tag(block, "slot") <- input
  block
})

describe_slots <- function(class, slots) {
  desc <- describe(slot_classes(class)[slots])
  setNames(str_c("An object of class ", desc), slots)
}

setMethod("writeRd", "SlotTag", function(object) {
  RdCommand("slots", object@slots)
})

setClass("SlotsCommand", contains = "RdCommand")
setMethod("format", "SlotsCommand", function(x, ...) {
  items <- str_c("  \\item{", names(x@values), "}{", x@values, "}\n",
    collapse = "\n")

  str_c(
    "\\section{Slots}{\n",
    "\\describe{\n",
    items,
    "\n}",
    "\n}"
  )
})

#' @rdname slots
setClass("AutoSlotsTag", contains = "Tag")
setMethod("getPrereqs", "AutoSlotsTag", function(tag) "SlotTag")

setMethod("process", "AutoSlotsTag", function(input, block) {
  if (!is(block@object@value, "classRepresentation")) {
    message("@autoSlots only valid for documenting S4 classes ",
      location(block))
    return(block)
  }

  obj <- block@object@value
  slot_tag <- tag(block, "slot")
  missing <- setdiff(slotNames(obj), names(slot_tag@slots))

  slot_tag@slots <- c(slot_tag@slots, describe_slots(obj, missing))
  tag(block, "slot") <- slot_tag
  tag(block, "autoSlot") <- NULL

  block
})
