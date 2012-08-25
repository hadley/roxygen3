rocout <- function(tag, name, subclass) {
  structure(list(tag = tag, name = name), class = c(subclass, "rocout"))
}


write_output <- function(writer, rocblocks) {
  UseMethod("write_output")
}

write_out <- function(roccers, rocblocks, out_path) {
  out <- list()
  
  out <- generate_output(roccers, rocblocks)
  
  writers <- names(out)
  for (writer in writers) {
    match.fun(writer)(out[[writer]], out_path)
  }
  
}

generate_output <- function(roccers, rocblocks) {
  out <- list()
  for (roccer in roccers) {
    rocout <- roccer$output
    if (is.null(rocout)) next
    
    type <- output_type(rocout)
    if (is.null(out[[type]])) out[[type]] <- list()
    
    for (rocblock in rocblocks) {
      tag <- rocblock$roc[[rocout$name]]
      if (is.null(tag)) next
      
      path <- output_path(rocout, rocblock)
      n <- length(out[[type]][[path]]) + 1
      if (n == 1) out[[type]][[path]] <- list()
      out[[type]][[path]][n] <- rocout$tag(tag)
    }
  }
  out
}

output_path <- function(writer, rocblock) {
  UseMethod("output_path")
}

output_type <- function(writer, rocblock) {
  UseMethod("output_type")
}



# @return a list of the same length as \code{rocblocks}
process_output <- function(rocout, rocblocks) {
  # Name should have been set by roccer
  stopifnot(!is.null(rocout$name))
  
  lapply(rocblocks, function(rocblock) {
    tag <- rocblock$roc[[rocout$name]]
    if (is.null(tag)) return()
    rocout$tag(tag)
  })
}

