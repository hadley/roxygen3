#' Encapuslate default behaviour.
#'
#' Roxygen defaults to using all subclasses of \code{\linkS4class{Tag}},
#' and all processors (\code{$process[A-Z].*}) and writers
#' (\code{$writer[A-Z].*}) defined in the roxygen package.
#'
#' @param debug if \code{TRUE}, will automatically add source information to
#'  add generated Rd files.
#' @dev
default_behaviour <- function(debug = FALSE) {
  tags <- base_tags()
  if (!debug) tags <- setdiff(tags, "debug")

  new("Behaviour",
    tags = tags,
    processors = local_apropos("^process[A-Z_]"),
    writers = local_apropos("^write[A-Z]")
  )
}

manual_behaviour <- function(tags = character(), processors = character(), writers = character()) {
  new("Behaviour",
    tags = tags,
    processors = processors,
    writers = writers
  )
}
