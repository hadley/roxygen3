#' Parse directory of source files.
#'
#' @param path path to directory of R files to parse
#' @param env environment that contains the results of evaluating the file.
#' @keywords internal
parse_directory <- function(path, env, tags = base_tags()) {
  r_files <- dir(path, pattern = "\\.[RrSs]$", full.names = TRUE)

  unlist(lapply(r_files, parse_file, env = env, default_tags = tags),
    recursive = FALSE)
}

#' Parse a source file containing roxygen blocks.
#'
#' @param path path of file to parse
#' @inheritParams parse_directory
#' @return A list of roc objects
#' @keywords internal
parse_file <- function(path, env, default_tags = base_tags()) {
  # Find the locations of (comment + code) blocks
  lines <- readLines(path, warn = FALSE)
  src <- srcfile(path)

  parse_text(lines, env, src, default_tags = default_tags)
}

parse_text <- memoise(function(lines, env, src, default_tags) {
  parsed <- parse(text = lines, src = src)
  if (length(parsed) == 0) {
    return(list())
  }

  refs <- getSrcref(parsed)
  comment_refs <- comments(refs)

  # Walk through each src ref and match code and comments
  extract <- function(i) {
    ref <- comment_refs[[i]]
    tags <- parse_roc(as.character(ref), default_tags = default_tags)
    obj <- object_from_call(parsed[[i]], env, refs[[i]])

    if (isNull(obj) & !isNull(tags$docType)) {
      name <- if (isNull(tags$name)) getPackageName(env) else value(tags$name)
      obj <- new("PackageObject", name = name, srcref = refs[[i]])
    }

    if (is.null(tags) && is.null(obj)) return()

    Block(tags, obj, ref, default_tags = default_tags)
  }
  compact(lapply(seq_along(parsed), extract))
})

# For each src ref, find the comment block preceeding it
comments <- function(refs) {
  srcfile <- attr(refs[[1]], "srcfile")

  # first_line, first_byte, last_line, last_byte
  com <- vector("list", length(refs))
  for(i in seq_along(refs)) {
    # Comments begin after last line of last block, and continue to
    # first line of this block
    if (i == 1) {
      first_byte <- 1
      first_line <- 1
    } else {
      first_byte <- refs[[i - 1]][4] + 1
      first_line <- refs[[i - 1]][3]
    }

    last_line <- refs[[i]][1]
    last_byte <- refs[[i]][2] - 1
    if (last_byte == 0) {
      if (last_line == 1) {
        last_byte <- 1
        last_line <- 1
      } else {
        last_line <- last_line - 1
        last_byte <- 1e3
      }
    }

    lloc <- c(first_line, first_byte, last_line, last_byte)
    com[[i]] <- srcref(srcfile, lloc)
  }

  com
}

#' @autoImports
# @param tags A character vector of tag names, order is respected.
parse_roc <- function(lines, match = "^\\s*#+\' ?", default_tags) {
  lines <- lines[str_detect(lines, match)]
  if (length(lines) == 0) return(list())

  trimmed <- str_replace(lines, match, "")
  joined <- str_c(trimmed, collapse = '\n')

  # If the comment block does not start with a @, then it must be the
  # introduction section
  if (!str_detect(joined, "^@")) {
    joined <- str_c("@intro ", joined)
  }

  ## Thanks to Fegis at #regex on Freenode for the
  ## lookahead/lookbehind hack; as he notes, however, "it's not
  ## proper escaping though... it will not split a@@@b."
  elements <- str_split(joined, perl('(?<!@)@(?!@)'))[[1]][-1]
  elements <- str_replace_all(elements, fixed("@@"), "@")

  cols <- str_split_fixed(elements, "[[:space:]]+", 2)
  cols[, 2] <- str_trim(cols[, 2])
  parsed_tags <- tapply(cols[, 2], cols[, 1], list)

  # Remove unknown tags and reorder
  unknown <- setdiff(names(parsed_tags), default_tags)
  if (length(unknown) > 0) {
    message("Unknown tags: ", str_c("@", unique(unknown), collapse = ", "))
  }
  parsed_tags <- parsed_tags[intersect(default_tags, names(parsed_tags))]

  compact(Map(build_tag, names(parsed_tags), parsed_tags))
}

