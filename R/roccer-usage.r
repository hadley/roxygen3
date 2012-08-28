roc_usage <- roccer("usage",
  roc_parser(
    tag = text_tag(),
    one = function(roc, obj, ...) {
      if (!is.null(roc$usage)) return()

      list(usage = usage(obj$value, obj$name, obj$srcref))
    }
  ),
  rd_out(rd_command("usage"))
)
