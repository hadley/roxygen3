roc_param <- roccer("param",
  roc_parser(name_desc_tag()),
  rd_out(function(tag) {
    new_command("arguments", tag)
  })
)