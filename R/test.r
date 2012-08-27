test_parse <- function(text, roccers = base_roccers()) {
  rocblocks <- parse_block(text)
  roxy_process(rocblocks, roccers)[[1]]$roc
}

test_output <- function(text, roccers = base_roccers()) {
  rocblocks <- test_parse(text, roccers)  
  rocblocks <- roxy_process(rocblocks, roccers)
  
  roxy_out(rocblocks, roccers)
}
