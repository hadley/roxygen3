test_process <- function(text, roccers = base_roccers()) {
  rocblocks <- parse_block(text)
  roxy_process(rocblocks, roccers)[[length(rocblocks)]]$roc
}

test_output <- function(text, roccers = base_roccers()) {
  rocblocks <- parse_block(text)
  rocblocks <- roxy_process(rocblocks, roccers)
  
  out <- roxy_out(rocblocks, roccers)
  roxy_postproc(out)
}
