
setup_metadata_input <- function() {
  text_out <- list(
    title = "title1",
    desc = "desc1",
    log_path = "logs/p1"
  )

  text_out %<>% unlist(use.names = FALSE)

  return(text_out)
}

setup_expected_metadata <- function() {
  out <- list(
    title = "title1",
    desc = "desc1",
    log_path = "logs/p1"
  )
  return(out)
}
