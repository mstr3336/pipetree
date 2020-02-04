
setup_config_1 <- function(dir) {
  dir %<>% pathlibr::Path$new()

  top <- dir$join("conf1")

  out <- list(
    d_root = top$join("r1")$show,
    profiles = list(
      default = top$join("r1")$show,
      r1 = top$join("r1")$show,
      r2 = top$join("r2")$show
    )
  )

  for (profile in out$profiles) {
    dir.create(path = profile, recursive = TRUE, showWarnings = FALSE)
  }

  return(out)
}


setup_local_default_config <- function() {

}
