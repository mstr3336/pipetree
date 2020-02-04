#' Get Metadata for a Particular Project
#'
#'
get_project_metadata <- function(root = NULL) {
  if (rlang::is_empty(root)) root <- get_paths() %>% .$root
  g <- glue::glue

  root %<>% pathlibr::Path$new()
  path <- root$join("metadata.yaml")


  if (!file.exists(path$show) ) metadata <- metadata_handle_missing(path)
  else metadata <- yaml::read_yaml(path$show)

  L$info("Using metadata file: ")
  L$info("\n%s", yaml::as.yaml(metadata))

  return(metadata)
}

metadata_handle_missing <- function(path) {
  path %<>% pathlibr::Path$new()

  g <- glue::glue
  out <- metadata_template()

  for (nm in names(out)) {
    print(g(
      "",
      "Please enter a value for {nm}.",
      "An example value for metadata[[{nm}]]: ",
      "{out[[nm]]}",
      .sep = "\n"
      ))

    input = readLine(g("Your value for {nm}: "))
    if (length(input)) out[[nm]] <- input
  }

  out %>% yaml::write_yaml(path$show)

  return(out)
}

metadata_template <- function() {
  g <- glue::glue
  out <- list(
    title = "title_of_particular_workflow",
    desc  = g(
      "A short description of the particular workflow.",
      "This is a good place to describe problems with the raw data, its ",
      "extract period, and so on.",
      .sep = "\n"
    ),
    log_path = "relative/path/from/Rproject/root/title_of_particular_workflow"
  )

  return(out)
}
