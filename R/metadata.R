#' Get Metadata for a Particular Project
#'
#' @description
#'
#' For a project whose data is rooted at `root`, get the associated metadata.
#' If none exists, query for it interactively.
#'
#' An example metadata file is as follows:
#'
#' @inheritSection metadata_template Metadata Example
#'
#' @inheritParams get_cache
#' @family metadata
#' @export
get_project_metadata <- function(root = NULL) {
  if (rlang::is_empty(root)) root <- get_paths() %>% .$root
  g <- glue::glue

  root %<>% pathlibr::Path$new()
  path <- root$join("metadata.yaml")


  if (!file.exists(path$show) ) metadata <- metadata_handle_missing(path$show)
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

    input = get_input(g("Your value for {nm}: "))
    if (length(input)) out[[nm]] <- input
  }

  out %>% yaml::write_yaml(path$show)

  return(out)
}

#' An example Metadata file for a dataset
#'
#' @description
#'
#' The template is as follows
#' @section Metadata Example
#' @eval describe_list_function(metadata_template)
#'
#' @family metadata
#' @export
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
