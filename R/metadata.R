#' Get Metadata for a Particular Project
#'
#' @description
#'
#' For a project whose data is rooted at `root`, get the associated metadata.
#' If none exists, query for it interactively.
#'
#' An example metadata file is as follows:
#'
#' @eval describe_list_function(metadata_template, section_name = "Metadata Example")
#'
#' @inheritParams get_cache
#' @param interactive use this for noninteractive scripts to handle missing
#'        metadata without user input
#' @param current_package_name the name of the calling package - If not given, will attempt to use
#'                             the package name of whatever function calls this
#' @family metadata
#' @export
get_project_metadata <- function(root = NULL, interactive = TRUE, current_package_name = NULL) {
  if (rlang::is_empty(root)) root <- get_paths() %>% .$root
  g <- glue::glue

  if (rlang::is_empty(current_package_name)) current_package_name <- getPackageName(parent.frame())

  root %<>% pathlibr::Path$new()
  path <- root$join("metadata.yaml")


  if (!file.exists(path$show) ) metadata <- metadata_handle_missing(path$show, interactive = interactive)
  else metadata <- yaml::read_yaml(path$show)

  metadata %<>% metadata_handle_runtime(package_name = current_package_name)

  L$info("Usng metadata file: ")
  L$info("\n%s", yaml::as.yaml(metadata))

  return(metadata)
}

metadata_handle_runtime <- function(metadata, package_name = NULL) {
  repo_head <- git2r::repository_head()
  last_commit <- git2r::last_commit()

  metadata$version <- list()

  metadata$version$commit <- last_commit[c("sha", "author", "message")]
  metadata$version$branch <- repo_head$name

  metadata$version$package <- list(
    name = package_name
  )

  if (rlang::is_empty(metadata$version$package$name)) {
    metadata$version$package$version <- "0.0.0"
  } else {
    metadata$version$package$version <- packageVersion(metadata$version$package$name)
  }

  metadata
}

metadata_handle_missing <- function(path, interactive = TRUE) {
  path %<>% pathlibr::Path$new()
  g <- glue::glue

  if (! interactive ) {
    L$warn(
      "%s",
      g(
        "No metadata file found at {path$show}",
        "and interactive = FALSE",
        "returning NULL metadata", .sep = "\n"))
    return(NULL)
  }

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
#'
#' @inheritSection get_project_metadata Metadata Example
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
    ) %>% as.character(),
    log_path = "relative/path/from/Rproject/root/title_of_particular_workflow"
  )

  return(out)
}
