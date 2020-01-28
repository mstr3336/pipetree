#' Default Path to Search for Local Configuration
#'
#' @description
#' This function returns a default for the local config file.
#' This is given by the following:
#'
#' ```r
#' file.path(here::here(), "maps", "paths", "local.yaml")
#' ```
#'
#' That is:
#'
#' *Assuming* __.__ *refers to the root of the current Rproject:*
#'
#' "./maps/paths/local.yaml"
#'
#' @export
default_config_path <- function() {
  return(file.path(here::here(), "maps", "paths", "local.yaml"))
}

#' @export
get_portrpath <- function(config_path = default_config_path()){
  glue <- glue::glue
  `%//%` <- pathlibr::`%//%`
  here <- here::here
  paths <- portrpaths::PortrPath$new(glue("{here::here()}/maps/paths/local.yaml"))
  return(paths)
}

#' Get a list of the main paths to be used
#' @family paths
#' @export
get_paths <- function(config_path = default_config_path()){
  glue <- glue::glue
  `%//%` <- pathlibr::`%//%`
  here <- here::here

  # Setup paths ======================================================

  paths <- get_portrpath(config_path = config_path)

  root <- paths$root %>% pathlibr::Path$new()

  if (!dir.exists(root$show)) stop("Root not setup!")

  pipeline <- root$.$pipeline

  raw <- pipeline$join("raw")

  preprocessed <- pipeline$join("preprocessed")

  processed <- pipeline$join("processed")

  unclean <- preprocessed$join('unclean')

  partition <- pipeline$join("partition")

  out <- list(
    pipeline = pipeline,
    raw = raw,
    preprocessed = preprocessed,
    unclean = unclean,
    partition = partition,
    root = root
  )

  for (in_dir in out){
    if (! dir.exists(in_dir$show)){
      dir.create(in_dir$show, recursive = TRUE)
    }
  }

  return(out)
}

#' Load the Drake Cache for the Project
#'
#' Given the root directory of the project, return the cache to be used by
#' drake.
#'
#' @details
#' The cache will be of the SQLite implementation, to improve performance on
#'  the lustre file system of the HPC.
#'
#'  This isn't intinsically threadsafe, so in `drake::config()`, be sure to
#'  set `caching = "master"`, as per
#'  [drake's documentation](https://ropenscilabs.github.io/drake-manual/storage.html#database-caches)
#'
#' @param root the project root (Top level directory containing "pipeline" directory), defaults to
#'        whatever root is currently set by [load_portrpaths()]
#' @family paths
#' @export
get_cache <- function(root = NULL){
  if (rlang::is_empty(root)) root <- get_paths() %>% .$root

  g <- glue::glue
  root %<>% pathlibr::Path$new()

  db_path <- root$join("pipeline")$join("drake_cache.sqlite")

  L$info(g("Configuring drake to use an SQL based cache @ {db_path$show}"))

  db_for_cache <- DBI::dbConnect(RSQLite::SQLite(), db_path$show)

  db_cache <- storr::storr_dbi("datatable", "keytable", db_for_cache)

  db_cache
}
