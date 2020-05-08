pipetree_config_env_var <- function() return("PIPETREE_CONFIG_PATH")

#' Default Path to Search for Local Configuration
#'
#' Get the default config path
#' @description
#' This function returns a default for the local config file.
#'
#' @section Default Config Path:
#' This is given by the following:
#'
#' ```r
#' file.path(here::here(), "local.yaml")
#' ```
#'
#' That is:
#'
#' *Assuming* __.__ *refers to the root of the current Rproject:*
#'
#' "./local.yaml"
#'
#' @export
default_config_path <- function() {
  out <- file.path(here::here(), "local.yaml")
  msg <- glue::glue("No path given, using default: {out}")
  L$warn(msg)
  #warning(msg)
  return(out)
}


#' Resolve the configuration path
#'
#' @section Specifying Config path:
#' The pipetree config path can be set using a number of mechanisms.
#'
#' If the path is set in multiple places, the path chosen will
#' prioritize in order of more specific settings befroe less specific.
#'
#' In order of first-used, the config path can be described as below:
#'
#' - Explicitly providing the path in a call to a `pipetree` function
#' - `getOption('pipetree.config')`
#' - The system environment variable `r pipetree:::pipetree_config_env_var()`
#' - The default config path given by [default_config_path()].
#'
resolve_config_path <- function() {
  `%||%` <- rlang::`%||%`
  env_var <- pipetree_config_env_var()


  msg <- glue::glue("No config path supplied")
  L$warn(msg)

  #warning(msg)

  config_path <- getOption("pipetree.config")
  if (!rlang::is_empty(config_path)) {
    L$info("Using getOption('pipetree.config')")
    return(config_path)
  }

  config_path <- Sys.getenv(env_var, unset = NA)
  if (!is.na(config_path)) {
    L$info(glue::glue('Using "${{{env_var}}}"'))
    return(config_path)
  }

  config_path <- default_config_path()

  L$info("Using default_config_path(): ", config_path)
  return(config_path)
}

#' Get the path Configuration object
#'
#' Get the `PortrPath` object for configuring paths, profiles etc.
#' See [portrpaths::PortrPath](https://mstr3336.github.io/portrpaths/articles/configuring-your-paths.html)
#' for more info.
#'
#' @inheritSection default_config_path Default Config Path
#' @inheritSection resolve_config_path Specifying Config path
#' @param config_path the path to the configuration `yaml` file. If not
#'        specified, this will be given by `getOption("pipetree.config")`,
#'        and if this is unset, will be given by [default_config_path()].
#'        (See **Default Config Path** section)
#' @export
get_portrpath <- function(config_path = NULL ) {
  if (rlang::is_empty(config_path)) config_path <- resolve_config_path()

  glue <- glue::glue
  `%//%` <- pathlibr::`%//%`
  here <- here::here
  paths <- portrpaths::PortrPath$new(config_path)
  return(paths)
}

#' Get a list of the main paths to be used
#'
#' @family paths
#' @inheritSection default_config_path Default Config Path
#' @inheritParams get_portrpath
#' @export
get_paths <- function(config_path = NULL) {
  if (rlang::is_empty(config_path)) config_path <- resolve_config_path()

  glue <- glue::glue
  `%//%` <- pathlibr::`%//%`
  here <- here::here

  # Setup paths ======================================================

  paths <- get_portrpath(config_path = config_path)

  root <- paths$root %>% pathlibr::Path$new()

  if (!dir.exists(root$show)) stop("Root not setup!")

  pipeline <- root$join("pipeline")

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
#'        whatever root is currently set by [get_portrpath()]
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


example_local_config <- function() {
  g <- glue::glue

  out <- list(
    d_root = "/default/path/to/dataset",
    info = g(
      "This config is specified in ./vignettes/default_config.yaml, and copied to",
      "./local.yaml whenever the Getting Started vignette is run.",
      "It is intended only as a demonstration", .sep = "\n") %>% as.character(),
    profiles = list(
      default = "/default/path/to/dataset",
      alt = "/alternate/path/to/dataset"
    )
  )
  out
}
