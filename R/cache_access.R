can_show_progress_bar <- function(override = NULL) {
  if (rlang::is_logical(override)) return(override)

  out <- getOption("pipetree.progress_bar", TRUE)

  if (!rlang::is_logical(out)) return(TRUE)
  return(out)
}

make_partition_pattern = function(target_set_name) {
  pats <- list(
    suffix = "(_\\d+)?$",
    prefix = "^"
  )
  paste0(pats$prefix, target_set_name, pats$suffix)
}

#' Load and merge all partitions that match a prefix
#'
#' @description
#'
#' Often, for performance and scalability reasons, dataframes across the
#' pipeline are "partitioned" into chunks, and these chunks are processed in
#' parallel.
#'
#' For example, a dataframe, such as `note_data_prepr` might be split into 50
#' partitions.
#'
#' Each partition is named like so: `note_data_prepr_{partition_index}`, eg,
#' `note_data_prepr_1`, `note_data_prepr_2`, ...  `note_data_prepr_50`.
#'
#' This is useful for the HPC, but acessing the full dataset is a pain for users.
#' This function allows a user to specify the table name they want to extract, eg
#' `note_data_prepr`, and each partition `note_data_prepr_``1`,`2`,`3` etc will be
#' loaded and row binded.
#'
#' @export
#' @param ... names of the target collections, as names (symbols) or chracter
#'       strings, specifying the prefix of each parition set, that directly
#'       precedes a given subpartition's index.
#'       For example, to load and combine `journey_analysis_base_1`,
#'       `journey_analysis_base_2`, ..., `journey_analysis_base_50`, supply
#'       `journey_analysis_base` as an argument.
#' @param cache the [drake::drake_cache] object from which the values will be
#'              retrieved
#' @return a named list of dataframes retrieved from the cache and combined.
#'         Each dataframe is named according the the input prefix.
#' @family cache_access
load_merged_partitions <- function(
  ...,
  cache = NULL
) {
  args <- rlang::enexprs(...)

  is_string_or_symbol <- function(x) rlang::is_string(x) | rlang::is_symbol(x)
  valid_inputs <- args %>% purrr::map_lgl(is_string_or_symbol)

  if (!all(valid_inputs)) {
    invalid_inputs <- args[!valid_inputs]
    msg <- glue::glue(
      "Inputs must be characters, or unquoted names (symbols), I recieved ",
      "{pretty_string(invalid_inputs)}"
      )
    stop(msg)
  }

  args %<>% purrr::map_chr(rlang::as_name)

  cached_list <- get_cache_list(cache)


  out <- args %>%
    purrr::set_names() %>%
    purrr::map(fetch_and_combine, identity, .cached_list = cached_list, .cache = cache)

  out
}

get_cache_list <- function(cache) {
  where <- "{connection printout not implemented}"
  switch (cache$driver$con_type,
          SQLiteConnection = {
            where <- RSQLite::dbGetInfo(cache$driver$con)$dbname
          }
  )

  L$debug("Fetching list of targets from %s", where)

  # cache$list() seems a lot faster than drake::cached(cache)
  cached_list <- cache$list()

  L$debug("Fetched list of targets from %s", where)
  cached_list
}


check_cache_hashes <- function(target_set_name, cache) {

  pattern <- make_partition_pattern(target_set_name)

  object_list <- cache$list()

  object_list %<>% stringr::str_subset(pattern)

  object_list %<>% cache$mget(namespace = "meta")

  object_names <- object_list %>%
    purrr::map_chr("target", .default = NA_character_)

  object_list %<>%
    .[!is.na(object_names)] %>%
    rlang::set_names(object_names %>% .[!is.na(.)]) %>%
    purrr::map_chr("hash")

  object_list
}

# Helpers For Iterating over data #######
load_and_combine <- function(already_combined, df_name, .f, ..., .pb = NULL, .target_set_name = NULL, .cache = NULL) {
  `%||%` <- rlang::`%||%`

  if (rlang::is_empty(.cache)) stop("give me a cache")

  dots <- rlang::enquos(...)

  L$debug("Appending '%s' to merged df, nrow before: %s",
          df_name,
          nrow(already_combined) %||% 0
  )

  out <- drake::readd(df_name, character_only = TRUE, cache = .cache)

  f_call <- rlang::call2(.f, out,!!!dots)
  out <- rlang::eval_tidy(f_call)

  if ("progress_bar" %in% class(.pb)) {
    .pb$tick(tokens = list(what = .target_set_name))
  }

  out <- data.table::rbindlist(list(already_combined, out))
  return(out)
}


fetch_and_combine <- function(target_set_name, .f, ..., .cached_list = c(), .cache = NULL) {
  L$info("Combining %s", target_set_name)

  dots <- rlang::enquos(...)

  pattern <- make_partition_pattern(target_set_name)

  table_partitions <- .cached_list %>% stringr::str_subset(pattern)

  pb <- NULL
  if (can_show_progress_bar()) {
    pb <- progress::progress_bar$new(
      format = "  fetching :what [:bar] :percent eta :eta",
      total = length(table_partitions),
    )

    pb$tick(0, tokens = list(what = target_set_name))
  }

  combined <- table_partitions %>%
    purrr::reduce(load_and_combine, .f, !!!dots, .pb = pb, .target_set_name = target_set_name, .cache = .cache,.init = NULL)

  return(combined)
}

# Fetch from local cache =======

#' Fetch a target locally if cached, otherwise fetch from remote and cache.
#'
#' Given the name of the target/target set, and the `remote_cache`, this will
#' query the target's hash*(es)* from the remote, and then fetch it only if
#' a locally cached copy doesn't already exist.
#'
#' @details
#' This uses a very simple cache implementation, from `xfun::rds_cache()`.
#'
#' It adds a directory, `.cfetch_cache`, to the top level of the project.
#'
#' `xfun::rds_cache()` describes how to configure this.
#'
#' I may add more caching params if necessary later.
#'
#' `remote_cache` is distinct from the local, `xfun::rds_cache()`
#' @seealso xfun::rds_cache()
#' @family cache_access
#' @param target_set_name a __single__ quoted `"string"`, providing the name of the
#'        target, or cluster of targets.
#' @param remote_cache the [drake::drake_cache] object from which the target
#'        will be retrieved, and the metadata queried.
#' @param modify_rbuildignore boolean - If `TRUE`, the project's `.Rbuildignore`
#'        will be searched for a pattern to ignore the cache, and if not found,
#'        adds it.
#' @export
cfetch <- function(target_set_name, remote_cache, modify_rbuildignore = T) {
  target_set_name <- rlang::expr(!!target_set_name)

  cache_name <- "cfetch_cache"
  dir <- file.path(here::here(), paste0(".", cache_name, "/"))

  metadata_name = "ABOUT_THIS_CACHE"

  out <- xfun::cache_rds({
    res <- pipetree::load_merged_partitions(
      !!target_set_name, cache = remote_cache) %>%
      .[[1]]
    res
  },
  hash = check_cache_hashes(target_set_name, remote_cache),
  file = target_set_name,
  dir = dir
  )

  if (dir.exists(dir)) {
    writeLines(c(
      "# Ignore everything in this cache",
      "*"
      ),
      con = file.path(dir, ".gitignore"))

    writeLines(c(
      "This cache is used by pipetree::cfetch",
      "It is typically used in interactive explorations",
      "to minimize repeated reads of remote drake caches.",
      "Ideally, you should see this directory ignored in",
      ".Rbuildignore"
    ),
    con = file.path(dir, metadata_name))

    cache_ignore_pat <- paste0("^.*", "\\.", cache_name, "/.*")

    # Early exist if you're not to touch build ignore
    if (!modify_rbuildignore) return(out)

    rb_ignore <- file.path(here::here(), ".Rbuildignore")
    if (!file.exists(rb_ignore)) {
      file.create(rb_ignore)
    }

    which_line <- grep(
      cache_ignore_pat,
      readLines(rb_ignore),
      fixed = TRUE)

    if (rlang::is_empty(which_line)) {
      cat(
        c("\n", cache_ignore_pat, "\n"),
        sep = "",
        file = rb_ignore,
        append = TRUE
        )
    }

  }



  out
}

# Fetch with mapping function ======

map_fetch <- function(.target_set_name, .f, ..., .cache = NULL) {
  dots <- rlang::enquos(...)

}

# Export from cache =====

export_single_target <- function(target_name, dir_out, cache) {
  target <- drake::readd(target_name, character_only = TRUE, cache = cache)


  path_out <- paste0(target_name, ".qs")
  path_out <- file.path(dir_out, path_out)

  L$info("Saving %s => %s", target_name, path_out)

  target %>% qs::qsave(path_out, nthreads = 4)
}


#' Export deidentified notes
#'
#' Export deidentified notes as rds from the cache to a directory
#' This function is to be used to provide deidentified inputs to
#' the pipeline (Which were previously outputs of the pipeline),
#' in order to fully remove identifiable data from the pipeline.
#'
#' @export
#' @inheritParams export_target_set
export_deidentified_notes <- function(dir_out, cache) {
  target_set_name <- "notes_deidentified"
  export_target_set(target_set_name, dir_out, cache)

  invisible(NULL)
}

#' Export all partitions of a table to file
#'
#' Save all partitions of a particular table to a directory.
#' Presently saves using the `qs` format, as this is meant to be fast (ooh).
#' For now I'm going to default it to using 4 threads
#'
#' @export
#' @param dir_out the path to the directory under which to save the targets
#' @inheritParams load_merged_partitions
#' @inheritParams cfetch
export_target_set <- function(target_set_name, dir_out, cache)  {

  pattern <- make_partition_pattern(target_set_name)

  targets <- cache$list()

  targets %<>% stringr::str_subset(pattern)

  targets %>%
    purrr::walk(~ export_single_target(., dir_out = dir_out, cache = cache))
}
