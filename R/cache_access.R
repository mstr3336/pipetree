#' Load and merge all partitions that match a prefix
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

  pats <- list(
    suffix = "(_\\d+)?$",
    prefix = "^"
  )

  cached_list <- drake::cached(cache = cache)

  load_and_combine <- function(already_combined, df_name) {
    out <- drake::readd(df_name, character_only = TRUE, cache = cache)
    out <- dplyr::bind_rows(already_combined, out)
    return(out)
  }


  fetch_and_combine <- function(target_set_name) {
    pattern <- paste0(pats$prefix, tartget_set_name, pats$suffix)

    table_partitions <- cached_list %>% stringr::str_subset(pattern)

    combined <- table_partitions %>%
      purrr::reduce(load_and_combine, .init = NULL)

    return(combined)
  }




  out <- args %>%
    purrr::set_names() %>%
    purrr::map(fetch_and_combine)

  out
}
