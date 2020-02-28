
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

}
