get_input <- function(prompt = "") {
  return(readline(prompt = prompt))
}

#' Describe a function that produces a list
#' @keywords internal
#' @export
describe_list_function <- function(fn) {
  input <- fn()
  g <- glue::glue

  out <- input %>% yaml::as.yaml()

  out <- g(
    "",
    "```yaml",
    "{out}",
    "```",
    "",
    .sep = "\n"
  )

  return(out)
}
