get_input <- function(prompt = "") {
  return(readline(prompt = prompt))
}

#' Describe a function that produces a list
#' @keywords internal
#' @export
describe_list_function <- function(fn, section_name = "List Function") {
  input <- fn()
  g <- glue::glue

  out <- input %>% yaml::as.yaml()

  out <- g(
    "@section {section_name}:",
    "```yaml",
    "{out}",
    "```",
    "",
    .sep = "\n"
  )

  return(out)
}

#' Log an object in its native print representation (with newlines)
#'
#' @keywords internal
#' @param x the object to be displayed
pretty_string <- function(x, ...){
  out <- utils::capture.output(print(x, ...)) %>%
    glue::glue_collapse(sep = "\n")

  return(out)
}
