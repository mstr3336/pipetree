get_input <- function(prompt = "") {
  return(readline(prompt = prompt))
}

#' Describe a function that produces a list
#' @keywords internal
#' @export
describe_list_function <- function(fn) {
  return(fn() %>% yaml::as.yaml())
}
