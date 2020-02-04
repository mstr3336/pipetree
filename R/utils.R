get_input <- function(prompt = "") {
  return(readline(prompt = prompt))
}


describe_list_function <- function(fn) {
  return(fn() %>% yaml::as.yaml())
}
