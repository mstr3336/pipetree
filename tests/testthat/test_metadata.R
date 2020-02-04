context("Does metadata extraction work?")

test_that("Are able to instantiate new metadata", {
  td <- tempdir()
  td %<>% pathlibr::Path$new()

  conf_1 <- setup_config_1(td)
  path_1 <- td$join("config1.yaml")

  conf_1 %>% yaml::write_yaml(path_1$show)

  pp <- get_portrpath(path_1$show)

  paths <- get_paths(path_1$show)

  metadata_user_input <- setup_metadata_input()
  expected <- setup_expected_metadata()

  input_index <- 1

  with_mock(
    get_input = function(prompt = "") {
      cat(prompt, sep = "\n")
      out <- metadata_user_input[[input_index]]
      input_index <<- input_index + 1
      return(out)
    },
    expect_equal(get_project_metadata(pp$root), expected)
  )

  unlink(td$show)

})


test_that("Are able to load existing metadata", {
  td <- tempdir() %>% pathlibr::Path$new()

  conf_1 <- setup_config_1(td)
  path_1 <- td$join("config1.yaml")

  pp <- get_portrpath(path_1$show)
  paths <- get_paths(path_1$show)

  to_write <- setup_expected_metadata()

  to_write %>% yaml::write_yaml(paths$root$join("metadata.yaml")$show)

  expect_equal(get_project_metadata(pp$root), to_write)

  unlink(td$show)
})
