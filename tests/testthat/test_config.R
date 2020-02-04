context("Do the defaults work for finding the config")


test_that("Are able to specify pipetree config by option", {
  td <- tempdir()
  td %<>% pathlibr::Path$new()

  conf_1 <- setup_config_1(td)
  path_1 <- td$join("config1.yaml")

  conf_1 %>% yaml::write_yaml(path_1$show)

  prev_option <- getOption("pipetree.config")

  options(pipetree.config = path_1$show)

  expect_equal(resolve_config_path(), path_1$show)

  path_list <- get_paths()

  expect_equal(path_list$root$show, td$join("conf1")$join("r1")$show)

  options(pipetree.config = prev_option)
})

test_that("Are able to get default config", {
  prev_option <- getOption("pipetree.config")
  options(pipetree.config = NULL)

  expect <- example_local_config()
  expect %>% yaml::write_yaml(default_config_path())

  expect_equal(resolve_config_path(), default_config_path())

  pp <- get_portrpath()

  expect_equal(pp$root, expect$d_root)

  options(pipetree.config = prev_option)
})
