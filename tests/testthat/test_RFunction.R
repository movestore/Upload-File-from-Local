library("move2")

test_data <- readRDS(here("tests/testthat/data/input3_move2.rds")) # file must be move2!


test_that("execute function rds: TRUE, csv: TRUE, data:TRUE", {
  actual <- rFunction(
    data = test_data,
    coords = "location.long,location.lat",
    track_id_col = "trackId",
    track_attr = "taxon.canonical.name,nick.name,sex,tag.id"
  )
  expected_count <- 14182
  expect_equal(nrow(actual), expected_count)
})


test_that("execute function rds: FALSE, csv: TRUE, data:TRUE", {
  toggle_rds()

  actual <- rFunction(
    data = test_data,
    coords = "location.long,location.lat",
    track_id_col = "trackId",
    track_attr = "taxon.canonical.name,nick.name,sex,tag.id"
  )

  expected_count <- 6412
  expect_equal(nrow(actual), expected_count)
  toggle_rds(hide = F)
})


test_that("execute function rds: TRUE, csv: FALSE, data:TRUE", {
  toggle_csv()

  actual <- rFunction(
    data = test_data,
    coords = "location.long,location.lat",
    track_id_col = "trackId",
    track_attr = "taxon.canonical.name,nick.name,sex,tag.id"
  )
  expected_count <- 10939
  expect_equal(nrow(actual), expected_count)
  toggle_csv(hide = F)
})



test_that("execute function rds: FALSE, csv: FALSE, data: TRUE", {
  toggle_both(hide = T)
  actual <- rFunction(
    data = test_data,
    coords = "location.long,location.lat",
    track_id_col = "trackId",
    track_attr = "taxon.canonical.name,nick.name,sex,tag.id"
  )
  expected_count <- nrow(test_data)
  expect_equal(nrow(actual), expected_count)
  toggle_both(hide = F)
})



test_that("execute function rds: TRUE, csv: FALSE, data: FALSE", {
  toggle_csv()
  actual <- rFunction(
    data = NULL,
    coords = "location.long,location.lat",
    track_id_col = "trackId",
    track_attr = "taxon.canonical.name,nick.name,sex,tag.id"
  )
  expected_count <- 7770
  expect_equal(nrow(actual), expected_count)
  toggle_csv(hide = F)
})


test_that("execute function rds: FALSE, csv: TRUE, data: FALSE", {
  toggle_rds()
  actual <- rFunction(
    data = NULL,
    coords = "location.long,location.lat",
    track_id_col = "trackId",
    track_attr = "taxon.canonical.name,nick.name,sex,tag.id"
  )
  expected_count <- 3243
  expect_equal(nrow(actual), expected_count)
  toggle_rds(hide = F)
})


test_that("execute function rds: FALSE, csv: FALSE, data: FALSE", {
  toggle_both(hide = T)
  actual <- rFunction(
    data = NULL,
    coords = "location.long,location.lat",
    track_id_col = "trackId",
    track_attr = "taxon.canonical.name,nick.name,sex,tag.id"
  )
  expect_null(actual)
  toggle_both(hide = F)
})