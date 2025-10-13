test_that("data preparation returns business.data object (S4)", {
  suppressWarnings(
    expect_s4_class(prepare_data(), "business.data")
  )
})

test_that("error if reponse not specified", {
  suppressWarnings(
    expect_error(prepare_data(response = "some bullshit"))
  )
})

test_that("test if data is returned as a data frame with no empty columns and has a column that gives the date", {
  suppressWarnings(
  expect_equal("data.frame" %in% class(prepare_data()@data), T)
  )
  suppressWarnings(
    expect_equal("Date" %in% sapply(prepare_data()@data, class), T)
  )
  suppressWarnings(
  expect_equal(!all(sapply(prepare_data()@data, function(d) all(is.na(d)))), T)
  )
})
