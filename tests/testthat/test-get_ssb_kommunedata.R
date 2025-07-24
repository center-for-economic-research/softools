test_that("get_ssb_dta returns a data.frame", {
  skip_on_cran() # Don't run on CRAN because it requires an internet connection
  ssb_data <- get_ssb_dta(
    ssb_tbl_id = "07459",
    aar = "2023",
    statistikkvariabel = "Personer",
    kommuner = c("0301"), # Oslo
    params = list(Kjonn = "1", Alder = "010")
  )
  expect_s3_class(ssb_data, "data.frame")
})

test_that("get_ssb_dta fetches correct data and structure", {
  skip_on_cran()
  ssb_data <- get_ssb_dta(
    ssb_tbl_id = "07459",
    aar = "2023",
    statistikkvariabel = "Personer",
    kommuner = c("0301"), # Oslo
    params = list(Kjonn = "1", Alder = "010")
  )

  # Check dimensions
  expect_equal(nrow(ssb_data), 1)

  # Check for expected columns
  expected_cols <- c("Region", "år", "value")
  expect_true(all(expected_cols %in% colnames(ssb_data)))

  # Check that metadata columns are removed
  expect_false("ContentsCode" %in% colnames(ssb_data))
  expect_false("Tid" %in% colnames(ssb_data))
})
