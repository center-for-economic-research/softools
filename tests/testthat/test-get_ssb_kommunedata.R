# Test for NAstatus column when present
test_that("get_ssb_dta returns NAstatus column when expected", {
  skip_on_cran()
  # Fetch data for a table that includes NAstatus
  ssb_data <- get_ssb_dta(
    ssb_tbl_id = "04469",
    aar = "2019",
    statistikkvariabel = "Beboere",
    kommuner = c("3011"), # Hvaler eksisterte ikke i 2019
  )
  expect_true("NAstatus" %in% colnames(ssb_data))
  })


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
