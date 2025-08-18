test_that("filter_municipality_structure retains all original columns and adds no new columns", {
  # Example input data with extra columns
  input_data <- data.frame(
    code = c("0301", "1601", "3001", "3002"),
    year = c(2020, 2018, 2021, 2022),
    value = c(1, 2, 3, 4),
    extra = c("A", "B", "C", "D"),
    stringsAsFactors = FALSE
  )

  # Example municipality structure with extra column
  muni_struct <- data.frame(
    code = c("0301", "1601", "3001", "3002"),
    valid_from = c("1900-01-01", "1900-01-01", "1900-01-01", "2020-01-01"),
    valid_to = c("2100-01-01", "2017-12-31", "2100-01-01", "2021-12-31"),
    name = c("Oslo", "Trondheim", "Halden", "Moss"),
    stringsAsFactors = FALSE
  )

  # Run filter
  result <- filter_municipality_structure(input_data, muni_struct)

  # The result should have the same columns as input_data
  expect_equal(sort(colnames(result)), sort(colnames(input_data)))

  # The result should not have any new columns
  expect_true(all(colnames(result) %in% colnames(input_data)))
})
test_that("filter_municipality_structure filters invalid municipality-year combos", {
  # Example municipality structure
test_structure <- data.frame(
    code = c("0301", "1601", "3001", "3002"),
    valid_from = as.Date(c("1838-01-01", "1838-01-01", "2020-01-01", "2020-01-01")),
    valid_to   = as.Date(c("9999-12-31", "2018-01-01", "9999-12-31", "2020-12-31")),
    stringsAsFactors = FALSE
  )

  # Data with some invalid municipality-year combos
  test_data <- data.frame(
    code = c("0301", "1601", "3001", "3002"),
    year = c(2020, 2018, 2021, 2022),
    stringsAsFactors = FALSE
  )

  # Should only keep Oslo 2020 and Halden 2021
  result <- filter_municipality_structure(test_data, test_structure)
  expect_equal(
    `rownames<-`(result[order(result$code), ], NULL),
    data.frame(code = c("0301", "3001"), year = c(2020, 2021), stringsAsFactors = FALSE)
  )
})

test_that("filter_municipality_structure returns empty if all invalid", {
  test_structure <- data.frame(
    code = c("0301"),
    valid_from = as.Date("2000-01-01"),
    valid_to   = as.Date("2010-01-01"),
    stringsAsFactors = FALSE
  )
  test_data <- data.frame(
    code = c("0301"),
    year = c(2020),
    stringsAsFactors = FALSE
  )
  result <- filter_municipality_structure(test_data, test_structure)
  expect_equal(nrow(result), 0)
})

test_that("filter_municipality_structure works with missing municipality_structure", {
  skip("Requires get_municipality_structure() to be implemented and available")
  test_data <- data.frame(
    code = c("0301"),
    year = c(2020),
    stringsAsFactors = FALSE
  )
  # Should not error if get_municipality_structure is available
  expect_error(
    filter_municipality_structure(test_data),
    NA
  )
})
