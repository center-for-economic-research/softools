test_that("Municipality structure is output as a data frame", {
  expect_s3_class(
    kmn_inndeling("2020-01-01"),
    "data.frame"
  )
}
)
