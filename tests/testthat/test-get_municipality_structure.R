test_that("get_municipality_structure validates input parameters correctly", {
  # Test invalid input types
  expect_error(
    get_municipality_structure("2020"), 
    "Both start_year and end_year must be numeric."
  )
  
  expect_error(
    get_municipality_structure(2020, "2021"), 
    "Both start_year and end_year must be numeric."
  )
  
  # Test non-single values
  expect_error(
    get_municipality_structure(c(2020, 2021)), 
    "start_year and end_year must be single values."
  )
  
  expect_error(
    get_municipality_structure(2020, c(2021, 2022)), 
    "start_year and end_year must be single values."
  )
  
  # Test non-integer values
  expect_error(
    get_municipality_structure(2020.5), 
    "start_year and end_year must be integers."
  )
  
  expect_error(
    get_municipality_structure(2020, 2021.3), 
    "start_year and end_year must be integers."
  )
  
  # Test non-four-digit years
  expect_error(
    get_municipality_structure(20), 
    "Both start_year and end_year must be four-digit years."
  )
  
  expect_error(
    get_municipality_structure(2020, 202), 
    "Both start_year and end_year must be four-digit years."
  )
  
  expect_error(
    get_municipality_structure(20200), 
    "Both start_year and end_year must be four-digit years."
  )
  
  # Test start_year > end_year
  expect_error(
    get_municipality_structure(2022, 2020), 
    "start_year must be less than or equal to end_year."
  )
})
