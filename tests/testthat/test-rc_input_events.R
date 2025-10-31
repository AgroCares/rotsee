# test functions for rc_input_events

testthat::source_file("helper-testdata.R")

test_that("rc_input_events combines crops and amendment data correctly", {
  crops <- data.table(
    time = c(1, 1, 2, 2),
    var = c("CDPM", "CRPM", "CDPM", "CRPM"), 
    method = c("add", "add", "add", "add"),
    value = c(100, 200, 75, 150)
  )
  
  amendment <- data.table(
    time = c(1, 1, 2, 2),
    var = c("CDPM", "CRPM", "CDPM", "CRPM"), 
    method = c("add", "add", "add", "add"),
    value = c(50, 75, 60, 80)
  )
  
  
  result <- rc_input_events(crops, amendment)
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("time", "var", "method", "value") %in% names(result)))
  expect_true(nrow(result) > 0)
  expect_true(all(result$time <= 5))
})


test_that("rc_input_events repetition logic works correctly", {
  crops <- data.table(
    time = c(1, 3),
    var = c("CDPM", "CRPM"), 
    method = c("add", "add"),
    value = c(100, 200)
  )
  
  amendment <- data.table(
    time = numeric(0),
    var = character(0),
    method = character(0), 
    value = numeric(0)
  )
  
  
  result <- rc_input_events(crops, amendment)
  
  expect_s3_class(result, "data.table")
  expect_true(max(result$time) <= 10)
  expect_true(nrow(result) >= 2) # at least the repeated events
  expect_true(all(result$time == sort(result$time)))
})
  

test_that("rc_input_events removes helper columns correctly", {
  crops <- data.table(
    time = c(1, 2),
    var = c("CDPM", "CRPM"), 
    method = c("add", "add"),
    value = c(100, 200)
  )
  
  amendment <- data.table(
    time = c(3),
    var = c("CHUM"),
    method = c("add"), 
    value = c(50)
  )
  
  
  result <- rc_input_events(crops, amendment)
  
  expect_false("id" %in% names(result))
  expect_false("year" %in% names(result))
  expect_false("yr_rep" %in% names(result))
  expect_equal(sort(names(result)), c("method", "time", "value", "var"))
})

test_that("rc_input_events orders output by time correctly", {
  crops <- data.table(
    time = c(3, 1, 2),
    var = c("CDPM", "CRPM", "CHUM"), 
    method = c("add", "add", "add"),
    value = c(100, 200, 150)
  )
  
  amendment <- data.table(
    time = c(4, 0.5),
    var = c("CBIO", "CDPM"),
    method = c("add", "add"), 
    value = c(50, 75)
  )
  
  
  result <- rc_input_events(crops, amendment)
  
  expect_true(all(result$time == sort(result$time)))
})

test_that("rc_input_events handles empty datasets", {
  empty_crops <- data.table(
    time = numeric(0),
    var = character(0),
    method = character(0), 
    value = numeric(0)
  )
  
  amendment <- data.table(
    time = c(1),
    var = c("CHUM"),
    method = c("add"), 
    value = c(50)
  )
  
  
  result1 <- rc_input_events(empty_crops, amendment)
  expect_s3_class(result1, "data.table")
  expect_true(nrow(result1) >= 1)
  
  crops <- data.table(
    time = c(1),
    var = c("CDPM"), 
    method = c("add"),
    value = c(100)
  )
  
  empty_amendment <- data.table(
    time = numeric(0),
    var = character(0),
    method = character(0), 
    value = numeric(0)
  )
  
  result2 <- rc_input_events(crops, empty_amendment)
  expect_s3_class(result2, "data.table")
  expect_true(nrow(result2) >= 1)
})



test_that("rc_input_events handles identical entries summation", {
  identical_entry <- data.table(
    time = c(1),
    var = c("CDPM"), 
    method = c("add"),
    value = c(100)
  )
  
  
  result <- rc_input_events(identical_entry, identical_entry)
  
  expect_s3_class(result, "data.table")
  first_entry <- result[time == 1 & var == "CDPM" & method == "add"]
  expect_equal(nrow(first_entry), 1)
  expect_equal(first_entry$value, 200)
})

test_that("rc_input_events preserves decimal precision", {
  crops <- data.table(
    time = c(1.123),
    var = c("CDPM"), 
    method = c("add"),
    value = c(100.456789)
  )
  
  amendment <- data.table(
    time = c(2.789),
    var = c("CHUM"),
    method = c("add"), 
    value = c(50.123456)
  )
  
  
  result <- rc_input_events(crops, amendment)
  
  expect_true(any(abs(result$value - 100.456789) < 1e-10))
  expect_true(any(abs(result$value - 50.123456) < 1e-10))
})

test_that("rc_input_events handles different variable types correctly", {
  crops <- data.table(
    time = c(1, 2),
    var = c("CDPM", "CRPM"), 
    method = c("add", "add"),
    value = c(100.5, 200.7)
  )
  
  amendment <- data.table(
    time = c(1, 3),
    var = c("CHUM", "CBIO"),
    method = c("add", "add"), 
    value = c(50.2, 75.8)
  )
  
  
  result <- rc_input_events(crops, amendment)
  
  expect_true(all(c("CDPM", "CRPM", "CHUM", "CBIO") %in% result$var))
  expect_true(is.numeric(result$value))
  expect_true(is.numeric(result$time))
  expect_true(is.character(result$var))
  expect_true(is.character(result$method))
})



test_that("rc_input_events handles mixed fractional and integer times", {
  crops <- data.table(
    time = c(0.5, 1.5, 2),
    var = c("CDPM", "CRPM", "CHUM"), 
    method = c("add", "add", "add"),
    value = c(100, 200, 150)
  )
  
  amendment <- data.table(
    time = numeric(0),
    var = character(0),
    method = character(0), 
    value = numeric(0)
  )

  
  result <- rc_input_events(crops, amendment)
  
  expect_s3_class(result, "data.table")
  expect_true(any(result$time %% 1 != 0)) # Should preserve fractional times
  expect_true(all(result$time <= 6))
})

test_that("rc_input_events rbind preserves column structure", {
  crops <- data.table(
    time = c(1),
    var = c("CDPM"), 
    method = c("add"),
    value = c(100)
  )
  
  amendment <- data.table(
    time = c(2),
    var = c("CHUM"),
    method = c("add"), 
    value = c(50)
  )
  
  
  result <- rc_input_events(crops, amendment)
  
  # Should contain data from both sources
  expect_true("CDPM" %in% result$var)
  expect_true("CHUM" %in% result$var)
  expect_true(setequal(unique(result$method), "add"))
})