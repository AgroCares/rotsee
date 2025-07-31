# test functions for helper function cdf_rank
test_that("estimation cf_ind_importance", {
  expect_equal(
    cf_ind_importance(seq(0,1,0.2)),
    expected = c(5,2.5,1.66,1.25,1.0,0.833),
    tolerance = 0.01)
})
