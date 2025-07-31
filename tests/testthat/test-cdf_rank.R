# test functions for helper function cdf_rank
test_that("estimation cdf_rank", {
  expect_equal(
    cdf_rank(25,12,seq(1,30,5)),
    expected = c(0.02275013,0.05667275,0.12167250,0.22662735,0.36944134,0.36944134,0.53320675),
    tolerance = 0.01)
})
