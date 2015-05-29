context("dplyr compatibility")

test_that("dplyr::select works on unitted_data.frames", {
  df <- u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z"))
  expect_equal(dplyr::select(df, a=y, x), u(dplyr::select(v(df), a=y, x), c("Y","X")))
})