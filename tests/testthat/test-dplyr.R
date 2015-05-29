context("dplyr compatibility")

test_that("tbl_df and as_data_frame work for all types", {
  mylist <- list(x = 1:500, y = runif(500), z = 500:1)
  mydf <- data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc"))
  myudf <- u(mydf, c("X","Y","Z"))
  
  # tbl_df
  expect_error(tbl_df(u(3,"bears")))
  expect_true(!is.unitted(tbl_df(mydf)))
  expect_true(is.unitted(tbl_df(myudf)))
  expect_equal(as.data.frame(tbl_df(myudf)), myudf)
  
  # as_data_frame
  expect_true(!is.unitted(as_data_frame(mylist)))
  expect_true(!is.unitted(as_data_frame(mylist)))
  expect_true(!is.unitted(as_data_frame(mydf)))
  expect_true(is.unitted(as_data_frame(myudf)))
  expect_equal(as.data.frame(as_data_frame(myudf)), myudf)
  
})

test_that("dplyr::select and rename work on unitted_data.frames and unitted_tbl_dfs", {
  df <- u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z"))
  tbldf <- tbl_df(df)
  
  # select
  expect_equal(dplyr::select(df, a=y, x), u(dplyr::select(v(df), a=y, x), c("Y","X")))
  expect_equal(dplyr::select_(df, a="y", "x"), u(dplyr::select_(v(df), a="y", "x"), c("Y","X")))
  expect_equal(dplyr::select(tbldf, a=y, x), u(dplyr::select(v(tbldf), a=y, x), c("Y","X")))
  expect_equal(dplyr::select_(tbldf, a="y", "x"), u(dplyr::select_(v(tbldf), a="y", "x"), c("Y","X")))
  
  # rename
  expect_equal(dplyr::rename(df, a=y, beta=x), u(dplyr::rename(v(df), a=y, beta=x), c("X","Y","Z")))
  expect_equal(dplyr::rename_(df, a="y", beta="x"), u(dplyr::rename_(v(df), a="y", beta="x"), c("X","Y","Z")))
  expect_equal(dplyr::rename(tbldf, a=y, beta=x), u(dplyr::rename(v(tbldf), a=y, beta=x), c("X","Y","Z")))
  expect_equal(dplyr::rename_(tbldf, a="y", beta="x"), u(dplyr::rename_(v(tbldf), a="y", beta="x"), c("X","Y","Z")))
  
})

test_that("dplyr::mutate works on unitted_data.frames and unitted_tbl_dfs", {
  
})