context("dplyr compatibility")

test_that("tbl_df and as_data_frame work for all types", {
  mylist <- list(x = 1:500, y = runif(500), z = 500:1)
  mydf <- data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc"), stringsAsFactors=FALSE)
  myudf <- u(mydf, c("X","Y","Z"))
  
  # as_tibble
  expect_true(is.unitted(u(tibble::tibble(u(3,"bears")))))
  expect_true(!is.unitted(tibble::as_tibble(mydf)))
  expect_true(is.unitted(tibble::as_tibble(myudf)))
  expect_equal(as.data.frame(tibble::as_tibble(myudf)), myudf)
  expect_true(!is.unitted(tibble::as_tibble(mylist)))
  expect_true(!is.unitted(tibble::as_tibble(mylist)))
  expect_equal(as.data.frame(tibble::as_tibble(myudf)), myudf)
  
})

test_that("dplyr::select and rename work on unitted_data.frames and unitted_tbl_dfs", {
  udf <- u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc"), stringsAsFactors=FALSE), c("X","Y","Z"))
  tbldf <- tibble::as_tibble(udf)
  
  # select
  expect_equal(dplyr::select(udf, a=y, x), u(dplyr::select(v(udf), a=y, x), c("Y","X")))
  expect_equal(dplyr::select_(udf, a="y", "x"), u(dplyr::select_(v(udf), a="y", "x"), c("Y","X")))
  expect_equal(dplyr::select(tbldf, a=y, x), u(dplyr::select(v(tbldf), a=y, x), c("Y","X")))
  expect_equal(dplyr::select_(tbldf, a="y", "x"), u(dplyr::select_(v(tbldf), a="y", "x"), c("Y","X")))
  
  # rename
  expect_equal(dplyr::rename(udf, a=y, beta=x), u(dplyr::rename(v(udf), a=y, beta=x), c("X","Y","Z")))
  expect_equal(dplyr::rename_(udf, a="y", beta="x"), u(dplyr::rename_(v(udf), a="y", beta="x"), c("X","Y","Z")))
  expect_equal(dplyr::rename(tbldf, a=y, beta=x), u(dplyr::rename(v(tbldf), a=y, beta=x), c("X","Y","Z")))
  expect_equal(dplyr::rename_(tbldf, a="y", beta="x"), u(dplyr::rename_(v(tbldf), a="y", beta="x"), c("X","Y","Z")))
  
})

test_that("dplyr::mutate works on unitted_data.frames and unitted_tbl_dfs", {
  udf <- u(data.frame(x=1:3, x2=1:3, y=3:5, stringsAsFactors=FALSE), c("mg L^-1","mg L^-1","L s^-1"))
  tbldf <- tibble::as_tibble(udf)
  
  # mutate - data.frame
  expect_equal(dplyr::mutate(udf, z=LETTERS[y])$z, u(c('C','D','E'), NA))
  expect_equal(dplyr::mutate(udf, z=u(LETTERS[y], 'LL'))$z, u(c('C','D','E'), 'LL'))
  expect_equal(dplyr::mutate(udf, k=x+x2), u(dplyr::mutate(v(udf), k=x+x2), c(get_units(udf[c('x','x2','y','x')]))))
  expect_error(dplyr::mutate(udf, z=LETTERS[y], k=x+y), 'Units of e2 are invalid')
  
  # mutate - tbldf
  expect_equal(dplyr::mutate(tbldf, z=LETTERS[y])[['z']], u(c('C','D','E'), NA))
  expect_equal(dplyr::mutate(tbldf, z=u(LETTERS[y], 'LL'))[['z']], u(c('C','D','E'), 'LL'))
  expect_equal(dplyr::mutate(tbldf, k=x+x2), u(dplyr::mutate(v(tbldf), k=x+x2), c(get_units(tbldf[c('x','x2','y','x')]))))
  expect_equal(dplyr::mutate(tbldf, k=x*y)[['k']], tbldf[['x']]*tbldf[['y']])
  expect_error(dplyr::mutate(tbldf, z=LETTERS[y], k=x+y), 'Units of e2 are invalid')
  
})
