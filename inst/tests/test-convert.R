
test_that("as.list works as expected", {
  
  # as.list for vectors should propagate the units to each element of the list by default
  uvec <- u(99:96,"|bottles of beer on the wall|")
  uvec_as_list <- as.list(uvec)
  expect_that(uvec_as_list, is_a("list"))
  expect_that(get_units(uvec_as_list), equals(rep("bottles of beer on the wall", 4)))
  expect_that(get_units(uvec_as_list, recursive=FALSE), equals(NA))
  # Alternative to as.list: If you want to create a list with all the same
  # units, use list(uvec) or u(as.list(v(uvec)), get_unitbundles(uvec)) instead of
  # as.list(uvec)
  expect_that(u(as.list(v(uvec)), get_unitbundles(uvec)), is_a("unitted_list"))
  expect_that(get_units(u(as.list(v(uvec)), get_unitbundles(uvec))), equals("bottles of beer on the wall"))
  
  #as.list(data.frame) should keep the units. 
  udf <- u(data.frame(x=1:3,y=8),c("q","r"))
  expect_that(as.list(udf), is_a("list"))
  expect_that(get_units(as.list(udf), recursive=TRUE), equals(c(x="q", y="r")))
  # Alternative to as.list for data.frames: If you want to drop units, use deunitted(udf).
  expect_that(get_units(as.list(v(udf))), equals(c(x=NA, y=NA)))
  
  #as.list(list) should push in the outer units unless requested to keep the inner units
  uls <- u(list(x=1,y=u("b","letters")), c("q"))
  expect_that(get_units(as.list(uls)), equals(c(x="q",y="q")))
  expect_that(get_units(as.list(uls, push.units=FALSE)), equals(c(x=NA,y="letters")))
  # as.list(uls, push.units=FALSE) equals deunitted(uls, partial=TRUE)
  expect_that(as.list(uls, push.units=FALSE), equals(v(uls, partial=TRUE)))
  # Alternative to as.list(uls): If you want to drop all units, use deunitted(uls)
  expect_that(get_units(v(uls)), equals(c(x=NA, y=NA)))
})