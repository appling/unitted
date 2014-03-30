context("inspect")

#### is_a(unitted), is.unitted, class(unitted) ####

test_that("Objects are recognized as unitted IFF the outer object is unitted", {
  units <- "mg dm^-3 sec^-1 dm^4 sec"
  
  # vectors
  expect_that(is.unitted(u(101:106, "dalmatians")), is_true()) # numeric
  expect_that(u(rep(c(T,F,NA),4), units), is_a("unitted")) # logical
  expect_that(class(u(Sys.Date()+(-2):6, units)), equals(c("unitted","Date"))) # Date
  expect_that(class(u(Sys.time()+1:9, units)), equals(c("unitted","POSIXct","POSIXt"))) # POSIXct
  expect_that(class(u(as.POSIXlt(Sys.time()+1:9), units)), equals(c("unitted","POSIXlt","POSIXt"))) # POSIXlt
  expect_that(is.unitted(u(as.POSIXlt(Sys.time()+1:9), units)), is_true()) # POSIXlt
  
  # data.frames
  df <- data.frame(z=1:5, y=sample(letters,5))
  expect_that(class(df), equals("data.frame"))
  dfu <- transform(df, x=u(z,"bluebottles"))
  expect_that(class(dfu), equals("data.frame"))
  expect_that(is.unitted(dfu), is_false())
  udf <- u(df, c("hi","mom"))
  expect_that(class(udf), equals(c("unitted","data.frame")))
  expect_that(is.unitted(udf), is_true())
  
  # arrays
  uarr <- u(array(1:60, c(3,5,4)),"bees")
  expect_that(is.unitted(uarr), is_true())
  expect_that(class(uarr), equals("unitted"))
  expect_that(typeof(uarr), equals("integer"))

  # matrices
  umat <- u(matrix(1:60, c(15,4)),"bees")
  expect_that(is.unitted(umat), is_true())
  expect_that(class(umat), equals("unitted"))
  expect_that(typeof(umat), equals("integer"))

  # lists
  expect_that(ulist <- u(list(a=1,b=2,5)), throws_error("Lists can't be unitted, although their elements may be."))
  listu <- list(a=u(1,"lasso"),b=u(2,"spurs"),c=5)
  expect_that(is.unitted(listu), is_false())
  expect_that(is.unitted(listu$b), is_true())  
})


#### get_units ####

test_that("get_units returns a unit string or vector of unit strings", {
  # vectors
  expect_that(get_units(1:5), equals(NA))
  expect_that(get_units(u(1:5,"pumpkins")), equals("pumpkins"))
  
  # data.frames
  df <- data.frame(co=1:4,balt=4:7)
  expect_that(get_units(u(df, c("u1","u2^4"))), is_equivalent_to(c("u1","u2^4")))
  expect_that(get_units(u(df, c("u1",NA))), is_equivalent_to(c("u1","")))
  expect_that(get_units(u(df, NA)), is_equivalent_to(c("","")))
  expect_that(get_units(data.frame(y=1:5, x=u(2:6,"pins"))), equals(NULL)) # this could be changed by implementing get_units.data.frame
  
  # arrays
  expect_that(get_units(u(array(1:300),"dog")), equals("dog"))
  expect_that(get_units(u(array(1:3,dim=c(1,3),dimnames=list("spotted",NULL)),"dog")), equals("dog"))
  
  # matrices
  expect_that(get_units(u(matrix(1:400),"cats")), equals("cats"))
  
  # lists
  expect_that(get_units(list(a=u(5,"golden rings"))), equals(NULL)) # this could be changed by implementing get_units.list
})



#### verify_units ####

test_that("verify_units passes IFF the units are the same", {
  # Behavior that is acceptable but that I might want to change:
  expect_that(verify_units(1:5, NA), throws_error("First value must be unitted"))
  expect_that(verify_units(data.frame(a=1:5, b=u(1:5,"m")),c(NA,"m")), throws_error("First value must be unitted"))
  expect_that(verify_units(data.frame(a=u(1:5,"hi"),b=6:10),c("hi",NA)), throws_error("First value must be unitted"))
  expect_that(verify_units(list(a=u(10:1,"happy new year")),c("happy new year")), throws_error("First value must be unitted"))
  
  ### Defaults: stop on error, return x otherwise
  # vectors
  expect_that(verify_units(u(1:5,"m"),c("m","m")), throws_error("Conflicting dimensions for given units"))
  expect_that(verify_units(u(1:5,"m"),"q"), throws_error("Unexpected units: given 'q', expected 'm'"))
  expect_that(verify_units(u(1:5,"m"),"m"), is_identical_to(u(1:5,"m")))
  # data.frames
  expect_that(verify_units(u(data.frame(a=1:5,b=6:10),c("m","m")),c("q")), throws_error("Conflicting dimensions for given units"))
  expect_that(verify_units(u(data.frame(a=1:5,b=6:10),c("m","m")),c("p","q")), throws_error("Unexpected units"))
  expect_that(verify_units(u(data.frame(a=1:5,b=6:10),c("m","m")),c("m","m")), is_identical_to(u(data.frame(a=1:5,b=6:10),c("m","m"))))
  expect_that(verify_units(data.frame(a=u(1:5,"hi"),b=6:10),c("hi",NA)), throws_error("First value must be unitted")) # could be changed (see above)
  # arrays
  expect_that(verify_units(u(array(1:5),"kids"),c("kiddos","kids")), throws_error("Conflicting dimensions for given units")) # breaks
  expect_that(verify_units(u(array(1:5),"kids"),"kiddos"), throws_error("Unexpected units")) # breaks
  expect_that(verify_units(u(array(1:5),"kids"),c("kids")), is_identical_to(u(array(1:5),"kids"))) # breaks
  # matrices
  expect_that(verify_units(u(matrix(1:6,ncol=3),"kids"),c("kiddos","kids")), throws_error("Conflicting dimensions for given units")) # breaks
  expect_that(verify_units(u(matrix(1:6,ncol=3),"kids"),"kiddos"), throws_error("Unexpected units")) # breaks
  expect_that(verify_units(u(matrix(1:6,ncol=3),"kids"),c("kids")), is_identical_to(u(matrix(1:6,ncol=3),"kids"))) # breaks
  # lists
  expect_that(verify_units(list(a=u(10:1,"happy new year")),c("happy new year")), throws_error("First value must be unitted")) # could be changed (see above)
})


test_that("verify_units options allow flexibility", {
  ### Options
  # return.values=list(x,NULL)
  expect_that(verify_units(u(1:5,"m"), "m", return.values=list(TRUE,FALSE)), is_true())
  expect_that(verify_units(u(1:5,"m"), "m", return.values=list(list(a=1:4,b=5),FALSE)), equals(list(a=1:4,b=5)))
  expect_that(verify_units(u(1:5,"m"), "m", return.values=c(TRUE,FALSE)), is_true())
  expect_that(verify_units(u(1:5,"m"), "M", return.values=c("apples","oranges")), throws_error("Unexpected units"))
  # violation.handler=stop
  expect_that(verify_units(u(1:5,"m"), "M", violation.handler=warning), gives_warning("Unexpected units"))
  expect_that(verify_units(u(1:5,"m"), "M", violation.handler=message), shows_message("Unexpected units"))
  expect_that(verify_units(u(1:5,"m"), "M", violation.handler=function(x){}), equals(NULL))
  expect_that(verify_units(u(1:5,"m"), "M", return.values=c("apples","oranges"), violation.handler=warning), gives_warning("Unexpected units"))
  expect_that(verify_units(u(1:5,"m"), "M", return.values=c("apples","oranges"), violation.handler=function(x){}), equals("oranges"))
})