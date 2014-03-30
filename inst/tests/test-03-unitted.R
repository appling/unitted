context("unitted")

#### u.vector ####

test_that("Vectors of any type can be unitted and deunitted", {
  units <- "mg dm^-3 sec^-1 dm^4 sec"
  
  vvec <- rep(c(T,F,NA),4); # logical
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- rnorm(5); # numeric; double
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- 1L:10L; # integer
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- as.single(rnorm(5)); # single
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- sample(LETTERS,26); # character
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- complex(real=rnorm(7),imaginary=-7:-2); # complex
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- as.raw(40:45); # raw
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- factor(letters[3:9]); # factor
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- ordered(letters[3:9]); # ordered factor
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- rep(parse(text="5*x+2*y==z"),4); # expression
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- Sys.Date()+(-2):6; # Date
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- Sys.time()+1:9; # POSIXct
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- as.POSIXlt(Sys.time()+1:9); # POSIXlt
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- ts(data=rnorm(20), end=20); # POSIXlt
  expect_that(u(vvec,  units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
  
  vvec <- NULL
  expect_that(u(NULL, units), is_a("unitted"))
  expect_that(v(u(vvec, units)), is_identical_to(vvec))
})


#### u.data.frame ####

test_that("data.frame method of unitted() works", {
  df <- data.frame(co=1:4,balt=4:7)
  # expect that u(df,units) adds the units and makes a unitted object
  expect_that(get_units(u(df, c("u1","u2^4"))), is_equivalent_to(c("u1","u2^4")))
  expect_that(get_units(u(df)), is_equivalent_to(c(NA,NA)))
  expect_that(u(df, c("u1","u2^4")), is_a("unitted"))
  # expect that v(u(df,units)) returns a data.frame with no units anywhere
  expect_that(v(u(df, c("u1","u2^4"))), is_identical_to(df))
  
  df2 <- data.frame(alpha=letters[1:4], beta=u(5:8,"keep"), gamma=u(rnorm(4),"overwrite"))
  # expect that u(df) uses existing units
  expect_that(get_units(u(df2)), is_equivalent_to(c(NA,"keep","overwrite")))
  # expect that u(df,c("newunit",NA)) uses c(new, existing) units, and "" counts as new units
  expect_that(get_units(u(df2, c("just",NA,"going"))), is_equivalent_to(c("just","keep","going")))
  expect_that(get_units(u(df2, c("just","","going"))), is_equivalent_to(c("just","","going")))
  
  df3 <- data.frame(raw=raw(5), single=single(5), POSIXct=Sys.time(), POSIXlt=as.POSIXlt(Sys.Date()))
  expect_that(v(u(df3)), is_identical_to(df3))
  expect_that(v(u(df3,c("a","b","c","d"))), is_identical_to(df3))
  expect_that(get_units(u(df3,letters[1:4])), is_equivalent_to(letters[1:4]))
  
  # expect that supplying the wrong number of units gets an error
  expect_that(u(df,letters[1:26]), throws_error())
  expect_that(u(df,"hi mom"), throws_error())
  expect_that(u(df,c(NA,"e","ru")), throws_error())
})

test_that("as.data.frame.unitted() works", {
  # expect that data.frame(u(x)) is just a data.frame, but u(data.frame(x)) is unitted
  expect_that(class(data.frame(Ca=u(1:5,"mg L^-1"))), is_identical_to("data.frame"))
  expect_that(class(u(data.frame(Ca=1:5),"mg L^-1")), is_identical_to(structure("unitted_data.frame", package="unitted")))
  
  # expect that data.frame(u(x)) and u(data.frame(x)) have the same data
  expect_that(data.frame(Ca=u(1:5,"mg L^-1")), is_equivalent_to(u(data.frame(Ca=1:5),"mg L^-1")))
  expect_that(u(data.frame(Ca=u(1:5,"mg L^-1"))["Ca"]), is_identical_to(u(data.frame(Ca=1:5),"mg L^-1")["Ca"]))
  expect_that(data.frame(Ca=u(1:5,"mg L^-1"))$Ca, is_identical_to(u(data.frame(Ca=1:5),"mg L^-1")$Ca))
  expect_that(names(data.frame(Ca=u(1:5,"mg L^-1"))), is_identical_to(names(u(data.frame(Ca=1:5),"mg L^-1"))))
  
  # expect that data.frame(y, u(x)) keeps the units for x
  expect_that(get_units(data.frame(y=1:5, x=u(2:6,"pins"))$x), equals("pins"))
  
})


#### u.array, u.matrix ####

test_that("Arrays and matrices can be unitted with [exactly] 1 unit", {
  test_create_uarray <- function(varray, note, units="mg dm^-3 sec^-1") {
    uarray <- u(varray,units)
    expect_that(uarray, is_a("unitted"), info="u(varray) returned non-unitted object")
    vuarray <- v(uarray)
    expect_that(vuarray, equals(varray), info=paste0("when creating unitted ",note," c(",paste0(varray[1:4],collapse=","),",...)"))
  }
  
  test_create_uarray(array(),"1-NA array")
  test_create_uarray(array(1:30,c(5,2,3)), "numeric array")
  test_create_uarray(array(1:30,c(5,2,3),list(NULL,c("rats","mice"),c("a","t","g"))), "named numeric array")
  test_create_uarray(matrix(sample(letters,45,replace=TRUE),c(3,15)), "character matrix")

  expect_that(u(array(),c("a","b")), throws_error())
  expect_that(u(array(1:2),c("a","b")), throws_error())
})

test_that("Lists can't be unitted", {
  expect_error(u(as.list(rnorm(5)),"rice"),"Lists can't be unitted, although their elements may be.")
})
