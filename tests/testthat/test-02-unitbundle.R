context("unitbundle")

test_that("unitbundles can be created", {
  
  # Empty units constructions
  expect_that(new("unitbundle"), equals(unitbundle()))
  # Explicit parameter names
  expect_that(unitbundle(unitdf=data.frame(Unit=character(), Power=numeric(), stringsAsFactors=FALSE)), equals(unitbundle()))
  expect_that(unitbundle(unitdf=data.frame(Unit=character(), Power=numeric())), equals(unitbundle()))
  expect_that(unitbundle(unitstr=NA), equals(unitbundle()))
  # Implicit parameter format
  expect_that(unitbundle(data.frame(Unit=character(), Power=numeric(), stringsAsFactors=FALSE)), equals(unitbundle()))
  expect_that(unitbundle(NA), equals(unitbundle()))
  expect_that(unitbundle(""), equals(unitbundle()))
  expect_that(unitbundle(unitbundle()), equals(unitbundle()))
  
  # Non-empty units constructions & validation
  # Explicit parameter names
  expect_that(unitbundle(unitdf=data.frame(Unit=c("hi","mom"), Power=c(5,-0.1), stringsAsFactors=FALSE)), equals(unitbundle(data.frame(Unit=c("hi","mom"), Power=c(5,-0.1)))))
  expect_that(unitbundle(unitstr="all^0 for^4 one"), equals(unitbundle(data.frame(Unit=c("for","one"),Power=c(4,1)))))
  expect_that(unitbundle(data.frame(Unit=c("all","for","one"), Power=c(0,4,1))), equals(unitbundle(data.frame(Unit=c("for","one"),Power=c(4,1)))))
  expect_that(unitbundle(data.frame(Unit=c("all","for","one"), Power=c(NA,NA,NA))), equals(unitbundle(data.frame(Unit=c("for","all","one"),Power=NA))))
  expect_that(unitbundle(data.frame(Unit=c("all","for","one"))), throws_error("unitdf must have columns Unit and Power"))
  expect_that(unitbundle(data.frame(Power=c(0,4,1))), throws_error("unitdf must have columns Unit and Power"))
  # Implicit parameter format & unit sorting
  expect_that(unitbundle(data.frame(Unit=c("one","two","three","four"), Power=c(1,-3,2,7.4))), equals(unitbundle(data.frame(Unit=c("four","three","two","one"), Power=c(7.4,2,-3,1)))))
  expect_that(unitbundle("a^2 b^-1 c^5 d^6.3"), equals(unitbundle("b^-1 a^4 d^6 c^5 a^-2 d^.3")))
  expect_that(unitbundle(data.frame(Power=c(1,-3,2,7.4), Unit=c("one","two","three","four"))), throws_error("unitdf must have columns Unit and Power"))
  expect_that(unitbundle(data.frame(Unit=c("one","two","three","four"), Power=c(1,-3,2,7.4))), equals(unitbundle("four^7.4 one^1 three^2 two^-3")))
  
  # Parsing (see test-01-parse for more rigorous tests; here test that delimiter gets passed through)
  expect_equal(unitbundle("|tree house|^2"), unitbundle(data.frame(Unit="tree house", Power=2)))
  
})

test_that("validObject works", {
  
  expect_that(validObject(unitbundle()), is_true())
  expect_that(validObject(unitbundle(unitstr="kg ha^-1 yr^-2")), is_true())
  expect_that(validObject({ub <- unitbundle(); names(ub@unitdf) <- c("Bombs","Away"); ub}), throws_error("unitdf should contain exactly the columns Unit and Power"))
  expect_that(validObject({ub <- unitbundle(); ub@unitdf <- data.frame(Unit=c("one","two","three","four"), Power=c(1,-3,2,7.4)); ub}), throws_error("Unit should be of type 'character'"))
  expect_that(validObject({ub <- unitbundle(data.frame(Unit=c("one","two","three","four"), Power=c(1,-3,2,7.4))); ub@unitdf$Power <- ub@unitdf$Unit; ub}), throws_error("Power should be numeric"))
  expect_that(validObject({ub <- unitbundle(unitstr="kg ha^-1 yr^-2"); ub@unitdf <- ub@unitdf[c(3,1,2),]; ub}), throws_error("unitdf should always be sorted"))

})

test_that("unitbundles can be inspected", {
  
  expect_that(isTRUE(is.na(get_units(5))), is_true())
  expect_that(all(get_units(list(unitbundle(),unitbundle()))==""), is_true())
  expect_that(get_units(unitbundle()), equals(""))
  expect_that(get_units(unitbundle(data.frame(Unit=c("joe","button"), Power=NA))), equals("button^NA joe^NA"))
  expect_that(get_units(unitbundle(data.frame(Unit=c("uno","dos","tres"), Power=c(-3,2.4,99)))), equals("dos^2.4 tres^99 uno^-3"))
  
})

test_that("arithmetic works for unitbundles", {
  
  ub <- unitbundle("hi mom^2")
  ub2 <- unitbundle("lo mid^-3 hi")
  
  # Addition
  expect_that(ub + ub, equals(ub))
  expect_that(ub + 2, throws_error("Units of e2"))
  expect_that(3 + ub, throws_error("Units of e2"))
  expect_that(ub + ub2, throws_error())

  # Subtraction
  expect_that(ub - ub, equals(ub))
  expect_that(ub - 2, throws_error("Units of e2"))
  expect_that(3 - ub, throws_error("Units of e2"))
  expect_that(ub - ub2, throws_error())
  
  # Multiplication
  expect_that(ub * ub, equals(unitbundle("hi^2 mom^4")))
  expect_that(ub * 2, equals(ub))
  expect_that(3 * ub, equals(ub))
  expect_that(ub * ub2, equals(unitbundle("lo mid^-3 hi^2 mom^2")))
  
  # Division
  expect_that(ub / ub, equals(unitbundle()))
  expect_that(ub / 2, equals(ub))
  expect_that(3 / ub, equals(unitbundle("hi^-1 mom^-2")))
  expect_that(ub / ub2, equals(unitbundle("lo^-1 mid^3 mom^2")))
  
  # Exponentiation
  expect_that(ub ^ ub, throws_error())
  expect_that(is.na(ub ^ unitbundle()), is_true())
  expect_that(ub ^ 2, equals(ub * ub))
  expect_that(3 ^ ub, throws_error())
  expect_that(is.na(3 ^ unitbundle()), is_true())
  
  # Modulo %%  
  expect_that(7 %% 2, equals(1))
  expect_that(ub %% ub, equals(ub))
  expect_that(ub %% ub2, equals(ub))
  expect_that(ub %% 2, equals(ub))
  expect_that(3 %% ub, equals(unitbundle()))
  
  # Integer division %/%
  expect_that(7 %/% 2, equals(3))
  expect_that(ub %/% ub2, equals(ub / ub2))
  expect_that(ub %/% unitbundle(), equals(ub))
  expect_that(ub %/% 2, equals(ub))
  expect_that(3 %/% ub, equals(1 / ub))
  
})

