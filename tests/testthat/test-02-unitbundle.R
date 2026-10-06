context("unitbundle")

test_that("unitbundles can be created", {
  
  # Empty units constructions
  expect_equal(new("unitbundle"), unitbundle())
  # Explicit parameter names
  expect_equal(unitbundle(unitdf=data.frame(Unit=character(), Power=numeric(), stringsAsFactors=FALSE)), unitbundle())
  expect_equal(unitbundle(unitdf=data.frame(Unit=character(), Power=numeric())), unitbundle())
  expect_equal(unitbundle(unitstr=NA), unitbundle())
  # Implicit parameter format
  expect_equal(unitbundle(data.frame(Unit=character(), Power=numeric(), stringsAsFactors=FALSE)), unitbundle())
  expect_equal(unitbundle(NA), unitbundle())
  expect_equal(unitbundle(""), unitbundle())
  expect_equal(unitbundle(unitbundle()), unitbundle())
  
  # Non-empty units constructions & validation
  # Explicit parameter names
  expect_equal(unitbundle(unitdf=data.frame(Unit=c("hi","mom"), Power=c(5,-0.1), stringsAsFactors=FALSE)), unitbundle(data.frame(Unit=c("hi","mom"), Power=c(5,-0.1))))
  expect_equal(unitbundle(unitstr="all^0 for^4 one"), unitbundle(data.frame(Unit=c("for","one"),Power=c(4,1))))
  expect_equal(unitbundle(data.frame(Unit=c("all","for","one"), Power=c(0,4,1))), unitbundle(data.frame(Unit=c("for","one"),Power=c(4,1))))
  expect_equal(unitbundle(data.frame(Unit=c("all","for","one"), Power=c(NA,NA,NA))), unitbundle(data.frame(Unit=c("for","all","one"),Power=NA)))
  expect_error(unitbundle(data.frame(Unit=c("all","for","one"))), "unitdf must have columns Unit and Power")
  expect_error(unitbundle(data.frame(Power=c(0,4,1))), "unitdf must have columns Unit and Power")
  # Implicit parameter format & unit sorting
  expect_equal(unitbundle(data.frame(Unit=c("one","two","three","four"), Power=c(1,-3,2,7.4))), unitbundle(data.frame(Unit=c("four","three","two","one"), Power=c(7.4,2,-3,1))))
  expect_equal(unitbundle("a^2 b^-1 c^5 d^6.3"), unitbundle("b^-1 a^4 d^6 c^5 a^-2 d^.3"))
  expect_error(unitbundle(data.frame(Power=c(1,-3,2,7.4), Unit=c("one","two","three","four"))), "unitdf must have columns Unit and Power")
  expect_equal(unitbundle(data.frame(Unit=c("one","two","three","four"), Power=c(1,-3,2,7.4))), unitbundle("four^7.4 one^1 three^2 two^-3"))
  
  # Parsing (see test-01-parse for more rigorous tests; here test that delimiter gets passed through)
  expect_equal(unitbundle("|tree house|^2"), unitbundle(data.frame(Unit="tree house", Power=2)))
  
})

test_that("validObject works", {
  
  expect_true(validObject(unitbundle()))
  expect_true(validObject(unitbundle(unitstr="kg ha^-1 yr^-2")))
  expect_error(validObject({ub <- unitbundle(); names(ub@unitdf) <- c("Bombs","Away"); ub}), "unitdf should contain exactly the columns Unit and Power")
  expect_error(validObject({ub <- unitbundle(); ub@unitdf <- data.frame(Unit=c("one","two","three","four"), Power=c(1,-3,2,7.4)); ub}), "Unit should be of type 'character'")
  expect_error(validObject({ub <- unitbundle(data.frame(Unit=c("one","two","three","four"), Power=c(1,-3,2,7.4))); ub@unitdf$Power <- ub@unitdf$Unit; ub}), "Power should be numeric")
  expect_error(validObject({ub <- unitbundle(unitstr="kg ha^-1 yr^-2"); ub@unitdf <- ub@unitdf[c(3,1,2),]; ub}), "unitdf should always be sorted")

})

test_that("unitbundles can be inspected", {
  
  expect_true(isTRUE(is.na(get_units(5))))
  expect_true(all(get_units(list(unitbundle(),unitbundle()))==""))
  expect_equal(get_units(unitbundle()), "")
  expect_equal(get_units(unitbundle(data.frame(Unit=c("joe","button"), Power=NA))), "button^NA joe^NA")
  expect_equal(get_units(unitbundle(data.frame(Unit=c("uno","dos","tres"), Power=c(-3,2.4,99)))), "dos^2.4 tres^99 uno^-3")
  
})

test_that("arithmetic works for unitbundles", {
  
  ub <- unitbundle("hi mom^2")
  ub2 <- unitbundle("lo mid^-3 hi")
  
  # Addition
  expect_equal(ub + ub, ub)
  expect_error(ub + 2, "Units of e2")
  expect_error(3 + ub, "Units of e2")
  expect_error(ub + ub2)

  # Subtraction
  expect_equal(ub - ub, ub)
  expect_error(ub - 2, "Units of e2")
  expect_error(3 - ub, "Units of e2")
  expect_error(ub - ub2)
  
  # Multiplication
  expect_equal(ub * ub, unitbundle("hi^2 mom^4"))
  expect_equal(ub * 2, ub)
  expect_equal(3 * ub, ub)
  expect_equal(ub * ub2, unitbundle("lo mid^-3 hi^2 mom^2"))
  
  # Division
  expect_equal(ub / ub, unitbundle())
  expect_equal(ub / 2, ub)
  expect_equal(3 / ub, unitbundle("hi^-1 mom^-2"))
  expect_equal(ub / ub2, unitbundle("lo^-1 mid^3 mom^2"))
  
  # Exponentiation
  expect_error(ub ^ ub)
  expect_true(is.na(ub ^ unitbundle()))
  expect_equal(ub ^ 2, ub * ub)
  expect_error(3 ^ ub)
  expect_true(is.na(3 ^ unitbundle()))
  
  # Modulo %%  
  expect_equal(7 %% 2, 1)
  expect_equal(ub %% ub, ub)
  expect_equal(ub %% ub2, ub)
  expect_equal(ub %% 2, ub)
  expect_equal(3 %% ub, unitbundle())
  
  # Integer division %/%
  expect_equal(7 %/% 2, 3)
  expect_equal(ub %/% ub2, ub / ub2)
  expect_equal(ub %/% unitbundle(), ub)
  expect_equal(ub %/% 2, ub)
  expect_equal(3 %/% ub, 1 / ub)
  
})

