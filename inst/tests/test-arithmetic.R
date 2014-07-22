context("arithmetic")


#### Ops.unitted ####

{
  u0 <- NA;       
  u1 <- "s q^-2";
  u2 <- "k^-1 R";
  u00 <- c("", "")
  u11 <- c(u1, u1)
  u21 <- c(u2, u1)
  scal <- 3; vec <- rnorm(4)
  u0scal <- u(scal, u0); u0vec <- u(vec, u0)
  u1scal <- u(scal, u1); u1vec <- u(vec, u1)
  u2scal <- u(scal, u2); u2vec <- u(vec, u2)  
  df <- data.frame(co=1:4,balt=4:7)
  u00df <- u(df, u00)
  u11df <- u(df, u11)
  u21df <- u(df, u21)
  mat2D <- matrix(1:12, nrow=4); arr1D <- array(1:4, dim=c(1,4)); arr3D <- array(1:24, dim=c(4,3,2))
  u0mat2D <- u(mat2D, u0); u0arr1D <- u(arr1D, u0); u0arr3D <- u(arr3D, u0)
  u1mat2D <- u(mat2D, u1); u1arr1D <- u(arr1D, u1); u1arr3D <- u(arr3D, u1)
  u2mat2D <- u(mat2D, u2); u2arr1D <- u(arr1D, u2); u2arr3D <- u(arr3D, u2)
  
  produ1u1 <- "s^2 q^-4"       # u1*u1
  divu1u1 <- ""                # u1/u1
  produ1u2 <- "s q^-2 k^-1 R"  # u1*u2
  divu1u2 <- "s q^-2 k R^-1"   # u1/u2
  invu1 <- "s^-1 q^2"          # 1/u1
  powu1scal <- "s^3 q^-6"      # u1^3
  invu11 <- c(invu1, invu1)

}

expect_OpsB <- function(info, uobj, vobj, vunits, OP) {
  expected <- vunits[[match(OP, names(vunits))]]
  expect_that(
    uobj, 
    if(isTRUE(expected=="eUM")) { # error Units Mismatch
      callinfo <- "throws_error('Units of e2 are invalid')"
      throws_error("Units of e2 are invalid")
    } else if(isTRUE(expected=="ePL")) { # error Power Length
      callinfo <- "throws_error('Attempting to raise units to a power of length != 1')"
      throws_error("Attempting to raise units to a power of length != 1")
    } else {
      callinfo <- paste0("equals(u(",paste0(match.call()[[3]], c("(",",",")"), collapse=""),", '",expected,"'))")
      equals(u(vobj, expected))
    },
    paste0("### INFO = ", info, "; OP = ", OP, " ###\n",
           "### CALL = expect_that(", paste0(match.call()[[2]], c("(",",",")"), collapse=""), ", " ,callinfo,") ###")
  )
}



test_that("Ops.unitted works for scalars", {
  #   "+", "-", "*", "/", "^", "%%", "%/%"
  #   "&", "|", "!"
  #   "==", "!=", "<", "<=", ">=", ">"
  for(OP in c("+", "-", "*", "/", "^")) { #}, "%%", "%/%")) {
    op <- get(OP)
    cat(OP,"")
    expect_OpsB("01 op(uscal, uscal), AA, same units", op(u1scal, u1scal), op(scal, scal), c("+"=u1, "-"=u1, "*"=produ1u1, "/"=divu1u1, "^"="eUM"), OP)
    expect_OpsB("02 op(uscal, uscal), AA, diff units", op(u1scal, u2scal), op(scal, scal), c("+"="eUM", "-"="eUM", "*"=produ1u2, "/"=divu1u2, "^"="eUM"), OP)
    
    expect_OpsB("03 op(uscal, scal),  AB, same non-units", op(u0scal, scal), op(scal, scal), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"=u0), OP)
    expect_OpsB("04 op(uscal, scal),  BA, same non-units", op(scal, u0scal), op(scal, scal), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"=u0), OP)
    expect_OpsB("05 op(uscal, scal),  AB, diff units",   op(u1scal, scal), op(scal, scal), c("+"="eUM", "-"="eUM", "*"=u1, "/"=u1, "^"=powu1scal), OP)
    expect_OpsB("06 op(uscal, scal),  BA, diff units",   op(scal, u1scal), op(scal, scal), c("+"="eUM", "-"="eUM", "*"=u1, "/"=invu1, "^"="eUM"),  OP)
    
    expect_OpsB("07 op(uscal, vec),   AB, same non-units", op(u0scal, vec), op(scal, vec), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"="ePL"), OP)
    expect_OpsB("08 op(uscal, vec),   BA, same non-units", op(vec, u0scal), op(vec, scal), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"=u0), OP)
    expect_OpsB("09 op(uscal, vec),   AB, diff units", op(u1scal, vec), op(scal, vec), c("+"="eUM", "-"="eUM", "*"=u1, "/"=u1, "^"="ePL"), OP)
    expect_OpsB("10 op(uscal, vec),   BA, diff units", op(vec, u1scal), op(vec, scal), c("+"="eUM", "-"="eUM", "*"=u1, "/"=invu1, "^"="eUM"), OP)
    
    expect_OpsB("11 op(uscal, df),    AB, same units", op(u0scal, df), op(scal, df), list("+"=u00, "-"=u00, "*"=u00, "/"=u00, "^"="ePL"), OP)
    # known oddity (bug?): df^u0scal adds row names to the resulting matrix, even if df had none to begin with. df^scal does not. 
    if(OP=="^") {
      expect_OpsB("12 op(uscal, df),    BA, same units", op(df, u0scal), {temp <- op(df, scal); rownames(temp) <- row.names(df); temp}, list("+"=u00, "-"=u00, "*"=u00, "/"=u00, "^"=""), OP)
    } else {
      expect_OpsB("12 op(uscal, df),    BA, same units", op(df, u0scal), op(df, scal), list("+"=u00, "-"=u00, "*"=u00, "/"=u00, "^"=""), OP)
    }
    expect_OpsB("13 op(uscal, df),    AB, diff units", op(u1scal, df), op(scal, df), list("+"="eUM", "-"="eUM", "*"=u11, "/"=u11, "^"="ePL"), OP)
    expect_OpsB("14 op(uscal, df),    BA, diff units", op(df, u1scal), op(df, scal), list("+"="eUM", "-"="eUM", "*"=u11, "/"=invu11, "^"="eUM"), OP)

    expect_OpsB("15 op(uscal, mat2D), AB, same non-units", op(u0scal, mat2D), op(scal, mat2D), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"="ePL"), OP)
    expect_OpsB("16 op(uscal, mat2D), BA, same non-units", op(mat2D, u0scal), op(mat2D, scal), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"=u0), OP)
    expect_OpsB("17 op(uscal, mat2D), AB, diff units", op(u1scal, mat2D), op(scal, mat2D), c("+"="eUM", "-"="eUM", "*"=u1, "/"=u1, "^"="ePL"), OP)
    expect_OpsB("18 op(uscal, mat2D), BA, diff units", op(mat2D, u1scal), op(mat2D, scal), c("+"="eUM", "-"="eUM", "*"=u1, "/"=invu1, "^"="eUM"), OP)
    
    expect_OpsB("19 op(uscal, arr1D), AB, same non-units", op(u0scal, arr1D), op(scal, arr1D), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"="ePL"), OP)
    expect_OpsB("20 op(uscal, arr1D), BA, same non-units", op(arr1D, u0scal), op(arr1D, scal), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"=u0), OP)
    expect_OpsB("21 op(uscal, arr1D), AB, diff units", op(u1scal, arr1D), op(scal, arr1D), c("+"="eUM", "-"="eUM", "*"=u1, "/"=u1, "^"="ePL"), OP)
    expect_OpsB("22 op(uscal, arr1D), BA, diff units", op(arr1D, u1scal), op(arr1D, scal), c("+"="eUM", "-"="eUM", "*"=u1, "/"=invu1, "^"="eUM"),  OP)

    expect_OpsB("23 op(uscal, arr3D), AB, same non-units", op(u0scal, arr3D), op(scal, arr3D), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"="ePL"), OP)
    expect_OpsB("24 op(uscal, arr3D), BA, same non-units", op(arr3D, u0scal), op(arr3D, scal), c("+"=u0, "-"=u0, "*"=u0, "/"=u0, "^"=u0), OP)
    expect_OpsB("25 op(uscal, arr3D), AB, diff units", op(u1scal, arr3D), op(scal, arr3D), c("+"="eUM", "-"="eUM", "*"=u1, "/"=u1, "^"="ePL"), OP)
    expect_OpsB("26 op(uscal, arr3D), BA, diff units", op(arr3D, u1scal), op(arr3D, scal), c("+"="eUM", "-"="eUM", "*"=u1, "/"=invu1, "^"="eUM"),  OP)
  }
})

test_that("Bugs in Ops.unitted", {
  miles_vec <- unitted(1:5,"mi")
  -miles_vec

  #sqrt seems not to get applied to elements of a unitted vector
  x <- u(data.frame(a=1:3,b=3:5),c("a","q"))
  expect_equal(v(sqrt(x)), sqrt(v(x)))
})

test_that("Ops.unitted works for vectors", {
  expect_that(u1vec, u1vec,  equals(u(vec, vec, u1)),            info="uvec, uvec,  AA, same units")
  expect_that(u1vec, u2vec,  throws_error("Units of e2 are invalid"), info="uvec, uvec,  AA, diff units")
  expect_that(u1vec, u1scal, equals(u(vec, scal, u1)),           info="uvec, uscal, AB, same units")
  expect_that(u1scal, u1vec, equals(u(scal, vec, u1)),           info="uvec, uscal, BA, same units")
  expect_that(u2vec, u1scal, throws_error("Units of e2 are invalid"), info="uvec, uscal, AB, diff units")
  expect_that(u1scal, u2vec, throws_error("Units of e2 are invalid"), info="uvec, uscal, BA, diff units")
  expect_that(u0vec, scal,   equals(u(vec, scal, u1)),           info="uvec, scal,  AB, same non-units") # breaks
  expect_that(scal, u0vec,   equals(u(scal, vec, u1)),           info="uvec, scal,  BA, same non-units") # breaks
  expect_that(u1vec, scal,   throws_error("Units of e2 are invalid"), info="uvec, scal,  AB, diff units")
  expect_that(scal, u1vec,   throws_error("Units of e2 are invalid"), info="uvec, scal,  BA, diff units")
  expect_that(u0vec, vec,    equals(u(vec, vec, u0)),            info="uvec, vec,   AB, same non-units")
  expect_that(vec, u0vec,    equals(u(vec, vec, u0)),            info="uvec, vec,   BA, same non-units")
  expect_that(u2vec, vec,    throws_error("Units of e2 are invalid"), info="uvec, vec,   AB, diff units")
  expect_that(vec, u2vec,    throws_error("Units of e2 are invalid"), info="uvec, vec,   BA, diff units")
  
  # data.frames
  expect_that(u11df, u1scal, equals(u(df, scal, u11)),           info="udf, uscal, AB, same units")
  expect_that(u1scal, u11df, equals(u(scal, df, u11)),           info="udf, uscal, BA, same units")
  expect_that(u21df, u2scal, throws_error("Units of e2 are invalid"), info="udf, uscal, AB, diff units")
  expect_that(u2scal, u21df, throws_error("Units of e2 are invalid"), info="udf, uscal, BA, diff units")
  expect_that(u11df, u1vec,  equals(u(df, vec, u11)),            info="udf, uvec, AB, same units")
  expect_that(u1vec, u11df,  equals(u(vec, df, u11)),            info="udf, uvec, BA, same units")
  expect_that(u21df, u2vec,  throws_error("Units of e2 are invalid"), info="udf, uvec, AB, diff units")
  expect_that(u2vec, u21df,  throws_error("Units of e2 are invalid"), info="udf, uvec, BA, diff units")
  expect_that(u21df, u21df,  equals(u(df, df, u21)),             info="udf, udf, AA, same units")
  expect_that(u11df, u21df, throws_error("Units of e2 are invalid"), info="udf, udf, AA, diff units")
  expect_that(u00df, scal, equals(u(df, scal, u00)), info="udf, scal, AB, same non-units")
  expect_that(scal, u00df, equals(u(scal, df, u00)), info="udf, scal, BA, same non-units")
  expect_that(u11df, scal, throws_error("Units of e2 are invalid"), info="udf, scal, AB, diff non-units")
  expect_that(scal, u11df, throws_error("Units of e2 are invalid"), info="udf, scal, BA, diff non-units")
  expect_that(u00df, vec, equals(u(df, vec, u00)), info="udf, vec, AB, same non-units")
  expect_that(vec, u00df, equals(u(vec, df, u00)),info="udf, vec, BA, same non-units")
  expect_that(, throws_error("Units of e2 are invalid"), info="udf, vec, AB, diff non-units")
  expect_that(, throws_error("Units of e2 are invalid"), info="udf, vec, BA, diff non-units")
  expect_that(, info="udf, df, AB, same non-units")
  expect_that(, info="udf, df, BA, same non-units")
  expect_that(, throws_error("Units of e2 are invalid"), info="udf, df, AB, diff non-units")
  expect_that(, throws_error("Units of e2 are invalid"), info="udf, df, BA, diff non-units")
  
  units <- c("u1","u2^4")
  udf <- u(df, units)
  expect_that(udf[,1]+3, throws_error("Units of e2 are invalid"))
  expect_that(udf[,1]+udf[,2], throws_error("Units of e2 are invalid"))
  expect_that(udf[,1]+udf[,1], equals(u(df[,1]+df[,1],get_units(udf[,1]))))
  expect_that(udf[,1]+udf[3,1], equals(u(df[,1]+df[3,1],get_units(udf[,1]))))
  expect_that(udf[1:2,]+udf[3:4,], equals(u(df[1:2,]+df[3:4,],get_units(udf))))
  expect_that({plusdf <- udf[1:2,]+udf[3:4,]; get_units(plusdf)}, equals(c(co="u1",balt="u2^4")))
  
  #data.frames can be added even when their column names differ, so we'll let that happen here
  df1 <- data.frame(one=c(1,1),two=c(2,2))
  df2 <- data.frame(three=c(3,3),four=c(6,6))  
  u1 <- c("rats","mice")
  u2 <- c("Beautiful","Day")
  expect_that(u(df1, u1) + u(df2, u1), equals(u(df1 + df2, u1)))
  expect_that(u(df2, u2) + u(df1, u2), equals(u(df2 + df1, u2)))
  expect_that(u(df1, u1) + u(df2, u2), throws_error("Units of e2 are invalid"))
  
  # matrices and arrays
  mat1 <- matrix(1:20,nrow=4)
  mat2 <- matrix(21:40, nrow=4)
  expect_that(u(mat1) + u(mat2,"yo"), throws_error("Units of e2 are invalid"))
  expect_that(u(mat1,"yo") + u(mat2,"yup"), throws_error("Units of e2 are invalid"))
  expect_that(u(mat1,"yo") + u(mat2,"yo"), equals(u(mat1 + mat2, "yo")))
  expect_that(u(mat1,"yo") + u(mat2,"yo"), equals(u(mat1 + mat2, "yo")))
  expect_that(u(7,"yo") + u(mat2,"yo"), equals(u(7 + mat2, "yo")))
  
})

test_that("-.unitted works", {
  # vectors
  vec <- rnorm(150)
  uvec <- u(vec, "s")
  expect_that(uvec[1:20] - uvec[43], equals(u(vec[1:20] - vec[43], "s")))
  expect_that(uvec[1:20] - uvec[43:62], equals(u(vec[1:20] - vec[43:62], "s")))
  expect_that(uvec[1:20] - u(vec[43:62],"R"), throws_error("Units of e2 are invalid"))
  expect_that(uvec[1:20] - 10, throws_error("Units of e2 are invalid"))
  
  # data.frames
  df <- data.frame(co=1:4,balt=4:7)
  udf <- u(df, c("u1","u2^4"))
  expect_that(udf[,1]-3, throws_error("Units of e2 are invalid"))
  expect_that(udf[,1]-udf[,2], throws_error("Units of e2 are invalid"))
  expect_that(udf[,1]-udf[,1], equals(u(df[,1]-df[,1],get_units(udf[,1]))))
  expect_that(udf[,1]-udf[3,1], equals(u(df[,1]-df[3,1],get_units(udf[,1]))))
  expect_that(udf[1:2,]-udf[3:4,], equals(u(df[1:2,]-df[3:4,],get_units(udf))))
  expect_that({plusdf <- udf[1:2,]-udf[3:4,]; get_units(plusdf)}, equals(c(co="u1",balt="u2^4")))
  
  #data.frames can be added even when their column names differ, so we'll let that happen here
  df1 <- data.frame(one=c(1,1),two=c(2,2))
  df2 <- data.frame(three=c(3,3),four=c(6,6))  
  u1 <- c("rats","mice")
  u2 <- c("Beautiful","Day")
  expect_that(u(df1, u1) - u(df2, u1), equals(u(df1 - df2, u1)))
  expect_that(u(df2, u2) - u(df1, u2), equals(u(df2 - df1, u2)))
  expect_that(u(df1, u1) - u(df2, u2), throws_error("Units of e2 are invalid"))
  
  # matrices and arrays
  mat1 <- matrix(1:20,nrow=4)
  mat2 <- matrix(21:40, nrow=4)
  expect_that(u(mat1) - u(mat2,"yo"), throws_error("Units of e2 are invalid"))
  expect_that(u(mat1,"yo") - u(mat2,"yup"), throws_error("Units of e2 are invalid"))
  expect_that(u(mat1,"yo") - u(mat2,"yo"), equals(u(mat1 - mat2, "yo")))
  expect_that(u(mat1,"yo") - u(mat2,"yo"), equals(u(mat1 - mat2, "yo")))
  expect_that(u(7,"yo") - u(mat2,"yo"), equals(u(7 - mat2, "yo")))
  
})

test_that("*.unitted works", {
  # vectors
  vec <- rnorm(150)
  uvec <- u(vec, "s q^-2")
  expect_that(uvec[1:20] * uvec[43], equals(u(vec[1:20] * vec[43], "s^2 q^-4")))
  expect_that(uvec[43] * uvec[1:20], equals(u(vec[43] * vec[1:20], "s^2 q^-4")))
  expect_that(uvec[1:20] * uvec[43:62], equals(u(vec[1:20] * vec[43:62], "s^2 q^-4")))
  expect_that(uvec[1:20] * u(vec[43:62],"R"), equals(u(vec[1:20] * vec[43:62], "s q^-2 R")))
  expect_that(uvec[1:20] * 10, equals(u(vec[1:20] * 10, "s q^-2")))
  expect_that(10 * uvec[1:20], equals(u(vec[1:20] * 10, "s q^-2")))
  
  # data.frames
  df <- data.frame(co=1:4,balt=4:7)
  units <- c("u1","u2^4")
  udf <- u(df, units)
  expect_that(udf*3, equals(u(df*3, units)))
  expect_that(3*udf, equals(u(df*3, units)))
  expect_that(udf*udf[,2], equals(u(df*df[,2], c("u1 u2^4", "u2^8"))))
  expect_that(udf*udf, equals(u(df*df, paste(units,units))))
  expect_that(udf*udf[3,1], equals(u(df*df[3,1], paste(units, "u1"))))
  expect_that(udf[1:2,]*udf[4:3,], equals(u(df[1:2,]*df[4:3,],paste(units,units))))
  
  #data.frames can be multiplied even when their column names differ, so we'll let that happen here
  df1 <- data.frame(one=c(1,1),two=c(2,2))
  df2 <- data.frame(three=c(3,3),four=c(6,6))  
  u1 <- c("rats","mice")
  u2 <- c("Beautiful","Day")
  expect_that(u(df1, u1) * u(df2, u1), equals(u(df1 * df2, paste(u1,u1))))
  expect_that(u(df1, u1) * u(df2, u2), equals(u(df1 * df2, paste(u1,u2))))
  expect_that(u(df1, u1)[,2] * u(df2, u2), equals(u(df1[,2] * df2, paste(u1[2],u2))))
  expect_that(u(df1, u1) * u(df2, u2)[,2], equals(u(df1 * df2[,2], paste(u1,u2[2]))))
  
  # matrices
  mat1 <- matrix(1:20,nrow=4)
  mat2 <- matrix(21:40, nrow=4)
  expect_that(u(mat1) * u(mat2,"yo"), equals(u(mat1*mat2, "yo")))
  expect_that(u(mat1,"yo") * u(mat2,"yup"), equals(u(mat1*mat2, "yup yo")))
  expect_that(u(mat1,"yo") * u(2,"yo"), equals(u(mat1 * 2, "yo yo")))
  expect_that(u(2,"yoo") * u(mat2,"yo"), equals(u(2 * mat2, "yo yoo")))
  expect_that(u(mat1,"yo") * 3, equals(u(mat1 * 3, "yo")))
  expect_that(3 * u(mat2,"yo"), equals(u(3 * mat2, "yo")))
  
  # arrays
  arr1 <- array(1:20, dim=c(2,3,2))
  arr2 <- array(21:40, dim=c(2,3,2))
  arr3 <- array(1:10, dim=c(1,10))
  arr4 <- array(1:10, dim=c(10,1))
  expect_that(u(arr1) * u(arr2,"yo"), equals(u(arr1*arr2, "yo")))
  expect_that(u(arr1,"yo") * u(arr2,"yup"), equals(u(arr1*arr2, "yup yo")))
  expect_that(u(arr1,"yo") * u(2,"yo"), equals(u(arr1 * 2, "yo yo")))
  expect_that(u(2,"yoo") * u(arr2,"yo"), equals(u(2 * arr2, "yo yoo")))
  expect_that(u(arr1,"yo") * 3, equals(u(arr1 * 3, "yo")))
  expect_that(3 * u(arr2,"yo"), equals(u(3 * arr2, "yo"))) 
})
  

test_that("/.unitted works", {
  
  df <- data.frame(x=1:4,y=2:5,z=3:6)
  udf <- u(df, c("x","y","z"))  
  expect_that(udf / u(2,"a"), equals(u(df / 2, c("x a^-1","y a^-1","z a^-1"))))
  expect_that(udf / u(1:6,"a"), equals(u(df / 1:6, c("x a^-1","y a^-1","z a^-1"))))
  
})

test_that("^.unitted works", {
  # vectors
  vec <- rnorm(150)
  uvec <- u(vec, "s q^-2")
  expect_that(uvec^2, equals(u(vec^2, "s^2 q^-4")))
  expect_that(uvec^c(2,4), throws_error("Attempting to raise units to a power of length != 1"))
  expect_that(uvec^"cat", throws_error("non-numeric argument to binary operator"))
  expect_that(uvec^NA, equals(u(vec^NA, list(c("s","q"), c(NA, NA)))))
  
})

test_that("%%, and %/% .unitted work", {
  
})

test_that("&, |, and ! .unitted work", {
  
})

test_that("==, !=, <, <=, >=, and > .unitted work", {
  
})



#### Math.unitted ####

test_that("abs, sign, sqrt.unitted work", {
  
  # vectors
  vec <- rnorm(25)
  units <- "cat rat^1 hat^3"
  uvec <- u(vec,units)
  expect_that(abs(uvec), equals(u(abs(vec),units)))
  expect_that(sign(uvec), equals(u(sign(vec),"")))
  expect_that(sqrt(uvec), gives_warning("NaNs produced"))
  expect_that(suppressWarnings(sqrt(uvec)), equals(suppressWarnings(u(sqrt(vec),"cat^0.5 rat^0.5 hat^1.5"))))
  expect_that(sqrt(abs(uvec)), equals(u(sqrt(abs(vec)),"cat^0.5 rat^0.5 hat^1.5")))
  
  expect_that(floor(uvec), equals(u(floor(vec),units)))
  expect_that(ceiling(uvec), equals(u(ceiling(vec),units)))
  expect_that(trunc(uvec,4), equals(u(trunc(vec,4),units)))
  
  # data.frames
  units <- c(a="pirates",b="ninjas",c="cowboys")
  df <- data.frame(a=rnorm(26),b=sample(1:26),c=27:52)[7:13,]
  make_dfu <- function(df, uns=units) { transform(df, a=u(a,uns['a']), c=u(c,uns['c'])) }
  dfu <- make_dfu(df)
  expect_that(abs(dfu), equals(make_dfu(abs(df))))
  expect_that(sign(dfu), equals(make_dfu(sign(df),c(a="",c=""))))
  expect_that(sqrt(dfu), gives_warning("NaNs produced"))
  expect_that(suppressWarnings(sqrt(dfu)), equals(suppressWarnings(make_dfu(sqrt(df),c(a="pirates^0.5",c="cowboys^0.5")))))
  expect_that(sqrt(abs(dfu)), equals(make_dfu(sqrt(abs(df)),c(a="pirates^0.5",c="cowboys^0.5"))))
  udfu <- u(dfu)
  expect_that(abs(udfu), equals(u(make_dfu(abs(df)))))
  expect_that(sign(udfu), equals(u(make_dfu(sign(df),c(a="",c="")))))
  expect_that(sqrt(udfu), gives_warning("NaNs produced"))
  expect_that(suppressWarnings(sqrt(udfu)), equals(suppressWarnings(u(make_dfu(sqrt(df),c(a="pirates^0.5",c="cowboys^0.5"))))))
  expect_that(sqrt(abs(udfu)), equals(u(make_dfu(sqrt(abs(df)),c(a="pirates^0.5",c="cowboys^0.5")))))
  udf <- u(df,units)
  expect_that(abs(udf), equals(u(abs(df),units)))
  expect_that(sign(udf), equals(u(sign(df),c(a="",b="",c=""))))
  expect_that(sqrt(udf), gives_warning("NaNs produced"))
  expect_that(suppressWarnings(sqrt(udf)), equals(u(suppressWarnings(sqrt(df)),c(a="pirates^0.5",b="ninjas^0.5",c="cowboys^0.5"))))
  expect_that(sqrt(abs(udf)), equals(u(sqrt(abs(df)),c(a="pirates^0.5",b="ninjas^0.5",c="cowboys^0.5"))))
  
  # matrices
  mat <- matrix(rnorm(25),5,5)
  units <- "cat rat^1 hat^3"
  umat <- u(mat,units)
  expect_that(abs(umat), equals(u(abs(mat),units)))
  expect_that(sign(umat), equals(u(sign(mat),"")))
  expect_that(sqrt(abs(umat)), equals(u(sqrt(abs(mat)),"cat^0.5 rat^0.5 hat^1.5")))
  
  # arrays
  arr <- array(rnorm(125),c(5,5,5))
  units <- "umol m^-2 s^-1"
  uarr <- u(arr,units)
  expect_that(abs(uarr), equals(u(abs(arr),units)))
  expect_that(sign(uarr), equals(u(sign(arr),"")))
  expect_that(sqrt(abs(uarr)), equals(u(sqrt(abs(arr)),"umol^0.5 m^-1 s^-0.5")))
  
})

test_that("floor, ceiling, trunc.unitted work", {
  
})

test_that("round, signif.unitted work", {
  
})

test_that("exp, log, expm1, log1p.unitted work", {
  expect_that(get_unitbundles(log(u(1:10,""))), equals(unitbundle("")))
  expect_that(log(u(1:10,"kg")), throws_error("Input units are invalid in log"))
  #expect_that(log(u(1:10,"kg"), check.input.units=FALSE), "this doesn't actually work; check.input.units doesn't get passed through math.unitted")
})

test_that("cos, sin, tan.unitted work", {
  
})

test_that("acos, asin, atan.unitted work", {
  
})

test_that("cosh, sinh, tanh.unitted work", {
  
})

test_that("acosh, asinh, atanh.unitted work", {
  
})

test_that("lgamma, gamma, digamma, trigamma.unitted work", {
  
})

test_that("cumsum, cumprod, cummax, cummin.unitted work", {
  
})


#### Summary.unitted ####

test_that("Summary.unitted works", {
  
})

test_that("all, any.unitted works", {
  
})

test_that("sum, prod.unitted works", {
  
})

test_that("min, max.unitted works", {
  
})

test_that("range.unitted works", {
  
})


#### Complex.unitted ####

test_that("Complex.unitted works", {
  
})

test_that("Arg, Conj, Im, Mod, Re.unitted works", {
  
})

#### Matrix operations ####

test_that("%*% and other matrix operations work", {
  
})