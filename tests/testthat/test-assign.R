context("assign")
knownbug <- function(expr, notes) invisible(NULL)

#### [<-.unitted ####

test_that("[<-.unitted works for vectors", {
  # replacing elements with units of "" with non-unitted elements This is a
  # design decision. Assignment and replacement treat non-unitted objects as if
  # they had units of "", which means that non-unitted elements may replace
  # unitted elements that have empty units ("" == NA)
  vvec <- 101:199
  u0vec <- u(vvec, NA)
  expect_that({u0vec[50] <- 5; u0vec}, equals({vvec[50] <- 5; u(vvec, "")}))
  expect_that({u0vec[] <- 1:99; u0vec}, equals({vvec[] <- 1:99; u(vvec, "")}))
  expect_that({u0vec[149] <- 7; u0vec}, equals({vvec[149] <- 7; u(vvec, "")}))
  expect_that({u1vec <- u0vec; u1vec[3] <- 1:20}, gives_warning("number of items to replace is not a multiple of replacement length")) # changes u0vec
  expect_that({u0vec[c(12,15,108)] <- c(1,2,4); u0vec}, equals({vvec[c(12,15,108)] <- c(1,2,4); u(vvec, "")}))
  
  # single element replacement - numeric indices
  vvec <- 101:199
  uvec <- u(vvec, "dbs")
  knownbug(expect_that({uvec[50] <- 5; uvec}, throws_error("Units mismatch in partial replacement")))
  knownbug(expect_that({uvec[50] <- u(5, "flux"); uvec}, throws_error("Units mismatch in partial replacement")))
  expect_that({uvec[50] <- u(5, "dbs"); uvec}, equals({vvec[50] <- 5; u(vvec, "dbs")}))
  expect_that({uvec[4.9] <- u(7, "dbs"); uvec}, equals({vvec[4.9] <- 7; u(vvec, "dbs")}))
  expect_that({uvec[102] <- u(12, "dbs"); uvec}, equals({vvec[102] <- 12; u(vvec, "dbs")}))
  expect_that({uvec[-20] <- u(42, "dbs"); uvec}, equals({vvec[-20] <- 42; u(vvec, "dbs")}))
  expect_that({uvec[3] <- u(1:20, "dbs"); uvec}, gives_warning("number of items to replace is not a multiple of replacement length"))
  
  # multiple element replacement - no indices
  vvec <- 101:199
  uvec <- u(vvec, "dbs")
  knownbug(expect_that({uvec[] <- 1:99; uvec}, throws_error("Units mismatch in partial replacement")))
  expect_that({uvec[] <- u(1:99, "dbs"); uvec}, equals({vvec[] <-1:99; u(vvec, "dbs")}))
  
  # multiple element replacement - numeric indices
  vvec <- (1:100)[c(F,T)]
  uvec <- u(vvec, "dbs")
  knownbug(expect_that({uvec[15:20] <- 3; uvec}, throws_error("Units mismatch in partial replacement")))
  expect_that({uvec[15:20] <- u(3, "dbs"); uvec}, equals({vvec[15:20] <- 3; u(vvec, "dbs")}))
  expect_that({uvec[c(12,15,18)] <- u(c(1,2,4), "dbs"); uvec}, equals({vvec[c(12,15,18)] <- c(1,2,4); u(vvec, "dbs")}))
  expect_that({uvec[c(72,75,78)] <- u(c(1,2,4), "dbs"); uvec}, equals({vvec[c(72,75,78)] <- c(1,2,4); u(vvec, "dbs")}))
  expect_that({uvec[c(-20,-63)] <- u(42, "dbs"); uvec}, equals({vvec[c(-20,-63)] <- 42; u(vvec, "dbs")}))
  expect_that({uvec[c(72,75,78)] <- u(c(1,4), "dbs"); uvec}, gives_warning("number of items to replace is not a multiple of replacement length"))
  
  # From ?Extract: "An empty index selects all values: this is most often
  # used to replace all the entries but keep the attributes."
  vvec <- (1:100)[c(F,T)]
  uvec <- u(vvec, "dbs")
  knownbug(expect_that({uvec[] <- 3; uvec}, throws_error("Units mismatch in partial replacement")))
  expect_that({uvec[] <- u(3, "dbs"); uvec}, equals({vvec[] <- 3; u(vvec, "dbs")}))
  
  # logical indices
  vvec <- (1:100)
  uvec <- u(vvec, "dbs")
  expect_error(uvec[uvec < 24])
  expect_that({uvec[uvec < u(24,"dbs")] <- u(9,"dbs"); uvec}, equals({vvec[vvec < 24] <- 9; u(vvec, "dbs")}))
  expect_that({uvec[c(T,F)] <- u(100:51,"dbs"); uvec}, equals({vvec[c(T,F)] <- 100:51; u(vvec, "dbs")}), info="reassignment indexed by short logical vector") # breaks
  expect_that({uvec[c(F,T)] <- u(1:5,"dbs"); uvec}, equals({vvec[c(F,T)] <- 1:5; u(vvec, "dbs")}), info="reassignment indexed by short logical vector & short value vector") # breaks
  expect_that({uvec[F] <- u(101:105,"dbs"); uvec}, equals({vvec[F] <- 101:105; u(vvec, "dbs")}), info="reassignment indexed by F") # breaks - shouldn't throw error
  
  # character indices
  knownbug(expect_that({uvec["juliuscaesar"] <- 9; uvec}, equals({vvec["juliuscaesar"] <- 9; u(vvec, "dbs")}), info="reassignment in unnamed vector indexed by new string"))
  vvec <- setNames(1:100, paste0(rep(LETTERS[1:25],each=4),rep(letters[1:4],25)))
  uvec <- u(vvec, "dbs")
  knownbug(expect_that({uvec["Fa"] <- 121; uvec}, equals({vvec["Fa"] <- 121; u(vvec, "dbs")}), info="reassignment indexed by existing string") )
  knownbug(expect_that({uvec[c("Fa","La","La")] <- 121; uvec}, equals({vvec[c("Fa","La","La")] <- 121; u(vvec, "dbs")}), info="reassignment indexed by existing strings, 1 value") )
  knownbug(expect_that({uvec[c("Fa","La","La")] <- -5:-3; uvec}, equals({vvec[c("Fa","La","La")] <- -5:-3; u(vvec, "dbs")}), info="reassignment indexed by existing strings, 3 values") )
  knownbug(expect_that({uvec[c("Ba","Tc","Ab")] <- c("go","tree","man"); uvec}, equals({vvec[c("Ba","Tc","Ab")] <- c("go","tree","man"); u(vvec, "dbs")}), info="reassignment indexed by existing strings, 3 values") )
  knownbug(expect_that({uvec["juliuscaesar"] <- 121; uvec}, equals({vvec["juliuscaesar"] <- 121; u(vvec, "dbs")}), info="reassignment in named vector indexed by new string"))
  knownbug(expect_that({uvec["Fa"] <- 100:51; uvec}, throws_error("number of items to replace is not a multiple of replacement length"), info="mismatch in replacee and replacement lengths"))
})


test_that("[<-.unitted works for data.frames", {
  df0 <- data.frame(one=letters[c(6,16,26)], two=5:7+0.5, three=as.POSIXlt(Sys.time()+1:3))
  units <- c("gene","freq","")
  udf0 <- u(df0, units)
  
  # replacement by no indices
  knownbug(expect_that({udf <- udf0; udf[] <- u(df0,c("g","f","h")); udf}, equals({df <- df0; numeric(); u(df,units)}), info="single element replacement should accept same units"))
  
  # replacement by numeric/character indices that works correctly
  expect_that({udf <- udf0; udf[2,2] <- u(10,"freq"); udf}, equals({df <- df0; df[2,2] <- 10; u(df,units)}), info="single element replacement should accept same units")
  expect_that({udf <- udf0; udf['2','two'] <- u(10,"freq"); udf}, equals({df <- df0; df['2','two'] <- 10; u(df,units)}), info="single element replacement should accept same units")
  expect_that({udf <- udf0; udf[3,3] <- df0[1,3]+90; udf}, equals({df <- df0; df[3,3] <- df0[1,3]+90; u(df,units)}), info="replacing elem having units='' with ununittted value should be OK")
  expect_that({udf <- udf0; udf[ ,2] <- 25; udf}, equals({df <- df0; df[ ,2] <- 25; u(df,c(units[1],"",units[3]))}), info="whole column replacement should change units")
  expect_that({udf <- udf0; udf[ ,'two'] <- 25; udf}, equals({df <- df0; df[ ,'two'] <- 25; u(df,c(units[1],"",units[3]))}), info="whole column replacement should change units")
  expect_that({udf <- udf0; udf[ ,1] <- u(c('a','b','c'),'kg'); udf}, equals({df <- df0; df[ ,1] <- c('a','b','c'); u(df,c("kg",units[2:3]))}), info="whole column replacement should change units")
  expect_that({udf <- udf0; udf[2,] <- udf[1,]; udf}, equals({df <- df0; df[2,] <- df[1,]; u(df,units)}), info="whole row replacement should accept matching units")
  expect_that({udf <- udf0; udf['2',] <- udf[1,]; udf}, equals({df <- df0; df['2',] <- df[1,]; u(df,units)}), info="whole row replacement should accept matching units")
  expect_that({udf <- udf0; udf[2,1:2] <- udf[3,1:2]; udf}, equals({df <- df0; df[2,1:2] <- df[3,1:2]; u(df,units)}), info="partial row replacement should accept matching units")
  
  # except for whole columns, replacement of data.frame parts should check units
  knownbug({
    expect_that({udf <- udf0; udf[2,2] <- 10; udf}, throws_error("units mismatch in replacement"), info="single element replacement should check units") #breaks
    expect_that({udf <- udf0; udf[2,] <- df0[1,]; udf}, throws_error("units mismatch in replacement"), info="whole row replacement should check units") # breaks
    expect_that({udf <- udf0; udf[2,1:2] <- df0[3,1:2]; udf}, throws_error("units mismatch in replacement"), info="partial row replacement should check units") # breaks
    expect_that({udf <- udf0; udf[2,] <- u(df0[1,],units[c(3,1,2)]); udf}, throws_error("units mismatch in replacement"), info="whole row replacement should check units") # breaks (sorta) - i'd like a more useful error
    expect_that({udf <- udf0; udf[2,1:2] <- udf[3,2:3]; udf}, throws_error("units mismatch in replacement"), info="partial row replacement should require correct units") # breaks (sorta) - i'd like a more useful error
  })
  
  # deletion by udf[xx] <- NULL
  expect_that({df <- df0; df[2,2] <- NULL; df}, throws_error("replacement has length zero"), info="NULL cannot replace an element") # for comparison to following
  expect_that({udf <- udf0; udf[2,2] <- NULL; udf}, throws_error("replacement has length zero"), info="NULL cannot replace an element")
  knownbug(expect_that({udf <- udf0; udf[2] <- NULL; udf}, equals({df <- df0; df[2] <- NULL; df}), info="columns may be deleted with udf[x] <- NULL"))
  knownbug(expect_that({udf <- udf0; udf[,2] <- NULL; udf}, equals({df <- df0; df[,2] <- NULL; df}), info="columns may be deleted with udf[,x] <- NULL"))
  expect_that({df <- df0; df[2,] <- NULL; df}, throws_error("replacement has 0 items, need 3"), info="rows may NOT be deleted with df[x] <- NULL") # for comparison to following
  expect_that({udf <- udf0; udf[2,] <- NULL; udf}, throws_error("replacement has 0 items, need 3"), info="rows may NOT be deleted with df[x] <- NULL")
  
  # replacement of one element/section with several
  expect_that({df <- df0; df[2,2] <- 1:3; df}, throws_error("replacement has 3 rows, data has 1"), info="replace one element with several") # regular data.frame for comparison
  expect_that({udf <- udf0; udf[2,2] <- 1:3; udf}, throws_error("replacement has 3 rows, data has 1"), info="replace one element with several") # breaks - should use same error as data.frame
  # it should be OK (albeit weird) to replace one row with several (the first wins)
  knownbug(expect_that({udf <- udf0; udf[2,1:2] <- df0[c(1,3),1:2]; udf}, equals({df <- df0; df[2,1:2] <- df[c(1,3),1:2]; u(df,units)}), info="replace one row from several rows"))
  # replacing one column with several should generate an error
  expect_that({df <- df0; df[2,1:2] <- df[1,c(1,2,1,2)]}, gives_warning("provided 4 variables to replace 2 variables"))
  knownbug(expect_that({udf <- udf0; udf[2,1:2] <- udf0[1,c(1,2,1,2)]; udf}, gives_warning("provided 4 variables to replace 2 variables")))
  knownbug(expect_that(udf, equals(u(df,units)), info="this expect_that relies on the previous two. expect that replacement happened despite warnings"))
  
  # replacement by logical indices
  expect_that({udf <- udf0; udf[c(T,F,F),c(F,T,F)] <- udf[3,2]; udf}, equals({df <- df0; df[c(T,F,F),c(F,T,F)] <- df[3,2]; u(df,units)}), info="logical index to replace single element")
  expect_that({udf <- udf0; udf[c(T,T,F),c(F,T,F)] <- udf[c(3,3),2]; udf}, equals({df <- df0; df[c(T,T,F),c(F,T,F)] <- df[c(3,3),2]; u(df,units)}), info="logical index to replace two elements in col")
  expect_that({udf <- udf0; udf[c(T,F,F),c(T,T,F)] <- udf[3,1:2]; udf}, equals({df <- df0; df[c(T,F,F),c(T,T,F)] <- df[3,1:2]; u(df,units)}), info="logical index to replace two elements in col")
  knownbug(expect_that({udf <- udf0; udf[c(T,F,F),] <- udf[3,]; udf}, gives_warning("this shouldn't give a warning"), info="logical index to replace whole row"))
  expect_that({udf <- udf0; udf[c(T,F,F),] <- udf[3,]; udf}, equals({df <- df0; df[c(T,F,F),] <- df[3,]; u(df,units)}), info="logical index to replace whole row") # breaks - shouldn't give warning
  knownbug(expect_that({udf <- udf0; udf[,c(F,T,F)] <- udf[,1]; udf}, gives_warning("this shouldn't give a warning"), info="logical index to replace whole column - shouldn't give warning"))
  expect_that({udf <- udf0; udf[,c(F,T,F)] <- udf[,1]; udf}, equals({df <- df0; df[,c(F,T,F)] <- df[,1]; u(df,units[c(1,1,3)])}), info="logical index to replace whole column") # breaks - shouldn't give warning
  expect_that({df <- df0; df[,F] <- df[,1]; df}, gives_warning("data length exceeds size of matrix"))
  knownbug(expect_that({udf <- udf0; udf[,F] <- udf[,1]; udf}, gives_warning("data length exceeds size of matrix")))
  expect_that({udf <- udf0; udf[F,] <- udf[1,]; udf}, equals({df <- df0; df[F,] <- df[1,]; u(df,units)}), info="replacement of rows=F should give no warning, weirdly")
  
  # addition of new columns: numeric & character indices
  expect_that({udf <- udf0; udf[,4] <- udf[,1]; udf}, equals({df <- df0; df[,4] <- df[,1]; u(df,units[c(1:3,1)])}), info="new column should take on units of new data")
  expect_that({udf <- udf0; udf[,4] <- df0[,1]; udf}, equals({df <- df0; df[,4] <- df[,1]; u(df,c(units,""))}), info="new column should take on units of new data")
  expect_that({df <- df0; df[,5] <- df[,1]; df}, throws_error("new columns would leave holes after existing columns"), info="far-away new column should get error")
  expect_that({udf <- udf0; udf[,5] <- udf[,1]; udf}, throws_error("new columns would leave holes after existing columns"), info="far-away new column should get error")
  knownbug(expect_that({udf <- udf0; udf[,"new col"] <- udf[,1]; udf}, equals({df <- df0; df[,"new col"] <- df[,1]; u(df,units[c(1:3,1)])}), info="new column should take on units of new data"))
  
  # addition of new rows: numeric & character indices
  expect_that({udf <- udf0; udf[4,] <- udf[1,]; udf}, equals({df <- df0; df[4,] <- df[1,]; u(df,units)}), info="add new row with consistent units")
  expect_that({udf <- udf0; udf[8,] <- udf[1,]; udf}, equals({df <- df0; df[8,] <- df[1,]; u(df,units)}), info="add new far-away row with consistent units")
  knownbug(expect_that({udf <- udf0; udf[4,] <- u(df[1,],c("hi","ho","dairy-o")); udf}, throws_error("units mismatch in replacement"), info="add new row with inconsistent units"), "i want different error msg")
  expect_that({udf <- udf0; udf['new row',] <- udf[1,]; udf}, equals({df <- df0; df['new row',] <- df[1,]; u(df,units)}), info="add new row with consistent units")
  knownbug(expect_that({udf <- udf0; udf['up and away!',] <- u(df[1,],c("hi","ho","dairy-o")); udf}, throws_error("units mismatch in replacement"), info="add new row with inconsistent units"), "i want different error msg")
})

test_that("[<-.unitted works for arrays and matrices", {
  units <- "kg ha^-1"
  mat0 <- matrix(1:15,c(3,5))
  arr0 <- array(61:120,c(3,5,4))
  umat0 <- u(mat0, units)
  uarr0 <- u(arr0, units)
  
  # replacement by numeric indices - breaks a lot. simpleError in unitdf$Unit: $ operator is invalid for atomic vectors
  # single values
  knownbug(expect_that({umat <- umat0; umat[2,2] <- u(10,units); umat}, equals({mat <- mat0; mat[2,2] <- 10; u(mat,units)}), info="single element replacement should accept same units"), "simpleError")
  knownbug(expect_that({umat <- umat0; umat[3,3] <- 20; umat}, throws_error("units mismatch in replacement"), info="single element replacement should require same units"), "simpleError")
  expect_that({umat <- u(mat0,""); umat[3,3] <- 20; umat}, equals({mat <- mat0; mat[3,3] <- 20; u(mat,"")}), info="replacing elem having units='' with ununittted value should be OK")
  # columns
  expect_that({umat <- umat0; umat[ ,1] <- u(c('a','b','c'),units); umat}, equals({mat <- mat0; mat[ ,1] <- c('a','b','c'); u(mat,units)}), info="whole column replacement should accept same units")
  knownbug(expect_that({umat <- umat0; umat[ ,2] <- 25; umat}, throws_error("units mismatch in replacement"), info="whole column replacement should require same units"), "should give error & simpleError")
  expect_that({umat <- u(mat0,""); umat[ ,3] <- 25; umat}, equals({mat <- mat0; mat[ ,3] <- 25; u(mat,"")}), info="replacing col having units='' with ununittted value should be OK")
  # rows
  knownbug(expect_that({umat <- umat0; umat[2, ] <- umat[1,]; umat}, equals({mat <- mat0; mat[2, ] <- mat[1,]; u(mat,units)}), info="whole row replacement"), "replaces 1 element instead of 5")
  knownbug(expect_that({umat <- umat0; umat[2, ] <- 20:24; umat}, throws_error("units mismatch in replacement"), info="whole row replacement should require same units"), "error about dims (bad) and need error about units")
  knownbug(expect_that({umat <- u(mat0,""); umat[3,] <- 25; umat}, equals({mat <- mat0; mat[3,] <- 25; u(mat,"")}), info="replacing row having units='' with ununittted value should be OK"))
  # sections
  expect_that({umat <- umat0; umat[2,c(1,3)] <- umat[3,1:2]; umat}, equals({mat <- mat0; mat[2,c(1,3)] <- mat[3,1:2]; u(mat,units)}), info="partial row replacement should accept matching units")
  expect_that({umat <- u(mat0,""); umat[2,c(1,3)] <- 48; umat}, equals({mat <- mat0; mat[2,c(1,3)] <- 48; u(mat,"")}), info="partial row replacement when units='' should accept ununitted values")
  
  # replacement by logical indices
  # ... write tests
  
  # replacement by character indices
  mat1 <- matrix(1:15,3,5,dimnames=list(letters[4:6],LETTERS[1:5]))
  arr1 <- array(61:120,dim=c(3,5,4),dimnames=list(letters[1:3],NULL,LETTERS[5:8]))
  umat1 <- u(mat1, units)
  uarr1 <- u(arr1, units)
  # ... write tests
  
  # addition of new columns, rows, 3rd+ dimensions
  # ... write tests
  
  # write more tests when I implement a better version of unitted arrays and matrices.
  
})

test_that("[<-.unitted works for lists", {
  list0 <- list(a=1:3,b=u(4:10,"fans"))
  expect_that({myls <- list0; myls[2] <- list(1:2); myls}, equals(list(a=1:3,b=1:2)))
  knownbug(expect_that({myls <- list0; myls[2][[1]][1] <- 77; myls}, equals(list(a=1:3,b=u(c(77,5:10),"fans")))))
  # there's really nothing about unitted to test here, since lists can't be unitted and list elements act like their own elements
})


#### [[<-.unitted ####

test_that("[[<-.unitted works for vectors", {
  
  vec0 <- rnorm(8)
  units <- "hash tags"
  uvec0 <- u(vec0, units)

  # numeric and character indices
  expect_that({uvec <- uvec0; uvec[[1]] <- 99; uvec}, equals({vec <- vec0; vec[[1]] <- 99; u(vec,units)}))
  expect_that({uvec <- uvec0; uvec[["4"]] <- 99; uvec}, equals({vec <- vec0; vec[["4"]] <- 99; u(vec,units)}))
  
  # any new units should be ignored (with warnings) with [[]]
  uvec <- uvec0; uvec[[1]] <- u(99,"newunits"); uvec # expect no warnings or errors
  expect_that(uvec, equals({vec <- vec0; vec[[1]] <- 99; u(vec,units)}))
  
  # improper [[<- usage
  expect_that({uvec <- uvec0; uvec[[c(2,3)]] <- 99; uvec}, throws_error("attempt to select more than one element"))
  expect_that({uvec <- uvec0; uvec[[2,3]] <- 99; uvec}, throws_error("improper number of subscripts"))
  expect_that({uvec <- uvec0; uvec[[]] <- 99; uvec}, throws_error("with missing subscript")) # but maybe [[<-.unitted should permit replacement of all values without changing units?
})

test_that("[[<-.unitted works for data.frames", {
  df0 <- data.frame(xam=1:5,yip=LETTERS[6:10],zoom=rnorm(5),stringsAsFactors=FALSE)
  units <- c(xam="toasts",yip="eggs",zoom="hams^2")
  udf0 <- u(df0, units)
  
  # one numeric or character index; logical indices really don't (and shouldn't) work
  knownbug(expect_that({udf <- udf0; udf[[1]] <- 5; udf}, equals({df <- df0; df[[1]] <- 5; u(df,units)})), "units get lost & scrambled")
  knownbug(expect_that({udf <- udf0; udf[[1]] <- u(5,"toasts"); udf}, equals({df <- df0; df[[1]] <- 5; u(df,units)}), info="consistent units should work without warning"), "units get lost & scrambled")
  knownbug(expect_that({udf <- udf0; udf[[1]] <- u(5,"newunits"); udf}, gives_warning("[[<-.unitted retains old units, ignores new ones")))
  knownbug(expect_that(udf, equals({df <- df0; df[[1]] <- 5; u(df,units)})),"unit scrambling")
  knownbug(expect_that({udf <- udf0; udf[["yip"]] <- u(5,"toasts"); udf}, gives_warning("[[<-.unitted retains old units, ignores new ones")), "need warning, and units get lost & scrambled")
  knownbug(expect_that(udf, equals({df <- df0; df[["yip"]] <- 5; u(df,units)})), "units get lost & scrambled")
  knownbug(expect_that({udf <- udf0; udf[["yip"]] <- u(5,"eggs"); udf}, equals({df <- df0; df[["yip"]] <- 5; u(df,units)})), "units get lost & scrambled")
  knownbug(expect_that({udf <- udf0; udf[[c("xam","zoom")]] <- 7; udf}, throws_error("more elements supplied than there are to replace")))
  knownbug(expect_that({udf <- udf0; udf[[c(T,F)]] <- 5; udf}, throws_error("more elements supplied than there are to replace")))
  knownbug(expect_that({udf <- udf0; udf[[T]] <- 5; udf}, equals({udf <- udf0; udf[[1]] <- 5; udf})))
  knownbug(expect_that({udf <- udf0; udf[[F]] <- 5; udf}, throws_error("attempt to select less than one element")))
  expect_that({df <- df0; df[[]] <- 5; df}, throws_error("with missing subscript"))
  knownbug(expect_that({udf <- udf0; udf[[]] <- 5; udf}, throws_error("with missing subscript")))
  
  # two numeric or character indices (logical breaks correctly)
  knownbug({
    expect_that({udf <- udf0; udf[[2,1]] <- u(55,"toasts"); udf}, equals({df <- df0; df[[2,1]] <- 55; u(df,units)}), info="consistent units should work without warning")
    expect_that({udf <- udf0; udf[[1,2]] <- u(55,"toasts"); udf}, gives_warning("[[<-.unitted retains old units, ignores new ones"), info="inconsistent units should give warning")# breaks - need warning
    expect_that(udf, equals({df <- df0; df[[1,2]] <- 55; u(df,units)}), info="inconsistent units should give warning but also right answer")
    expect_that({udf <- udf0; udf[[1,2]] <- 55; udf}, equals({df <- df0; df[[1,2]] <- 55; u(df,units)}))
    expect_that({udf <- udf0; udf[[ ,2]] <- 55; udf}, throws_error("only valid calls are "), info="cols may NOT be changed with df[[x,]] <- value")
    expect_that({udf <- udf0; udf[[2, ]] <- 55; udf}, throws_error("only valid calls are "), info="rows may NOT be changed with df[[x,]] <- value")
    expect_that({udf <- udf0; udf[[,"yip"]] <- 55; udf}, throws_error("only valid calls are "), info="cols may NOT be changed with df[[x,]] <- value")
    expect_that({udf <- udf0; udf[['2',]] <- 55; udf},   throws_error("only valid calls are "), info="rows may NOT be changed with df[[x,]] <- value")
    expect_that({udf <- udf0; udf[[T,T]] <- u(5,"toasts"); udf}, throws_error("only a single element should be replaced"))
    expect_that({udf <- udf0; udf[[T,F]] <- u(5,"toasts"); udf}, throws_error("only a single element should be replaced"))  
    expect_that({udf <- udf0; udf[[F,T]] <- u(5,"toasts"); udf}, throws_error("only a single element should be replaced"))  
    expect_that({udf <- udf0; udf[[F,F]] <- u(5,"toasts"); udf}, throws_error("attempt to select less than one element"))  
  })
  
  # deletion by udf[xx] <- NULL
  knownbug({
    expect_that({udf <- udf0; udf[[2]] <- NULL; udf}, equals({df <- df0; df[[2]] <- NULL; u(df,units[c(1,3)])}), info="a column may be deleted with [[x]] <- NULL")
    expect_that({udf <- udf0; udf[[c(1,2)]] <- NULL; udf}, throws_error("more elements supplied than there are to replace"), info="only one column at a time")
    expect_that({udf <- udf0; udf[[2,2]] <- NULL; udf}, throws_error("more elements supplied than there are to replace"), info="NULL cannot replace an element")
    expect_that({udf <- udf0; udf[[,2]] <- NULL; udf}, throws_error("only valid calls are "), info="cols may NOT be deleted with df[[x,]] <- NULL")
    expect_that({udf <- udf0; udf[[2,]] <- NULL; udf}, throws_error("only valid calls are "), info="rows may NOT be deleted with df[[x,]] <- NULL")
  })
  expect_that({df <- df0; df[[2,2]] <- NULL; df}, throws_error("more elements supplied than there are to replace"), info="NULL cannot replace an element")
  
  # adding columns
  knownbug({
    expect_that({udf <- udf0; udf[["whoo"]] <- 5; udf}, equals({df <- df0; df[["whoo"]] <- 5; u(df,c(units[1],"jellies",units[3]))})) # breaks - should create column
    expect_that({udf <- udf0; udf[["whoo"]] <- u(5,""); udf}, gives_warning("ignoring new units with [[<-.unitted column creation; use [<- instead to pass units"))
    expect_that({udf <- udf0; udf[["whoo"]] <- u(5,"jellies"); udf}, gives_warning("ignoring new units with [[<-.unitted column creation; use [<- instead to pass units"))
    expect_that(udf, equals({df <- df0; df[["whoo"]] <- 5; u(df,c(units,""))}))
  })
  
  # partial matching should be unavailable for assignment
  expect_that({df <- df0; df[["y",exact=FALSE]] <- 5; df}, throws_error("unused argument")) # for comparison to unitted version
  expect_that({udf <- udf0; udf[["y",exact=FALSE]] <- u(5,"jellies"); udf}, throws_error("unused argument")) # yes. exact argument should be rejected
})

test_that("[[<-.unitted works for arrays and matrices", {
  units <- "kg ha^-1"
  mat0 <- matrix(1:15,c(3,5))
  arr0 <- array(61:120,c(3,5,4))
  umat0 <- u(mat0, units)
  uarr0 <- u(arr0, units)
  
  # replacement should occur smoothly when units of the replacement are nonexistent or match perfectly
  # replacement should occur with warning when units are specified and don't match; new units are ignored
  
  # matrices
  # single values, notation for one long vector
  expect_that({umat <- umat0; umat[[2]] <- 10; umat}, equals({mat <- mat0; mat[[2]] <- 10; u(mat,units)}))
  expect_that({umat <- umat0; umat[[2]] <- u(10,units); umat}, equals({mat <- mat0; mat[[2]] <- 10; u(mat,units)}))
  umat <- umat0; umat[[2]] <- u(10,"newunits"); umat # expect no warnings/errors
  expect_that(umat, equals({mat <- mat0; mat[[2]] <- 10; u(mat,units)}))
  # single values, matrix row-column notation
  expect_that({umat <- umat0; umat[[2,3]] <- 33; umat}, equals({mat <- mat0; mat[[2,3]] <- 33; u(mat,units)}))
  expect_that({umat <- umat0; umat[[2,3]] <- u(33,units); umat}, equals({mat <- mat0; mat[[2,3]] <- 33; u(mat,units)}))
  umat <- umat0; umat[[2,3]] <- u(33,"newunits"); umat # expect no warnings/errors
  expect_that(umat, equals({mat <- mat0; mat[[2,3]] <- 33; u(mat,units)}))
  
  # arrays
  # single values, notation for one long vector
  expect_that({uarr <- uarr0; uarr[[2]] <- 10; uarr}, equals({arr <- arr0; arr[[2]] <- 10; u(arr,units)}))
  expect_that({uarr <- uarr0; uarr[[2]] <- u(10,units); uarr}, equals({arr <- arr0; arr[[2]] <- 10; u(arr,units)}))
  uarr <- uarr0; uarr[[2]] <- u(10,"newunits"); uarr # expect no warnings/errors
  expect_that(uarr, equals({arr <- arr0; arr[[2]] <- 10; u(arr,units)}))
  # single values, row-column-d3 notation
  expect_that({uarr <- uarr0; uarr[[2,3,4]] <- 33; uarr}, equals({arr <- arr0; arr[[2,3,4]] <- 33; u(arr,units)}))
  expect_that({uarr <- uarr0; uarr[[2,3,4]] <- u(33,units); uarr}, equals({arr <- arr0; arr[[2,3,4]] <- 33; u(arr,units)}))
  uarr <- uarr0; uarr[[2,3,4]] <- u(33,"newunits"); uarr # expect no warnings/errors
  expect_that(uarr, equals({arr <- arr0; arr[[2,3,4]] <- 33; u(arr,units)}))
  
  # character indices (allowed for 1 string only)
  knownbug({
    expect_that({umat <- umat0; umat[['2']] <- 10; umat}, equals({mat <- mat0; mat[['2']] <- 10; u(mat,units)})) # creates 1-D array with new element c('2'=10)
    expect_that({umat <- umat0; umat[['2']] <- u(10,units); umat}, equals({mat <- mat0; mat[['2']] <- 10; u(mat,units)})) # creates 1-D array with new element c('2'=10)
    umat <- umat0; umat[['2']] <- u(10,"newunits"); umat # expect no warnings/errors
    expect_that(umat, equals({mat <- mat0; mat[['2']] <- 10; u(mat,units)}))
    expect_that({uarr <- uarr0; uarr[['2']] <- 10; uarr}, equals({arr <- arr0; arr[['2']] <- 10; u(arr,units)})) # creates 1-D array with new element c('2'=10)
    expect_that({uarr <- uarr0; uarr[['2']] <- u(10,units); uarr}, equals({arr <- arr0; arr[['2']] <- 10; u(arr,units)})) # creates 1-D array with new element c('2'=10)
    expect_that({uarr <- uarr0; uarr[['2']] <- u(10,"newunits"); uarr}, gives_warning("[[<-.unitted retains old units, ignores new ones")) # creates 1-D array with new element c('2'=10)
    expect_that(uarr, equals({arr <- arr0; arr[['2']] <- 10; u(arr,units)}))
  }, 'currently requiring 2D matrices')
  expect_that({umat <- umat0; umat[['2','3']] <- 10; umat}, throws_error("subscript out of bounds"))
  expect_that({uarr <- uarr0; uarr[['2','3','1']] <- 10; uarr}, throws_error("subscript out of bounds"))
  
  # logical (disallowed) indices
  expect_that({umat <- umat0; umat[[T]] <- 10; umat}, equals({mat <- mat0; mat[[1]] <- 10; u(mat,units)})) # only works because T=1
  expect_that({umat <- umat0; umat[[F]] <- 10; umat}, throws_error("attempt to select less than one element"))
  expect_that({umat <- umat0; umat[[rep(T,16)]] <- 10; umat}, throws_error("attempt to select more than one element"))
  
  # columns and rows - empty indices are not allowed
  expect_that({umat <- umat0; umat[[,1]] <- 6:4; umat}, throws_error("more elements supplied than there are to replace"))
  expect_that({umat <- umat0; umat[[,1]] <- u(c('a','b','c'),units); umat}, throws_error("more elements supplied than there are to replace"))
  expect_that({umat <- umat0; umat[[3,]] <- 6:2; umat}, throws_error("more elements supplied than there are to replace"))
  expect_that({umat <- umat0; umat[[3,]] <- u(c('a','b','c','d','e'),units); umat}, throws_error("more elements supplied than there are to replace"))
  
})

test_that("[[<-.unitted works for lists", {
  list0 <- list(a=1:3,b=u(4:10,"fans"))
  expect_that({myls <- list0; myls[[2]] <- 1:2; myls}, equals(list(a=1:3,b=1:2)))
  expect_that({myls <- list0; myls[[2]][1] <- u(77,"fans"); myls}, equals(list(a=1:3,b=u(c(77,5:10),"fans"))))
  expect_that({myls <- list0; myls[[2]][1] <- 77; myls}, throws_error("Mismatched units in subset replacement"))
  # there's really nothing about unitted to test here, since lists can't be unitted and list elements act like their own elements
})


#### $<-.unitted ####

# FROM DOCUMENTATION at ?Extract: "The default methods work somewhat differently
# for atomic vectors, matrices/arrays and for recursive (list-like, see 
# is.recursive) objects. $ is only valid for recursive objects, and is only 
# discussed in the section below on recursive objects. ... Both [[ and $ select 
# a single element of the list. The main difference is that $ does not allow 
# computed indices, whereas [[ does. x$name is equivalent to x[["name", exact = 
# FALSE]]."

test_that("$<-.unitted works for data.frames", {
  # ?"$<-.data.frame" says, "There is a replacement method [for $] which checks
  # value for the correct number of rows, and replicates it if necessary."
  
  df0 <- data.frame(xig=1:5,yog=LETTERS[6:10],zug=rnorm(5),stringsAsFactors=FALSE)
  units <- c(xig="toasts",yog="eggs",zug="hams^2")
  udf0 <- u(df0, units)

  # existing columns
  expect_that({udf <- udf0; udf$zug <- u(11:15,"juices"); udf}, equals({df <- df0; df$zug <- 11:15; u(df,c(units[1:2],"juices"))}))
  knownbug(expect_that({udf <- udf0; udf$zug <- 11:15; udf}, equals({df <- df0; df$zug <- 11:15; u(df,c(units[1:2],""))})))

  # new columns
  expect_that({udf <- udf0; udf$kablammo <- u(11:15,"juices"); udf}, equals({df <- df0; df$kablammo <- 11:15; u(df,c(units,"juices"))}))
  udf <- udf0; udf$kablammo <- 11:15; udf # expect no warnings
  expect_that(udf, equals({df <- df0; df$kablammo <- 11:15; u(df,c(units,""))}))  
})

test_that("$<-.unitted works for lists", {
  list0 <- list(a=1:3,b=u(4:10,"fans"))
  expect_that({myls <- list0; myls$b <- 1:2; myls}, equals(list(a=1:3,b=1:2)))
  expect_that({myls <- list0; myls$b[[1]][1] <- u(77,"fans"); myls}, equals(list(a=1:3,b=u(c(77,5:10),"fans"))))
  knownbug(expect_that({myls <- list0; myls$b[[1]][1] <- 77; myls}, throws_error("units mismatch in replacement")), "this is really a [<-.unitted-vector issue")
  # there's really nothing about unitted to test here, since lists can't be unitted and list elements act like their own elements
})

