context("combine")
knownbug <- function(expr, notes) invisible(NULL)

#### unitted:::c.unitted ####

test_that("unitted:::c.unitted works", {
  
  # empty should return NULL
  expect_equal(unitted:::c.unitted(), c())
  
  # vectors - just one vector to concatenate
  expect_equal(unitted:::c.unitted(1), u(1,NA))
  expect_equal(unitted:::c.unitted(u(1,"helmets")), u(c(1),"helmets"))
  expect_equal(c(u(1,"helmets")), u(c(1),"helmets"))
  expect_equal(unitted:::c.unitted(u(1:40,"hornet")), u(c(1:40),"hornet"))
  expect_equal(c(u(1:40,"hornet")), u(c(1:40),"hornet"))
  expect_equal(unitted:::c.unitted(u(letters[26:1],"horns")), u(c(letters[26:1]),"horns"))
  expect_equal(c(u(letters[26:1],"horns")), u(c(letters[26:1]),"horns"))
  
  # vectors - concatenate more than one
  expect_equal(c(u(letters[26:21],"corns"),u(LETTERS[4:12],"corns")), u(c(letters[26:21],LETTERS[4:12]),"corns"))
  expect_error(c(u(letters[26:21],"corns"),u(LETTERS[4:12],"cornucopias")), "every element must have the same units")
  expect_equal(c(u(26:21,"mazes"),u(4:12,"mazes")), u(c(26:21,4:12),"mazes"))
  expect_error(c(u(26:21,"mazes"),u(4:12,"maizes")), "every element must have the same units")
  expect_equal(c(u(26:21,"mazes"),u(4:12,"mazes"),u(LETTERS[4:12],"mazes")), u(c(26:21,4:12,LETTERS[4:12]),"mazes"))
  
  # data.frames
  df1 <- data.frame(A=1:5, B=12:16, stringsAsFactors=FALSE)
  df2 <- data.frame(X=letters[4:12], Y=LETTERS[6:14], stringsAsFactors=FALSE)
  dfu1 <- data.frame(A=u(1:5,"cats"), B=u(12:16,"cats"), stringsAsFactors=FALSE)
  dfu2 <- data.frame(X=letters[4:12], Y=u(LETTERS[6:14],"dogs"), stringsAsFactors=FALSE)
  dfu3 <- data.frame(X=u(letters[4:12],"cats"), Y=u(LETTERS[6:14],"cats"), stringsAsFactors=FALSE)
  dfu4 <- data.frame(X=u(letters[4:12],"cats"), Y=u(LETTERS[6:14],"dogs"), stringsAsFactors=FALSE)
  udf1 <- u(dfu1)
  udf2 <- u(dfu2)
  udf3 <- u(dfu3)
  udf4 <- u(dfu4)
  # joining two unitted (or not) data.frames
  expect_equal(get_units(c(udf1, C=u(2:3,"bats"))), c(A="cats",B="cats",C1="bats",C2="bats"))
  knownbug(expect_equal(c(udf1,udf3), lapply(c(df1,df2),u,"cats")))
  knownbug(expect_equal(c(udf1,udf3), c(dfu1,dfu3)))
  expect_equal(unitted:::c.unitted(dfu1,dfu3), c(dfu1,dfu3))
  expect_equal(unitted:::c.unitted(3,udf1,dfu3), c(3,dfu1,dfu3)) # fine - units can all be different for data.frame joins
  
  # matrices and arrays
  mat1 <- matrix(1:10,2)
  mat2 <- matrix(31:40,5)
  umat11 <- u(mat1,"mat1")
  umat21 <- u(mat2,"mat1")
  umat22 <- u(mat2,"mat2")
  expect_equal(c(umat11,umat21), u(c(mat1,mat2),"mat1"))
  expect_error(c(umat11,umat22), "every element must have the same units")
  arr1 <- array(1:60,c(4,5,3))
  arr2 <- array(letters[1:60],c(2,6,5))
  uarr11 <- u(arr1,"arr1")
  uarr21 <- u(arr2,"arr1")
  uarr22 <- u(arr2,"arr2")
  expect_equal(c(uarr11,uarr21), u(c(arr1,arr2),"arr1"))
  expect_error(c(uarr11,uarr22), "every element must have the same units")
  
  # regular lists in the arg list mean we join anything and let each element keep its units
  ls1 <- list(A = 1, c = "C")
  vec <- 8:9
  ls2 <- list(k=1:3)
  ls3 <- list(e=u(1,"E"),r=u(2,"R"),g="G")
  expect_equal(unitted:::c.unitted(ls1, d=ls2), c(ls1, d=ls2)) # acts like usual c(list)
  expect_equal(unitted:::c.unitted(vec, d=ls2), c(vec, d=ls2)) # acts like usual c(list)
  expect_equal(unitted:::c.unitted(ls3, d=ls2), c(ls3, d=ls2)) # acts like usual c(list)
  
  # unitted lists in the arg list mean we join at the list level and require that the final list has a single sensible unit
  uls1 <- u(ls1, "jump")
  uls3 <- u(ls3, "around")
  expect_error(c(uls1, uls3), "every element must have the same high-level units")
  expect_equal(c(uls1, u(ls3, "jump")), u(c(ls1, ls3),"jump"))
  expect_equal(get_units(c(uls1, u(ls3, "jump")), recursive=TRUE), c(A=NA,c=NA,e="E",r="R",g=NA))
  expect_s4_class(c(uls1, u(vec,"jump")), "unitted_list")
  expect_equal(get_units(c(uls1, u(vec,"jump")), recursive=FALSE), "jump")
  expect_equal(get_units(c(uls1, u(vec,"jump")), recursive=TRUE), c(A=NA,c=NA,NA,NA))
  
})

#### rbind.unitted ####

test_that("rbind works", {
  # vectors
  vec <- 1:5
  uvec <- u(vec,"snaps")
  expect_equal(rbind(uvec,uvec), u(rbind(uvec=vec,uvec=vec),"snaps"))
  expect_error(rbind(u(vec,"snaps"),u(vec,"crackles")), "Every element must have the same units")
  expect_error(rbind(vec,uvec), "Every element must have the same units")
  expect_equal(rbind(vec,vec=u(vec,"")), u(rbind(vec,vec),""))
  
  # vectors with mismatched types - use rbind.unitted
  # KNOWN ISSUE - must use rbind.unitted when types are mismatched
  #   expect_equal(rbind(u(1:3,"c"),u(c("a","b","c"),"c")), u(rbind(1:3,c("a","b","c")), "c")) # rbind.unitted doesn't get called
  # SOLUTION: call rbind.unitted directly
  expect_equal(rbind.unitted(u(1:3,"c"),u(c("a","b","c"),"c")), u(rbind(1:3,c("a","b","c")), "c"))
  
  # deparse.level argument
  expect_equal(rbind(uvec,uvec, deparse.level=0), u(rbind(vec,vec, deparse.level=0),"snaps"))
  expect_equal(rbind(uvec,uvec, deparse.level=1), rbind(uvec,uvec))
  expect_warning(rbind(uvec,uvec, deparse.level=2), "deparse.level=2 has no unitted implementation; defaulting to deparse.level=1")
  
  # nested within functions
  test_rbind <- function(...) {
    rbind(..., u(5:9,"snaps"))
  }
  expect_warning(test_rbind(uvec,uvec), "arguments could not be deparsed; forcing deparse.level to 0")
  expect_warning(test_rbind(uvec,uvec,deparse.level=0), "arguments could not be deparsed; forcing deparse.level to 0")
  expect_equal(suppressWarnings(test_rbind(uvec,uvec)), u(rbind(vec,vec,5:9,deparse.level=0),"snaps"))
  test_rbind2 <- function(...) {
    rbind(..., u(5:9,"snaps"), deparse.level=0)
  }
  expect_equal(test_rbind2(uvec,uvec), u(rbind(vec,vec,5:9,deparse.level=0),"snaps"))
  test_rbind3 <- function(...) {
    arglist <- list(...)
    do.call(rbind, arglist)
  }
  expect_equal(test_rbind3(uvec, uvec, u(5:1,"snaps"), deparse.level=0), u(test_rbind3(vec,vec,5:1,deparse.level=0),"snaps"))
  expect_equal(test_rbind3(uvec, uvec, u(5:1,"snaps"), deparse.level=1), u(test_rbind3(vec,vec,5:1,deparse.level=1),"snaps"))
  
  # data.frames
  df <- data.frame(co=1:4,balt=4:7)
  udf <- u(df, c("u1","u2^4"))
  expect_equal(rbind(udf,udf), u(rbind(df,df), c("u1","u2^4")))
  expect_error(rbind(udf,u(data.frame(co=5,balt=8),c("one","u2^4"))), "Every element must have the same units")
  expect_equal(rbind(udf,u(data.frame(co=5,balt=8),c("u1","u2^4"))), u(rbind(df,data.frame(co=5,balt=8)),c("u1","u2^4")))
  # KNOWN ISSUE: rbind(udf,df) calls the data.frame method and returns junk
  #   expect_error(rbind(udf,data.frame(co=5:6,balt=8)), "Every element must have the same units") # KNOWN ISSUE - rbind data.frame (not unitted) method gets called
  #   expect_equal(rbind(udf,data.frame(co=u(5:6,"u1"),balt=u(8:9,"u2^4"))), rbind(udf,u(data.frame(co=u(5:6,"u1"),balt=u(8:9,"u2^4"))))) # KNOWN ISSUE - rbind data.frame (not unitted) method gets called
  #   expect_error(rbind(udf,data.frame(co=u(5:6,"u1"),balt=u(8:9,"u7"))), "Every element must have the same units")# KNOWN ISSUE - rbind data.frame (not unitted) method gets called
  # SOLUTION: data.frames: rbind.unitted(udf,df) calls unitted_data.frame method
  expect_error(rbind.unitted(udf,data.frame(co=5:6,balt=8)), "Every element must have the same units")
  expect_equal(rbind.unitted(u(udf,c("","")),data.frame(co=5:6,balt=8)), u(rbind(df,data.frame(co=5:6,balt=c(8,8))),c("","")))
  expect_equal(rbind.unitted(udf,data.frame(co=u(5:6,"u1"),balt=u(8:9,"u2^4"))), u(rbind(df,data.frame(co=5:6,balt=8:9)),c("u1","u2^4")))
  expect_error(rbind.unitted(udf,data.frame(co=u(5:6,"u1"),balt=u(8:9,"u7"))), "Every element must have the same units")
  # KNOWN ISSUE: rbind(udf, uvec) requires cbind.unitted
  #   expect_equal(rbind(u(df,c("kits","kits")),co=u(5:6,'kits')), u(rbind(df,co=5:6),c("kits","kits"))) # breaks; malformed result
  # KNOWN ISSUE: 
  #   expect_equal(rbind.unitted(u(df,c("kits","kits")),co=u(5:6,'kits')), u(rbind(df,co=5:6),c("kits","kits"))) # breaks; 
  # SOLUTION: call rbind.unitted directly and with as.list for vector
  knownbug(expect_equal(rbind.unitted(u(df,c("kits","kits")),co=as.list(u(5:6,'kits'))), u(rbind(df,co=5:6),c("kits","kits"))))
  
  # data.frames with vectors
  # A funky case: how to rbind a vector to a data.frame of columns with identical units? use as.list.
  knownbug(expect_equal(rbind(u(df,c("kits","kits")),co=as.list(u(5:6,'kits'))), u(rbind(df,co=5:6),c("kits","kits"))))
  
  # matrices and arrays
  mat1 <- matrix(1:10,2)
  mat2 <- matrix(31:40,2)
  umat11 <- u(mat1,"mat1")
  umat21 <- u(mat2,"mat1")
  umat22 <- u(mat2,"mat2")
  expect_equal(rbind(umat11,umat21), u(rbind(mat1,mat2),"mat1"))
  expect_error(rbind(umat11,umat22), "Every element must have the same units")
  arr1 <- array(1:60,c(4,5,3))
  arr2 <- array(letters[1:60],c(2,6,5))
  uarr11 <- u(arr1,"arr1")
  uarr21 <- u(arr2,"arr1")
  uarr22 <- u(arr2,"arr2")
  expect_equal(rbind(uarr11,uarr21), u(rbind(uarr11=arr1,uarr21=arr2),"arr1"))
  expect_error(rbind(uarr11,uarr22), "Every element must have the same units")
})

#### cbind.unitted ###

test_that("cbind works", {
  # vectors
  vec <- 1:5
  uvec <- u(vec,"snaps")
  expect_equal(cbind(uvec,uvec), u(cbind(uvec=vec,uvec=vec),"snaps"))
  expect_error(cbind(u(vec,"snaps"),u(vec,"crackles")), "Every element must have the same units")
  expect_error(cbind(vec,uvec), "Every element must have the same units")
  expect_error(cbind(uvec,vec), "Every element must have the same units")
  expect_equal(cbind(vec,vec=u(vec,"")), u(cbind(vec,vec),""))
  
  # deparse.level argument
  expect_equal(cbind(uvec,uvec, deparse.level=0), u(cbind(vec,vec, deparse.level=0),"snaps"))
  expect_equal(cbind(uvec,uvec, deparse.level=1), cbind(uvec,uvec))
  expect_warning(cbind(uvec,uvec, deparse.level=2), "deparse.level=2 has no unitted implementation; defaulting to deparse.level=1")
  
  # nested within functions
  test_cbind <- function(...) {
    cbind(..., u(5:9,"snaps"))
  }
  expect_warning(test_cbind(uvec,uvec), "arguments could not be deparsed; forcing deparse.level to 0")
  expect_warning(test_cbind(uvec,uvec,deparse.level=0), "arguments could not be deparsed; forcing deparse.level to 0")
  expect_equal(suppressWarnings(test_cbind(uvec,uvec)), u(cbind(vec,vec,5:9,deparse.level=0),"snaps"))
  test_cbind2 <- function(...) {
    cbind(..., u(5:9,"snaps"), deparse.level=0)
  }
  expect_equal(test_cbind2(uvec,uvec), u(cbind(vec,vec,5:9,deparse.level=0),"snaps"))
  test_cbind3 <- function(...) {
    arglist <- list(...)
    do.call(cbind, arglist)
  }
  expect_equal(test_cbind3(uvec, uvec, u(5:1,"snaps"), deparse.level=0), u(test_cbind3(vec,vec,5:1,deparse.level=0),"snaps"))
  expect_equal(test_cbind3(uvec, uvec, u(5:1,"snaps"), deparse.level=1), u(test_cbind3(vec,vec,5:1,deparse.level=1),"snaps"))
  
  # data.frames - see KNOWN ISSUES for cbind(udf, df)
  df <- data.frame(co=1:4,balt=4:7)
  udf <- u(df, c("u1","u2^4"))
  expect_equal(cbind(udf,udf), u(cbind(df,df), c("u1","u2^4","u1","u2^4")))
  expect_equal(cbind(udf,u(data.frame(co=2:5,balt=5:8),c("one","u2^4"))), u(cbind(df,data.frame(co=2:5,balt=5:8)),c("u1","u2^4","one","u2^4")))
  expect_equal(cbind(udf,u(data.frame(co=5,balt=8:9),c("one","two"))), u(cbind(df,data.frame(co=5,balt=8:9)),c("u1","u2^4","one","two")))
  # KNOWN ISSUE: cbind(udf,df) calls the data.frame method and returns junk
  #   expect_error(cbind(udf,data.frame(co=5:6,balt=8)), "Every element must have the same units") # breaks - cbind data.frame (not unitted) method gets called
  #   expect_equal(cbind(udf,data.frame(co=u(5:6,"u1"),balt=u(8:9,"u2^4"))), u(cbind(df,data.frame(co=5:6,balt=8:9)), c("u1","u2^4","u1","u2^4"))) # breaks - cbind data.frame (not unitted) method gets called
  #   expect_error(cbind(data.frame(co=5:6,balt=8), udf), "Every element must have the same units") # breaks - cbind data.frame (not unitted) method gets called
  #   expect_equal(cbind(data.frame(co=u(5:6,"u1"),balt=u(8:9,"u2^4")), udf), cbind(udf,u(data.frame(co=u(5:6,"u1"),balt=u(8:9,"u2^4"))))) # breaks - cbind data.frame (not unitted) method gets called
  # SOLUTION: although cbind(udf,df) breaks, cbind.unitted(udf,df) calls unitted_data.frame method even when object types in ... are different
  expect_equal(cbind.unitted(udf,data.frame(co=5:6,balt=8)), u(cbind(df, data.frame(co=5:6,balt=8)),c("u1","u2^4","","")))
  expect_equal(cbind.unitted(u(udf,c("","")),data.frame(co=5:6,balt=8)), u(cbind(df,data.frame(co=5:6,balt=c(8))),c("","","","")))
  # KNOWN ISSUE: cbind(udf, uvec) requires cbind.unitted
  #   expect_equal(cbind(udf,co=u(5:6,'kits')), u(cbind(df,co=5:6),c("u1","u2^4","kits"))) # breaks; malformed result
  # SOLUTION: call cbind.unitted directly
  expect_equal(cbind.unitted(udf,co=u(5:6,'kits')), u(cbind(df,co=5:6),c("u1","u2^4","kits")))
  
  # data.frames with vectors - see KNOWN ISSUES for cbind() version
  # expect_equal(cbind(udf,co=u(5:6,'kits')), u(cbind(df,co=5:6),c("u1","u2^4","kits"))) # KNOWN ISSUE
  expect_equal(cbind.unitted(udf,co=u(5:6,'kits')), u(cbind(df,co=5:6),c("u1","u2^4","kits")))
  
  # matrices and arrays
  mat1 <- matrix(1:10,2)
  mat2 <- matrix(31:40,2)
  umat11 <- u(mat1,"mat1")
  umat21 <- u(mat2,"mat1")
  umat22 <- u(mat2,"mat2")
  expect_equal(cbind(umat11,umat21), u(cbind(mat1,mat2),"mat1"))
  expect_error(cbind(umat11,umat22), "Every element must have the same units")
  arr1 <- array(1:60,c(4,5,3))
  arr2 <- array(letters[1:60],c(2,6,5))
  uarr11 <- u(arr1,"arr1")
  uarr21 <- u(arr2,"arr1")
  uarr22 <- u(arr2,"arr2")
  expect_equal(cbind(uarr11,uarr21), u(cbind(uarr11=arr1,uarr21=arr2),"arr1"))
  expect_error(cbind(uarr11,uarr22), "Every element must have the same units")
})


#### data.frame.unitted ####

test_that("data.frame(unitted vec, mat) works", {
  vec <- 1:5
  mat <- matrix(1:20, 5, 4)
  uvec <- u(vec,"donuts")
  umat <- u(mat,"coffees")
  knownbug(expect_equal(u(data.frame(uvec, umat)), u(data.frame(uvec=vec, mat), c("donuts",rep("coffees",4)))))
})


#### merge.unitted_data.frame ####

test_that("merge.unitted_data.frame works", {
  df1 <- data.frame(x=1:3, y=4:6, z=7:9)
  df2 <- data.frame(a=11:13, b=c(7,7,8), x=c(3,1,2))
  udf1 <- u(df1, c("X","Y","Z"))
  udf2 <- u(df2, c("A","Z","X"))
  
  expect_equal(merge(udf1, u(df2, c("A","B","X"))), u(merge(df1, df2), c("X","Y","Z","A","B")))
  expect_error(suppressMessages(merge(udf1, u(df2, c("A","B","C")))), "Units conflict in merge") 
  
  expect_equal(merge(udf1, udf2, by.x="z", by.y="b"), u(merge(df1, df2, by.x="z", by.y="b"), c("Z","X","Y","A","X")))
  expect_error(suppressMessages(merge(udf1, u(df2, c("A","Q","X")), by.x="z", by.y="b")), "Units conflict in merge") 
})


#### rep ####

test_that("rep.unitted works", {
  # vectors
  expect_equal(rep(u(1:3,"d"), 7), u(rep(1:3,7),"d"))
  expect_equal(rep(u(1:3,"d"), times=7), u(rep(1:3,7),"d"))
  expect_equal(rep(u(1:3,"d"), length.out=7), u(rep(1:3,length.out=7),"d"))
  expect_equal(rep(u(1:3,"d"), each=7), u(rep(1:3,each=7),"d"))
  
  # data.frames
  df <- data.frame(i=c("k","j","l"), y=4:6, z="a", stringsAsFactors=FALSE)
  udf <- u(df, c("A","B","C"))
  expect_equal(rep(udf, 2), mapply(function(elem, units) unitted(elem, units), rep(df,2), c("A","B","C","A","B","C"), SIMPLIFY=FALSE))
  expect_equal(v(rep(udf, 2)), rep(df, 2))
  
  # lists
  li <- list(r=4:5, t=rep("a", 4), y=Sys.Date())
  uli <- u(li, "oneunit")
  expect_equal(rep(uli, 3), u(rep(li, 3), "oneunit"))
  li2 <- list(r=u(4:5,"10"), t=u(rep("a", 4),"20"), y=u(Sys.Date(),"30"))
  expect_equal(get_units(rep(li2, 3)), rep(c(r="10",t="20",y="30"), 3))
})
