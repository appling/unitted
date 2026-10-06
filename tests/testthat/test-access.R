context("access")
knownbug <- function(expr, notes) invisible(NULL)

#### [.unitted ####

test_that("vectors can be accessed with '[.unitted' by element numbers", {
  vvec <- 1:10
  names(vvec) <- LETTERS[5:14]
  uvec <- u(vvec,"hats")
  
  # numeric indices
  expect_equal(uvec[2], u(vvec[2],"hats"), info="indexing by an existing element number")
  expect_equal(uvec[c(3,9,1)], u(vvec[c(3,9,1)],"hats"), info="indexing by several existing element numbers")
  expect_equal(uvec[c(8,NA)], u(vvec[c(8,NA)],"hats"), info="indexing with NAs")
  expect_equal(uvec[24], u(vvec[24],"hats"), info="indexing by a too-high element number")
  expect_equal(uvec[-2], u(vvec[-2],"hats"), info="indexing by a realistic negative number")
  expect_equal(uvec[-89], u(vvec[-89],"hats"), info="indexing by a too-low negative number")
  
  # logical indices  
  expect_equal(uvec[rep(T,10)], u(vvec[rep(T,10)],"hats"), info="indexing by all TRUEs")
  expect_equal(uvec[rep(T,20)], u(vvec[rep(T,20)],"hats"), info="indexing by a long list of TRUEs")
  expect_equal(uvec[rep(F,50)], u(vvec[rep(F,50)],"hats"), info="indexing by a long list of FALSEs")
  expect_equal(uvec[c(T,F,F)], u(vvec[c(T,F,F)],"hats"), info="indexing by a short list of logicals")
  expect_equal(uvec[c(T,NA,F)], u(vvec[c(T,NA,F)],"hats"), info="indexing with logicals and NAs")
  expect_equal(uvec[rep(c(T,F),8)], u(vvec[rep(c(T,F),8)],"hats"), info="indexing by many alternating logicals")
  
  # character indices
  expect_equal(uvec["F"], u(vvec["F"],"hats"), info="indexing by a single string")
  expect_equal(uvec[c("N","H","N")], u(vvec[c("N","H","N")],"hats"), info="indexing by a vector of strings")
  expect_equal(uvec["x"], u(vvec["x"],"hats"), info="indexing by a non-name string")
  vvec2 <- 1:10 # no names
  uvec2 <- u(vvec2,"hats") # no names
  expect_equal(uvec2["F"], u(vvec2["F"],"hats"), info="indexing an unnamed vector by a single string")
  expect_equal(uvec2[c("N","H","N")], u(vvec2[c("N","H","N")],"hats"), info="indexing an unnamed vector by a vector of strings")

  # multiple indices
  expect_equal(uvec[], uvec, info="empty args to [] change nothing")
  expect_error(uvec[,], "incorrect number of dimensions")
  expect_error(uvec[1,1], "incorrect number of dimensions")
  expect_error(uvec[,,2], "incorrect number of dimensions")
  expect_error(uvec["quantum","physics"], "incorrect number of dimensions")
})

test_that("vectors of all types can be accessed with '[.unitted'", {
  test_index_both_ways <- function(vvec, index, note) {
    uvec <- u(vvec,"hats rats^-1")
    info <- paste0("when indexing ",note," c(",paste0(vvec[1:4],collapse=","),",...) by c(",paste(index,collapse=","),")")
    expect_equal(uvec[index], u(vvec[index],"hats rats^-1"), info=info)
    expect_equal(v(uvec[index]), vvec[index], info=info)
  }
  
  test_index_both_ways(rep(c(T,F),4), 1:3, "logical")
  test_index_both_ways(rnorm(5), c(T,NA,F), "numeric")
  test_index_both_ways(1L:10L, c(T,NA,F), "integer")
  test_index_both_ways(rnorm(5), c(T,NA,F), "double")
  test_index_both_ways(as.single(rnorm(5)), c(T,NA,F), "single")
  test_index_both_ways(sample(LETTERS,26), c(T,NA,F), "character")
  test_index_both_ways(complex(real=rnorm(7),imaginary=-7:-2), c(5,2,1), "complex")
  test_index_both_ways(as.raw(40:45), c(T,NA,F), "raw")
  knownbug(test_index_both_ways(rep(parse(text="5*x+2*y==z"),4), 1:9, "expression"), 'target is not list-like when indexing expression')
  knownbug(test_index_both_ways(factor(letters[3:7]), c(T,NA,F), "factor"))
  knownbug(test_index_both_ways(ordered(letters[7:3]), c(T,NA,F), "ordered"), "dropping factor levels in v(uvec[index])")
  knownbug(test_index_both_ways(Sys.time()+1:9, c(T,NA,F), "POSIXct"))
  test_index_both_ways(Sys.Date()+(-2):6, c(T,NA,F), "Date")  
  test_index_both_ways(as.POSIXlt(Sys.time()+1:9), c(T,NA,F), "POSIXlt")
})

test_that("data.frames can be accessed with '[.unitted'", {
  df <- data.frame(x=1:5,y=LETTERS[6:10],z=rnorm(5),stringsAsFactors=FALSE)
  units <- c("toasts","eggs","hams^2")
  udf <- u(df, units)
  
  # empty indices
  expect_equal(udf[], u(df[],units))
  expect_equal(udf[,], u(df[,],units))
  expect_equal(udf[,,], u(df[,,],units)) #breaks
  expect_error(df[,,,], "unused argument")
  expect_error(udf[,,,], "unused argument")
  
  # numeric indices
  expect_equal(udf[2,2], u(df[2,2],"eggs"))
  expect_equal(udf[2:3,c(3,1)], u(df[2:3,c(3,1)],units[c(3,1)]))
  expect_equal(udf[2:3,], u(df[2:3,],units))
  expect_equal(udf[,c(3,1)], u(df[,c(3,1)],units[c(3,1)]))
  
  # repeated indices
  expect_equal(udf[c(2,4,2,4),2], u(df[c(2,4,2,4),2],"eggs"))
  expect_equal(udf[c(2,4,2,4),2,drop=FALSE], u(df[c(2,4,2,4),2,drop=FALSE],"eggs"))
  expect_equal(udf[,c(2,3,2,3)], u(df[,c(2,3,2,3)],units[c(2,3,2,3)]))
  expect_equal(udf[c(2,4,2,4),c(2,3,2,3)], u(df[c(2,4,2,4),c(2,3,2,3)],units[c(2,3,2,3)]))
  
  # out-of-bounds indices
  expect_error(udf[,5], "undefined columns selected")
  expect_equal(udf[7,], u(df[7,],units))
  expect_equal(udf[7,5], u(df[7,5],units[5]))
  expect_error(udf[7,5:9], "undefined columns selected")
  
  # logical indices
  expect_equal(udf[c(T,T,F,F,T),c(T,F,F)], u(df[c(T,T,F,F,T),c(T,F,F)],units[c(T,F,F)]))
  expect_equal(udf[c(T,T,F,F,T),], u(df[c(T,T,F,F,T),],units))
  expect_equal(udf[,c(T,F,F)], u(df[,c(T,F,F)],units[c(T,F,F)]))
  expect_equal(udf[,c(T,F)], u(df[,c(T,F)],units[c(T,F)]))
  expect_equal(udf[c(T,F),], u(df[c(T,F),],units[]))
  expect_equal(udf[T,T], u(df[T,T],units[T]))
  knownbug(expect_equal(udf[T,F], u(df[T,F],NA)), "Error in units[[col]] : subscript out of bounds")
  expect_equal(udf[F,T], u(df[F,T],units[T]))
  knownbug(expect_equal(udf[F,F], u(df[F,F],NA)), "Error in units[[col]] : subscript out of bounds")
  #future feature:
  #logical.matrix <- matrix(c(rep(c(T,F),7),T),nrow=5,ncol=3)
  #expect_equal(udf[logical.matrix], unname(mapply(function(elem,unit) { u(elem,unit) }, df[logical.matrix], matrix(units,nrow=5,ncol=3,byrow=TRUE)[logical.matrix], SIMPLIFY=FALSE))) # breaks
  
  # character indices
  expect_equal(udf[,c('x','y')], u(df[,c('x','y')],get_units(udf)[c('x','y')]))
  expect_equal(udf[c("1","3"),'y'], u(df[c("1","3"),'y'],get_units(udf)['y']))
  expect_equal(udf[c("1","3"),], u(df[c("1","3"),],units))
  expect_error(udf[,"newname"], "undefined columns selected")
  
  # partial matching
  df <- data.frame(yxz=1:5,yum=LETTERS[6:10],zop=rnorm(5),stringsAsFactors=FALSE,row.names=c("alpha","beta","gamma","delta","epsilon"))
  udf <- u(df, units)
  expect_equal(udf["alp",], u(df["alp",], units))
  expect_error(udf[,"z"], "undefined columns selected")
  expect_error(udf["z"], "undefined columns selected")
  expect_equal(udf[["y",exact=FALSE]], u(df[["y",exact=FALSE]], NA))
  
  # drop argument
  expect_equal(udf[1:3,"zop",drop=T], u(df[1:3,"zop",drop=T],get_units(udf)['zop']))
  expect_equal(udf[1:3,"zop",drop=F], u(df[1:3,"zop",drop=F],get_units(udf)['zop']))
})


test_that("matrices and arrays can be accessed with '[.unitted'", {
  mat <- matrix(1:60,6,10,dimnames=list(onetosix=letters[1:6],ONETEN=LETTERS[11:20]))
  umat <- u(mat, "peanuts")
  arr <- array(1:60,c(3,4,5),dimnames=list(paste0(letters[24:26]," ray"),1:4,LETTERS[21:25]))
  uarr <- u(arr, "popcorns")
  
  # empty indices
  expect_equal(umat[,], u(mat[,],"peanuts"))
  expect_equal(umat[], u(mat[],"peanuts"))
  
  expect_equal(uarr[,,], u(arr[,,],"popcorns"))
  expect_equal(uarr[], u(arr[],"popcorns"))
  
  
  # numeric indices
  expect_equal(umat[2,4], u(mat[2,4],"peanuts"))
  expect_equal(umat[-(1:5),4], u(mat[-(1:5),4],"peanuts"))
  expect_equal(umat[2:3,c(6,8)], u(mat[2:3,c(6,8)],"peanuts"))
  expect_error(mat[2:80,4], "subscript out of bounds")
  expect_error(umat[2:80,4], "subscript out of bounds")
  
  expect_equal(uarr[-6:-2,1:2,], u(arr[-6:-2,1:2,],"popcorns"))
  expect_equal(uarr[2,3,4], u(arr[2,3,4],"popcorns"))
  expect_equal(uarr[2,3,], u(arr[2,3,],"popcorns"))
  expect_equal(uarr[,3,4], u(arr[,3,4],"popcorns"))
  expect_equal(uarr[2,,4], u(arr[2,,4],"popcorns"))
  expect_equal(uarr[2,,], u(arr[2,,],"popcorns"))
  expect_equal(uarr[,2,], u(arr[,2,],"popcorns"))
  expect_equal(uarr[,,2], u(arr[,,2],"popcorns"))
  
  
  # logical indices
  expect_equal(umat[1:6==2,1:7==4], u(mat[2,4],"peanuts"))
  expect_equal(umat[c(F,T,T,F,F,F),1:10 %in% c(6,8)], u(mat[c(F,T,T,F,F,F),1:10 %in% c(6,8)],"peanuts"))
  expect_equal(umat[1:6==2,F], u(mat[1:6==2,F],"peanuts"))
  expect_error(mat[c(T,F,F,F,F,T,T),1:7==4], "logical subscript too long")
  expect_error(umat[c(T,F,F,F,F,T,T),1:7==4], "logical subscript too long")
  
  expect_equal(uarr[1:3==2,1:4==4,], u(arr[1:3==2,1:4==4,],"popcorns"))
  expect_equal(uarr[c(F,T,T),rep(T,4),c(F,F,F,T)], u(arr[c(F,T,T),rep(T,4),c(F,F,F,T)],"popcorns"))
  i1 <- 1:3==2; i2 <- 1:4==1; i3 <- 1:5==4
  expect_equal(uarr[F,F,F], u(arr[F,F,F],"popcorns"))
  expect_equal(uarr[i1,i2,i3], u(arr[i1,i2,i3],"popcorns"))
  expect_equal(uarr[i1,i2,], u(arr[i1,i2,],"popcorns"))
  expect_equal(uarr[i1,,i3], u(arr[i1,,i3],"popcorns"))
  expect_equal(uarr[,i2,i3], u(arr[,i2,i3],"popcorns"))
  expect_equal(uarr[i1,,], u(arr[i1,,],"popcorns"))
  expect_equal(uarr[,i2,], u(arr[,i2,],"popcorns"))
  expect_equal(uarr[,,i3], u(arr[,,i3],"popcorns"))
  
  
  # character indices
  expect_equal(umat["b","N"], u(20,"peanuts"))
  expect_equal(unname(umat[c('b','c'),c("P","R")]), u(matrix(c(32,33,44,45),2),"peanuts"))
  expect_error(umat["b","No no no"], "subscript out of bounds")
  
  expect_equal(uarr["y ray",'4',], u(arr["y ray",'4',],"popcorns"))
  expect_equal(uarr[c('x ray'),c('2','2'),c('X','U')], u(arr[c('x ray'),c('2','2'),c('X','U')],"popcorns"))
  i1 <- 'z ray'; i2 <- c('4','1','1'); i3 <- 'Y'
  expect_error(arr['cubic','poly','nomial'], "subscript out of bounds")
  expect_error(uarr['cubic','poly','nomial'], "subscript out of bounds")
  expect_equal(uarr[i1,i2,i3], u(arr[i1,i2,i3],"popcorns"))
  expect_equal(uarr[i1,i2,], u(arr[i1,i2,],"popcorns"))
  expect_equal(uarr[i1,,i3], u(arr[i1,,i3],"popcorns"))
  expect_equal(uarr[,i2,i3], u(arr[,i2,i3],"popcorns"))
  expect_equal(uarr[i1,,], u(arr[i1,,],"popcorns"))
  expect_equal(uarr[,i2,], u(arr[,i2,],"popcorns"))
  expect_equal(uarr[,,i3], u(arr[,,i3],"popcorns"))
})


#### [[.unitted ####

test_that("vectors can be accessed with '[[.unitted'", {
  
  #?"[[<-" says, "[[ can be used to select a single element dropping names,
  #whereas [ keeps them, e.g., in c(abc = 123)[1]."
  
  vvec <- 1:10
  names(vvec) <- LETTERS[5:14]
  uvec <- u(vvec,"hats")
  
  # numeric indices
  expect_equal(uvec[[2]], u(vvec[[2]],"hats"))
  expect_error(uvec[[NA]], "subscript out of bounds")
  expect_error(uvec[[c(3,9,1)]], "attempt to select more than one element")
  expect_error(uvec[[-2]], "attempt to select more than one element")
  expect_error(uvec[[-(2:10)]], "attempt to select more than one element")
  
  # logical indices  
  expect_equal(uvec[[T]], uvec[[1]]) # trivial; T=1
  expect_error(uvec[[F]], "attempt to select less than one element")
  expect_error(uvec[[rep(T,10)]], "attempt to select more than one element")
  
  # character indices
  expect_equal(uvec[["F"]], u(vvec[["F"]],"hats"))
  expect_error(uvec[[c("N","H","N")]], "attempt to select more than one element")
  expect_error(uvec[["x"]], "subscript out of bounds")
  
  # multiple indices
  expect_error(uvec[[]], "invalid subscript type")
  expect_error(uvec[[1,1]], "incorrect number of subscripts")
  expect_error(uvec[["quantum","physics"]], "incorrect number of subscripts")
})

test_that("data.frames can be accessed with '[[.unitted'", {
  
  #?"[[<-" says, "[[ can be used to select a single element dropping names,
  #whereas [ keeps them, e.g., in c(abc = 123)[1]."
  
  df <- data.frame(yxz=1:5,yum=LETTERS[6:10],zop=rnorm(5),stringsAsFactors=FALSE,row.names=c("alpha","beta","gamma","delta","epsilon"))
  units <- c(yxz="toasts",yum="eggs",zop="hams^2")
  udf <- u(df, units)
  
  # [[i]] can't select for rows; selects for columns only
  expect_equal(udf[[1]], u(df[[1]],units[1])) # selects first column
  expect_equal(udf[["alpha"]], u(df[["alpha"]],NA)) # 1 value = columns; trying rows gives NULL
  expect_equal(udf[["zop"]], u(df[["zop"]],units["zop"])) # naming a column works fine
  expect_error(udf[[c("yxz","zop")]], "subscript out of bounds") # two columns is not allowed
  
  # partial matching - columns only
  expect_equal(udf[["almega",exact=FALSE]], u(df[["almega",exact=FALSE]], NA)) # nonexistent - gives NULL
  expect_equal(udf[["y",     exact=FALSE]], u(df[["y",     exact=FALSE]], NA)) # ambiguous - gives NULL
  expect_equal(udf[["yu",    exact=FALSE]], u(df[["yu",  exact=FALSE]], units["yum"])) # inexact but unambiguous
  expect_equal(udf[["zop",   exact=FALSE]], u(df[["zop", exact=FALSE]], units["zop"])) # inexact but very unambiguous
  
  # df has funny behavior for vectors of column or row indices
  expect_equal(udf[["alpha",c(1,2)]], u(df[["alpha",c(1,2)]], NA))
  expect_error(udf[["alpha",c("yxz","yum")]], "subscript out of bounds")
  expect_error(udf[["alpha",c(1,2,3)]], "recursive indexing failed") 
  expect_error(udf[[c(1,2,3),"zop"]], "attempt to select more than one element") 
  expect_error(udf[[c(1,2,3),c(1,2,3)]], "recursive indexing failed")
  
  # [[i,j]] selects for one row, one column. "[[ can only be used to select one element"
  expect_equal(udf[[4,3]], u(df[[4,3]],           units[3]))
  expect_equal(udf[["alpha","yxz"]], u(df[["alpha","yxz"]], units["yxz"]))
  
  # partial matching - rows and columns
  expect_equal(udf[["gamma","yxz",exact=FALSE]], u(df[["gamma","yxz",exact=FALSE]],units["yxz"]))
  expect_equal(udf[["gam",  "yxz",exact=FALSE]], u(df[["gam",  "yxz",exact=FALSE]],units["yxz"]))
  expect_equal(udf[["gam",  "yu", exact=FALSE]], u(df[["gam",  "yu", exact=FALSE]],units["yum"]))
  
})

test_that("matrices and arrays can be accessed with '[[.unitted'", {
  
  #?"[[<-" says, "[[ can be used to select a single element dropping names,
  #whereas [ keeps them, e.g., in c(abc = 123)[1]."
  
  mat <- matrix(1:60,6,10,dimnames=list(onetosix=letters[1:6],ONETEN=LETTERS[11:20]))
  umat <- u(mat, "peanuts")
  arr <- array(1:60,c(3,4,5),dimnames=list(paste0(letters[24:26]," ray"),1:4,LETTERS[21:25]))
  uarr <- u(arr, "popcorns")
  
  # empty indices
  expect_error(umat[[]], "invalid subscript type")
  expect_error(umat[[,]], "invalid subscript type")
  expect_error(uarr[[]], "invalid subscript type")
  expect_error(uarr[[,]], "incorrect number of subscripts")
  expect_error(uarr[[,,]], "invalid subscript type")
    
  # one numeric index
  expect_equal(umat[[4]], u(mat[[4]],"peanuts"))
  expect_error(umat[[1:5]], "attempt to select more than one element")
  expect_error(umat[[NA]], "subscript out of bounds")
  expect_equal(uarr[[4]], u(arr[[4]],"popcorns"))
  expect_error(uarr[[1:5]], "attempt to select more than one element")
  expect_error(uarr[[NA]], "subscript out of bounds")
  
  # two numeric indices
  expect_equal(umat[[2,4]], u(mat[[2,4]],"peanuts"))
  expect_error(umat[[1:5,4]], "attempt to select more than one element")
  expect_error(umat[[NA,4]], "subscript out of bounds")
  expect_error(umat[[-3,4]], "attempt to select") #error message is inconsistent, "more" or "less" than one unit
  expect_equal(uarr[[2,4,3]], u(arr[[2,4,3]],"popcorns"))
  expect_error(uarr[[1:5,4,3]], "attempt to select more than one element")
  expect_error(uarr[[3,4]], "incorrect number of subscripts")
  expect_error(uarr[[3,4,]], "invalid subscript type")
  expect_error(uarr[[NA,4,3]], "subscript out of bounds")
  expect_error(uarr[[-3,4,3]], "attempt to select")
  
  # logical indices
  expect_equal(umat[[T]], umat[[1]])
  expect_error(umat[[F]], "attempt to select less than one element")
  expect_error(umat[[F,4]], "attempt to select less than one element")
  expect_equal(uarr[[T]], uarr[[1]])
  expect_error(uarr[[F]], "attempt to select less than one element")
  expect_error(uarr[[F,4,3]], "attempt to select less than one element")
  
  # character indices
  expect_equal(umat[["b","N"]], u(20,"peanuts"))
  expect_error(umat[[c('b','c'),c("P","R")]], "attempt to select more than one element")
  expect_error(umat[["b","No no no"]], "subscript out of bounds")
  expect_equal(uarr[["y ray",'4',"W"]], u(arr[["y ray",'4',"W"]],"popcorns"))
  expect_error(uarr[["y ray",'kiddy',"W"]], "subscript out of bounds")
  expect_error(uarr[["y ray"]], "subscript out of bounds")
  expect_error(uarr[["2"]], "subscript out of bounds")

})

#### $.unitted ####

test_that("data.frames can be accessed with '$.unitted'", {
  # ?"$<-.data.frame" says, "There is no data.frame method for $, so x$name uses
  # the default method which treats x as a list."
  
  df <- data.frame(yxz=1:5,yum=LETTERS[6:10],zop=rnorm(5),stringsAsFactors=FALSE,row.names=c("alpha","beta","gamma","delta","epsilon"))
  units <- c(yxz="toasts",yum="eggs",zop="hams^2")
  udf <- u(df, units)
  
  # Existing column names
  expect_equal(udf$yum, u(df$yum,units["yum"]))
  expect_equal(udf$'yum', u(df$'yum',units["yum"]))
  
  # Nonexistent column names
  expect_equal(udf$youthere, u(df$youthere,NA))
})


test_that("lists can be accessed with '$.unitted'", {
  vlist <- list(yxz=1:5,yum=LETTERS[6:10],zop=rnorm(5))
  units <- c(yxz="toasts",yum="eggs",zop="hams^2")
  knownbug(expect_that(ulist <- u(vlist, units), gives_warning("The implementation of unitted lists is currently primitive")), "a character argument describing a units bundle must have length 1")
  ulist <- lapply(1:length(vlist), function(listnum) { u(vlist[[listnum]], units[listnum]) })
  names(ulist) <- names(vlist)
  
  # Existing element names
  expect_equal(ulist$yum, u(vlist$yum,units["yum"]))
  expect_equal(ulist$'yum', u(vlist$'yum',units["yum"]))
  
  # Nonexistent column names - returns NULL
  expect_equal(ulist$youthere, vlist$youthere)
  
  # unitted POSIXlt vectors have historically been problematic
  vvec <- as.POSIXlt(Sys.time()+1:9)
  uvec <- u(vvec,"dates")
  expect_equal(v(uvec), vvec)
  expect_equal(names(uvec), names(vvec))
  expect_equal(uvec, uvec)
})

