context("access")
knownbug <- function(expr, notes) invisible(NULL)

#### [.unitted ####

test_that("vectors can be accessed with '[.unitted' by element numbers", {
  vvec <- 1:10
  names(vvec) <- LETTERS[5:14]
  uvec <- u(vvec,"hats")
  
  # numeric indices
  expect_that(uvec[2],        equals(u(vvec[2],"hats")),        info="indexing by an existing element number")
  expect_that(uvec[c(3,9,1)], equals(u(vvec[c(3,9,1)],"hats")), info="indexing by several existing element numbers")
  expect_that(uvec[c(8,NA)],  equals(u(vvec[c(8,NA)],"hats")),  info="indexing with NAs")
  expect_that(uvec[24],       equals(u(vvec[24],"hats")),       info="indexing by a too-high element number")
  expect_that(uvec[-2],       equals(u(vvec[-2],"hats")),       info="indexing by a realistic negative number")
  expect_that(uvec[-89],      equals(u(vvec[-89],"hats")),      info="indexing by a too-low negative number")
  
  # logical indices  
  expect_that(uvec[rep(T,10)],     equals(u(vvec[rep(T,10)],"hats")),     info="indexing by all TRUEs")
  expect_that(uvec[rep(T,20)],     equals(u(vvec[rep(T,20)],"hats")),     info="indexing by a long list of TRUEs")
  expect_that(uvec[rep(F,50)],     equals(u(vvec[rep(F,50)],"hats")),     info="indexing by a long list of FALSEs")
  expect_that(uvec[c(T,F,F)],      equals(u(vvec[c(T,F,F)],"hats")),      info="indexing by a short list of logicals")
  expect_that(uvec[c(T,NA,F)],     equals(u(vvec[c(T,NA,F)],"hats")),     info="indexing with logicals and NAs")
  expect_that(uvec[rep(c(T,F),8)], equals(u(vvec[rep(c(T,F),8)],"hats")), info="indexing by many alternating logicals")
  
  # character indices
  expect_that(uvec["F"],             equals(u(vvec["F"],"hats")),            info="indexing by a single string")
  expect_that(uvec[c("N","H","N")],  equals(u(vvec[c("N","H","N")],"hats")), info="indexing by a vector of strings")
  expect_that(uvec["x"],             equals(u(vvec["x"],"hats")),            info="indexing by a non-name string")
  vvec2 <- 1:10 # no names
  uvec2 <- u(vvec2,"hats") # no names
  expect_that(uvec2["F"],            equals(u(vvec2["F"],"hats")),            info="indexing an unnamed vector by a single string")
  expect_that(uvec2[c("N","H","N")], equals(u(vvec2[c("N","H","N")],"hats")), info="indexing an unnamed vector by a vector of strings")

  # multiple indices
  expect_that(uvec[], equals(uvec), info="empty args to [] change nothing")
  expect_that(uvec[,], throws_error("incorrect number of dimensions"))
  expect_that(uvec[1,1], throws_error("incorrect number of dimensions"))
  expect_that(uvec[,,2], throws_error("incorrect number of dimensions"))
  expect_that(uvec["quantum","physics"], throws_error("incorrect number of dimensions"))
})

test_that("vectors of all types can be accessed with '[.unitted'", {
  test_index_both_ways <- function(vvec, index, note) {
    uvec <- u(vvec,"hats rats^-1")
    info <- paste0("when indexing ",note," c(",paste0(vvec[1:4],collapse=","),",...) by c(",paste(index,collapse=","),")")
    expect_that(uvec[index], equals(u(vvec[index],"hats rats^-1")), info=info)
    expect_that(v(uvec[index]), equals(vvec[index]), info=info)
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
  expect_that(udf[], equals(u(df[],units)))
  expect_that(udf[,], equals(u(df[,],units)))
  expect_that(udf[,,], equals(u(df[,,],units))) #breaks
  expect_that(df[,,,], throws_error("unused argument"))
  expect_that(udf[,,,], throws_error("unused argument"))
  
  # numeric indices
  expect_that(udf[2,2], equals(u(df[2,2],"eggs")))
  expect_that(udf[2:3,c(3,1)], equals(u(df[2:3,c(3,1)],units[c(3,1)])))
  expect_that(udf[2:3,], equals(u(df[2:3,],units)))
  expect_that(udf[,c(3,1)], equals(u(df[,c(3,1)],units[c(3,1)])))
  
  # repeated indices
  expect_that(udf[c(2,4,2,4),2], equals(u(df[c(2,4,2,4),2],"eggs")))
  expect_that(udf[c(2,4,2,4),2,drop=FALSE], equals(u(df[c(2,4,2,4),2,drop=FALSE],"eggs")))
  expect_that(udf[,c(2,3,2,3)], equals(u(df[,c(2,3,2,3)],units[c(2,3,2,3)])))
  expect_that(udf[c(2,4,2,4),c(2,3,2,3)], equals(u(df[c(2,4,2,4),c(2,3,2,3)],units[c(2,3,2,3)])))
  
  # out-of-bounds indices
  expect_that(udf[,5], throws_error("undefined columns selected"))
  expect_that(udf[7,], equals(u(df[7,],units)))
  expect_that(udf[7,5], equals(u(df[7,5],units[5])))
  expect_that(udf[7,5:9], throws_error("undefined columns selected"))
  
  # logical indices
  expect_that(udf[c(T,T,F,F,T),c(T,F,F)], equals(u(df[c(T,T,F,F,T),c(T,F,F)],units[c(T,F,F)])))
  expect_that(udf[c(T,T,F,F,T),], equals(u(df[c(T,T,F,F,T),],units)))
  expect_that(udf[,c(T,F,F)], equals(u(df[,c(T,F,F)],units[c(T,F,F)])))
  expect_that(udf[,c(T,F)], equals(u(df[,c(T,F)],units[c(T,F)])))
  expect_that(udf[c(T,F),], equals(u(df[c(T,F),],units[])))
  expect_that(udf[T,T], equals(u(df[T,T],units[T])))
  knownbug(expect_that(udf[T,F], equals(u(df[T,F],NA))), "Error in units[[col]] : subscript out of bounds")
  expect_that(udf[F,T], equals(u(df[F,T],units[T])))
  knownbug(expect_that(udf[F,F], equals(u(df[F,F],NA))), "Error in units[[col]] : subscript out of bounds")
  #future feature:
  #logical.matrix <- matrix(c(rep(c(T,F),7),T),nrow=5,ncol=3)
  #expect_that(udf[logical.matrix], equals(unname(mapply(function(elem,unit) { u(elem,unit) }, df[logical.matrix], matrix(units,nrow=5,ncol=3,byrow=TRUE)[logical.matrix], SIMPLIFY=FALSE)))) # breaks
  
  # character indices
  expect_that(udf[,c('x','y')], equals(u(df[,c('x','y')],get_units(udf)[c('x','y')])))
  expect_that(udf[c("1","3"),'y'], equals(u(df[c("1","3"),'y'],get_units(udf)['y'])))
  expect_that(udf[c("1","3"),], equals(u(df[c("1","3"),],units)))
  expect_that(udf[,"newname"], throws_error("undefined columns selected"))
  
  # partial matching
  df <- data.frame(yxz=1:5,yum=LETTERS[6:10],zop=rnorm(5),stringsAsFactors=FALSE,row.names=c("alpha","beta","gamma","delta","epsilon"))
  udf <- u(df, units)
  expect_that(udf["alp",], equals(u(df["alp",], units)))
  expect_that(udf[,"z"], throws_error("undefined columns selected"))
  expect_that(udf["z"], throws_error("undefined columns selected"))
  expect_that(udf[["y",exact=FALSE]], equals(u(df[["y",exact=FALSE]], NA)))
  
  # drop argument
  expect_that(udf[1:3,"zop",drop=T], equals(u(df[1:3,"zop",drop=T],get_units(udf)['zop'])))
  expect_that(udf[1:3,"zop",drop=F], equals(u(df[1:3,"zop",drop=F],get_units(udf)['zop'])))
})


test_that("matrices and arrays can be accessed with '[.unitted'", {
  mat <- matrix(1:60,6,10,dimnames=list(onetosix=letters[1:6],ONETEN=LETTERS[11:20]))
  umat <- u(mat, "peanuts")
  arr <- array(1:60,c(3,4,5),dimnames=list(paste0(letters[24:26]," ray"),1:4,LETTERS[21:25]))
  uarr <- u(arr, "popcorns")
  
  # empty indices
  expect_that(umat[,], equals(u(mat[,],"peanuts")))
  expect_that(umat[], equals(u(mat[],"peanuts")))
  
  expect_that(uarr[,,], equals(u(arr[,,],"popcorns")))
  expect_that(uarr[], equals(u(arr[],"popcorns")))
  
  
  # numeric indices
  expect_that(umat[2,4], equals(u(mat[2,4],"peanuts")))
  expect_that(umat[-(1:5),4], equals(u(mat[-(1:5),4],"peanuts")))
  expect_that(umat[2:3,c(6,8)], equals(u(mat[2:3,c(6,8)],"peanuts")))
  expect_that(mat[2:80,4], throws_error("subscript out of bounds"))
  expect_that(umat[2:80,4], throws_error("subscript out of bounds"))
  
  expect_that(uarr[-6:-2,1:2,], equals(u(arr[-6:-2,1:2,],"popcorns")))
  expect_that(uarr[2,3,4], equals(u(arr[2,3,4],"popcorns")))
  expect_that(uarr[2,3,], equals(u(arr[2,3,],"popcorns")))
  expect_that(uarr[,3,4], equals(u(arr[,3,4],"popcorns")))
  expect_that(uarr[2,,4], equals(u(arr[2,,4],"popcorns")))
  expect_that(uarr[2,,], equals(u(arr[2,,],"popcorns")))
  expect_that(uarr[,2,], equals(u(arr[,2,],"popcorns")))
  expect_that(uarr[,,2], equals(u(arr[,,2],"popcorns")))
  
  
  # logical indices
  expect_that(umat[1:6==2,1:7==4], equals(u(mat[2,4],"peanuts")))
  expect_that(umat[c(F,T,T,F,F,F),1:10 %in% c(6,8)], equals(u(mat[c(F,T,T,F,F,F),1:10 %in% c(6,8)],"peanuts")))
  expect_that(umat[1:6==2,F], equals(u(mat[1:6==2,F],"peanuts")))
  expect_that(mat[c(T,F,F,F,F,T,T),1:7==4], throws_error("logical subscript too long"))
  expect_that(umat[c(T,F,F,F,F,T,T),1:7==4], throws_error("logical subscript too long"))
  
  expect_that(uarr[1:3==2,1:4==4,], equals(u(arr[1:3==2,1:4==4,],"popcorns")))
  expect_that(uarr[c(F,T,T),rep(T,4),c(F,F,F,T)], equals(u(arr[c(F,T,T),rep(T,4),c(F,F,F,T)],"popcorns")))
  i1 <- 1:3==2; i2 <- 1:4==1; i3 <- 1:5==4
  expect_that(uarr[F,F,F], equals(u(arr[F,F,F],"popcorns")))
  expect_that(uarr[i1,i2,i3], equals(u(arr[i1,i2,i3],"popcorns")))
  expect_that(uarr[i1,i2,], equals(u(arr[i1,i2,],"popcorns")))
  expect_that(uarr[i1,,i3], equals(u(arr[i1,,i3],"popcorns")))
  expect_that(uarr[,i2,i3], equals(u(arr[,i2,i3],"popcorns")))
  expect_that(uarr[i1,,], equals(u(arr[i1,,],"popcorns")))
  expect_that(uarr[,i2,], equals(u(arr[,i2,],"popcorns")))
  expect_that(uarr[,,i3], equals(u(arr[,,i3],"popcorns")))
  
  
  # character indices
  expect_that(umat["b","N"], equals(u(20,"peanuts")))
  expect_that(unname(umat[c('b','c'),c("P","R")]), equals(u(matrix(c(32,33,44,45),2),"peanuts")))
  expect_that(umat["b","No no no"], throws_error("subscript out of bounds"))
  
  expect_that(uarr["y ray",'4',], equals(u(arr["y ray",'4',],"popcorns")))
  expect_that(uarr[c('x ray'),c('2','2'),c('X','U')], equals(u(arr[c('x ray'),c('2','2'),c('X','U')],"popcorns")))
  i1 <- 'z ray'; i2 <- c('4','1','1'); i3 <- 'Y'
  expect_that(arr['cubic','poly','nomial'], throws_error("subscript out of bounds"))
  expect_that(uarr['cubic','poly','nomial'], throws_error("subscript out of bounds"))
  expect_that(uarr[i1,i2,i3], equals(u(arr[i1,i2,i3],"popcorns")))
  expect_that(uarr[i1,i2,], equals(u(arr[i1,i2,],"popcorns")))
  expect_that(uarr[i1,,i3], equals(u(arr[i1,,i3],"popcorns")))
  expect_that(uarr[,i2,i3], equals(u(arr[,i2,i3],"popcorns")))
  expect_that(uarr[i1,,], equals(u(arr[i1,,],"popcorns")))
  expect_that(uarr[,i2,], equals(u(arr[,i2,],"popcorns")))
  expect_that(uarr[,,i3], equals(u(arr[,,i3],"popcorns")))
})


#### [[.unitted ####

test_that("vectors can be accessed with '[[.unitted'", {
  
  #?"[[<-" says, "[[ can be used to select a single element dropping names,
  #whereas [ keeps them, e.g., in c(abc = 123)[1]."
  
  vvec <- 1:10
  names(vvec) <- LETTERS[5:14]
  uvec <- u(vvec,"hats")
  
  # numeric indices
  expect_that(uvec[[2]],        equals(u(vvec[[2]],"hats")))
  expect_that(uvec[[NA]],       throws_error("subscript out of bounds"))
  expect_that(uvec[[c(3,9,1)]], throws_error("attempt to select more than one element"))
  expect_that(uvec[[-2]],       throws_error("attempt to select more than one element"))
  expect_that(uvec[[-(2:10)]],  throws_error("attempt to select more than one element"))
  
  # logical indices  
  expect_that(uvec[[T]],        equals(uvec[[1]])) # trivial; T=1
  expect_that(uvec[[F]],        throws_error("attempt to select less than one element"))
  expect_that(uvec[[rep(T,10)]],throws_error("attempt to select more than one element"))
  
  # character indices
  expect_that(uvec[["F"]],            equals(u(vvec[["F"]],"hats")))
  expect_that(uvec[[c("N","H","N")]], throws_error("attempt to select more than one element"))
  expect_that(uvec[["x"]],            throws_error("subscript out of bounds"))
  
  # multiple indices
  expect_that(uvec[[]],    throws_error("invalid subscript type"))
  expect_that(uvec[[1,1]], throws_error("incorrect number of subscripts"))
  expect_that(uvec[["quantum","physics"]], throws_error("incorrect number of subscripts"))
})

test_that("data.frames can be accessed with '[[.unitted'", {
  
  #?"[[<-" says, "[[ can be used to select a single element dropping names,
  #whereas [ keeps them, e.g., in c(abc = 123)[1]."
  
  df <- data.frame(yxz=1:5,yum=LETTERS[6:10],zop=rnorm(5),stringsAsFactors=FALSE,row.names=c("alpha","beta","gamma","delta","epsilon"))
  units <- c(yxz="toasts",yum="eggs",zop="hams^2")
  udf <- u(df, units)
  
  # [[i]] can't select for rows; selects for columns only
  expect_that(udf[[1]],       equals(u(df[[1]],units[1]))) # selects first column
  expect_that(udf[["alpha"]], equals(u(df[["alpha"]],NA))) # 1 value = columns; trying rows gives NULL
  expect_that(udf[["zop"]],   equals(u(df[["zop"]],units["zop"]))) # naming a column works fine
  expect_that(udf[[c("yxz","zop")]], throws_error("subscript out of bounds")) # two columns is not allowed
  
  # partial matching - columns only
  expect_that(udf[["almega",exact=FALSE]], equals(u(df[["almega",exact=FALSE]], NA))) # nonexistent - gives NULL
  expect_that(udf[["y",     exact=FALSE]], equals(u(df[["y",     exact=FALSE]], NA))) # ambiguous - gives NULL
  expect_that(udf[["yu",    exact=FALSE]], equals(u(df[["yu",  exact=FALSE]], units["yum"]))) # inexact but unambiguous
  expect_that(udf[["zop",   exact=FALSE]], equals(u(df[["zop", exact=FALSE]], units["zop"]))) # inexact but very unambiguous
  
  # df has funny behavior for vectors of column or row indices
  expect_that(udf[["alpha",c(1,2)]],         equals(u(df[["alpha",c(1,2)]], NA)))
  expect_that(udf[["alpha",c("yxz","yum")]], throws_error("subscript out of bounds"))
  expect_that(udf[["alpha",c(1,2,3)]],       throws_error("recursive indexing failed")) 
  expect_that(udf[[c(1,2,3),"zop"]],         throws_error("attempt to select more than one element")) 
  expect_that(udf[[c(1,2,3),c(1,2,3)]],      throws_error("recursive indexing failed"))
  
  # [[i,j]] selects for one row, one column. "[[ can only be used to select one element"
  expect_that(udf[[4,3]],           equals(u(df[[4,3]],           units[3])))
  expect_that(udf[["alpha","yxz"]], equals(u(df[["alpha","yxz"]], units["yxz"])))
  
  # partial matching - rows and columns
  expect_that(udf[["gamma","yxz",exact=FALSE]], equals(u(df[["gamma","yxz",exact=FALSE]],units["yxz"])))
  expect_that(udf[["gam",  "yxz",exact=FALSE]], equals(u(df[["gam",  "yxz",exact=FALSE]],units["yxz"])))
  expect_that(udf[["gam",  "yu", exact=FALSE]], equals(u(df[["gam",  "yu", exact=FALSE]],units["yum"])))
  
})

test_that("matrices and arrays can be accessed with '[[.unitted'", {
  
  #?"[[<-" says, "[[ can be used to select a single element dropping names,
  #whereas [ keeps them, e.g., in c(abc = 123)[1]."
  
  mat <- matrix(1:60,6,10,dimnames=list(onetosix=letters[1:6],ONETEN=LETTERS[11:20]))
  umat <- u(mat, "peanuts")
  arr <- array(1:60,c(3,4,5),dimnames=list(paste0(letters[24:26]," ray"),1:4,LETTERS[21:25]))
  uarr <- u(arr, "popcorns")
  
  # empty indices
  expect_that(umat[[]],   throws_error("invalid subscript type"))
  expect_that(umat[[,]],  throws_error("invalid subscript type"))
  expect_that(uarr[[]],   throws_error("invalid subscript type"))
  expect_that(uarr[[,]],  throws_error("incorrect number of subscripts"))
  expect_that(uarr[[,,]], throws_error("invalid subscript type"))
    
  # one numeric index
  expect_that(umat[[4]],   equals(u(mat[[4]],"peanuts")))
  expect_that(umat[[1:5]], throws_error("attempt to select more than one element"))
  expect_that(umat[[NA]],  throws_error("subscript out of bounds"))
  expect_that(uarr[[4]],   equals(u(arr[[4]],"popcorns")))
  expect_that(uarr[[1:5]], throws_error("attempt to select more than one element"))
  expect_that(uarr[[NA]],  throws_error("subscript out of bounds"))
  
  # two numeric indices
  expect_that(umat[[2,4]],     equals(u(mat[[2,4]],"peanuts")))
  expect_that(umat[[1:5,4]],   throws_error("attempt to select more than one element"))
  expect_that(umat[[NA,4]],    throws_error("subscript out of bounds"))
  expect_that(umat[[-3,4]],    throws_error("attempt to select")) #error message is inconsistent, "more" or "less" than one unit
  expect_that(uarr[[2,4,3]],   equals(u(arr[[2,4,3]],"popcorns")))
  expect_that(uarr[[1:5,4,3]], throws_error("attempt to select more than one element"))
  expect_that(uarr[[3,4]],     throws_error("incorrect number of subscripts"))
  expect_that(uarr[[3,4,]],    throws_error("invalid subscript type"))
  expect_that(uarr[[NA,4,3]],  throws_error("subscript out of bounds"))
  expect_that(uarr[[-3,4,3]],  throws_error("attempt to select"))
  
  # logical indices
  expect_that(umat[[T]],     equals(umat[[1]]))
  expect_that(umat[[F]],     throws_error("attempt to select less than one element"))
  expect_that(umat[[F,4]],   throws_error("attempt to select less than one element"))
  expect_that(uarr[[T]],     equals(uarr[[1]]))
  expect_that(uarr[[F]],     throws_error("attempt to select less than one element"))
  expect_that(uarr[[F,4,3]], throws_error("attempt to select less than one element"))
  
  # character indices
  expect_that(umat[["b","N"]],               equals(u(20,"peanuts")))
  expect_that(umat[[c('b','c'),c("P","R")]], throws_error("attempt to select more than one element"))
  expect_that(umat[["b","No no no"]],        throws_error("subscript out of bounds"))
  expect_that(uarr[["y ray",'4',"W"]],       equals(u(arr[["y ray",'4',"W"]],"popcorns")))
  expect_that(uarr[["y ray",'kiddy',"W"]],   throws_error("subscript out of bounds"))
  expect_that(uarr[["y ray"]],               throws_error("subscript out of bounds"))
  expect_that(uarr[["2"]],                   throws_error("subscript out of bounds"))

})

#### $.unitted ####

test_that("data.frames can be accessed with '$.unitted'", {
  # ?"$<-.data.frame" says, "There is no data.frame method for $, so x$name uses
  # the default method which treats x as a list."
  
  df <- data.frame(yxz=1:5,yum=LETTERS[6:10],zop=rnorm(5),stringsAsFactors=FALSE,row.names=c("alpha","beta","gamma","delta","epsilon"))
  units <- c(yxz="toasts",yum="eggs",zop="hams^2")
  udf <- u(df, units)
  
  # Existing column names
  expect_that(udf$yum,   equals(u(df$yum,units["yum"])))
  expect_that(udf$'yum', equals(u(df$'yum',units["yum"])))
  
  # Nonexistent column names
  expect_that(udf$youthere, equals(u(df$youthere,NA)))
})


test_that("lists can be accessed with '$.unitted'", {
  vlist <- list(yxz=1:5,yum=LETTERS[6:10],zop=rnorm(5))
  units <- c(yxz="toasts",yum="eggs",zop="hams^2")
  knownbug(expect_that(ulist <- u(vlist, units), gives_warning("The implementation of unitted lists is currently primitive")), "a character argument describing a units bundle must have length 1")
  ulist <- lapply(1:length(vlist), function(listnum) { u(vlist[[listnum]], units[listnum]) })
  names(ulist) <- names(vlist)
  
  # Existing element names
  expect_that(ulist$yum, equals(u(vlist$yum,units["yum"])))
  expect_that(ulist$'yum', equals(u(vlist$'yum',units["yum"])))
  
  # Nonexistent column names - returns NULL
  expect_that(ulist$youthere, equals(vlist$youthere))
  
  # unitted POSIXlt vectors have historically been problematic
  vvec <- as.POSIXlt(Sys.time()+1:9)
  uvec <- u(vvec,"dates")
  expect_that(v(uvec), equals(vvec))
  expect_that(names(uvec), equals(names(vvec)))
  expect_that(uvec, equals(uvec))
})

