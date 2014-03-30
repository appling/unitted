context("combine")

#### c.unitted ####

test_that("c.unitted works", {
  
  # empty should return NULL
  expect_that(c.unitted(), equals(c()))
  
  # vectors - just one vector to concatenate
  expect_that(c.unitted(1), throws_error("All elements must be unitted unless recursive==TRUE"))
  expect_that(c.unitted(u(1,"helmets")), equals(u(c(1),"helmets")))
  expect_that(c(u(1,"helmets")), equals(u(c(1),"helmets")))
  expect_that(c.unitted(u(1:40,"hornet")), equals(u(c(1:40),"hornet")))
  expect_that(c(u(1:40,"hornet")), equals(u(c(1:40),"hornet")))
  expect_that(c.unitted(u(letters[26:1],"horns")), equals(u(c(letters[26:1]),"horns")))
  expect_that(c(u(letters[26:1],"horns")), equals(u(c(letters[26:1]),"horns")))
  
  # vectors - concatenate more than one
  expect_that(c(u(letters[26:21],"corns"),u(LETTERS[4:12],"corns")), equals(u(c(letters[26:21],LETTERS[4:12]),"corns")))
  expect_that(c(u(letters[26:21],"corns"),u(LETTERS[4:12],"cornucopias")), throws_error("All elements must have the same units"))
  expect_that(c(u(26:21,"mazes"),u(4:12,"mazes")), equals(u(c(26:21,4:12),"mazes")))
  expect_that(c(u(26:21,"mazes"),u(4:12,"maizes")), throws_error("All elements must have the same units"))
  expect_that(c(u(26:21,"mazes"),u(4:12,"mazes"),u(LETTERS[4:12],"mazes")), equals(u(c(26:21,4:12,LETTERS[4:12]),"mazes")))
  
  # data.frames
  df1 <- data.frame(A=1:5, B=12:16)
  df2 <- data.frame(X=letters[4:12], Y=LETTERS[6:14])
  dfu1 <- data.frame(A=u(1:5,"cats"), B=u(12:16,"cats"))
  dfu2 <- data.frame(X=letters[4:12], Y=u(LETTERS[6:14],"dogs"))
  dfu3 <- data.frame(X=u(letters[4:12],"cats"), Y=u(LETTERS[6:14],"cats"))
  dfu4 <- data.frame(X=u(letters[4:12],"cats"), Y=u(LETTERS[6:14],"dogs"))
  udf1 <- u(dfu1)
  udf2 <- u(dfu2)
  udf3 <- u(dfu3)
  udf4 <- u(dfu4)
  # joining two fully unitted data.frames non-recursively
  expect_that(c(udf1,udf3), equals(lapply(c(df1,df2),u,"cats"))) # breaks - shouldn't be recursive
  expect_that(c(udf1,udf3), equals(c(dfu1,dfu3))) # breaks - shouldn't be recursive
  expect_that(c(udf1,udf3,recursive=FALSE), equals(lapply(c(df1,df2),u,"cats"))) # breaks - shouldn't be recursive. it's ignoring the recursive=FALSE argument.
  # joining two fully unitted data.frames recursively
  expect_that(c(udf1,udf3,recursive=TRUE), equals(u(c(dfu1,dfu3,recursive=TRUE),"cats"))) # breaks - (get_units(x) != newunits) fails to produce TRUE/FALSE
  expect_that(c(udf1,udf2,recursive=TRUE), throws_error("All elements must have the same units"))
  expect_that(c(udf1,udf4,recursive=TRUE), throws_error("All elements must have the same units"))
  # joining two internally unitted data.frames non-recursively
  expect_that(c(dfu1,dfu3), equals(lapply(c(df1,df2),u,"cats"))) # breaks - factors not being assigned units in u()?
  expect_that(c.unitted(dfu1,dfu3,recursive=FALSE), throws_error("All elements must be unitted unless recursive==TRUE"))
  # joining two internally unitted data.frames recursively
  expect_that(c.unitted(dfu1,dfu2,recursive=TRUE), throws_error("base elements must all be unitted"))
  expect_that(c.unitted(dfu1,dfu4,recursive=TRUE), throws_error("All elements must have the same units"))
  expect_that(c.unitted(dfu1,dfu3,recursive=TRUE), equals(u(c(df1,df2,recursive=TRUE),"cats"))) # breaks - (get_units(x) != newunits) fails to produce TRUE/FALSE
  
  # matrices and arrays
  mat1 <- matrix(1:10,2)
  mat2 <- matrix(31:40,5)
  umat11 <- u(mat1,"mat1")
  umat21 <- u(mat2,"mat1")
  umat22 <- u(mat2,"mat2")
  expect_that(c(umat11,umat21), equals(u(c(mat1,mat2),"mat1")))
  expect_that(c(umat11,umat22), throws_error("All elements must have the same units"))
  arr1 <- array(1:60,c(4,5,3))
  arr2 <- array(letters[1:60],c(2,6,5))
  uarr11 <- u(arr1,"arr1")
  uarr21 <- u(arr2,"arr1")
  uarr22 <- u(arr2,"arr2")
  expect_that(c(uarr11,uarr21), equals(u(c(arr1,arr2),"arr1")))
  expect_that(c(uarr11,uarr22), throws_error("All elements must have the same units"))
  
  # lists
  expect_that(c.unitted(list(A = 1, c = "C"), d = list(1:3)), throws_error("All elements must be unitted unless recursive==TRUE"))
  expect_that(c.unitted(list(A = u(1,"abc", c = u("C","abc"))), d = list(u(1:3,"abc"))), throws_error("All elements must be unitted unless recursive==TRUE"))
  expect_that(c.unitted(list(A = u(1,"abc", c = u("C","abc"))), d = list(u(1:3,"abc")), recursive=TRUE), equals(c(list(A = 1, c = "C"), d = list(1:3), recursive=TRUE))) # breaks
  expect_that(c.unitted(list(A = u(1,"abc", c = u("C","abc"))), d = list(u(1:3,"DEF")), recursive=TRUE), throws_error("All elements must have the same units"))
  expect_that(c.unitted(list(A = u(1,"abc", c = u("C","DEF"))), d = list(u(1:3,"abc")), recursive=TRUE), throws_error("All elements must have the same units")) # breaks
  
})

#### rbind.unitted ####

test_that("rbind works", {
  # vectors
  vec <- 1:5
  uvec <- u(vec,"snaps")
  expect_that(rbind(uvec,uvec), equals(u(rbind(vec,vec),"snaps"))) # breaks; units not retained
  expect_that(rbind(u(vec,"snaps"),u(vec,"crackles")), throws_error("All elements must have the same units")) # breaks; units not retained or checked
  expect_that(rbind(vec,uvec), throws_error("All elements must have the same units")) # breaks - ignores units
  expect_that(rbind(vec,u(vec,"")), equals(u(rbind(vec,vec),""))) # breaks - new matrix is not unitted
  
  # data.frames
  df <- data.frame(co=1:4,balt=4:7)
  udf <- u(df, c("u1","u2^4"))
  expect_that(rbind(udf,udf), equals(u(rbind(df,df)))) # breaks - check_index throws error
  expect_that(rbind(udf,data.frame(co=5,balt=8)), throws_error("All elements must have the same units")) # breaks - check_index error
  expect_that(rbind(udf,u(data.frame(co=5,balt=8),c("one","u2^4"))), throws_error("All elements must have the same units")) # breaks - check_index error
  
  # matrices and arrays
  mat1 <- matrix(1:10,2)
  mat2 <- matrix(31:40,2)
  umat11 <- u(mat1,"mat1")
  umat21 <- u(mat2,"mat1")
  umat22 <- u(mat2,"mat2")
  expect_that(rbind(umat11,umat21), equals(u(rbind(mat1,mat2),"mat1"))) # breaks - drops units
  expect_that(rbind(umat11,umat22), throws_error("All elements must have the same units")) # breaks - doesn't check units
  arr1 <- array(1:60,c(4,5,3))
  arr2 <- array(letters[1:60],c(2,6,5))
  uarr11 <- u(arr1,"arr1")
  uarr21 <- u(arr2,"arr1")
  uarr22 <- u(arr2,"arr2")
  expect_that(rbind(uarr11,uarr21), equals(u(rbind(arr1,arr2),"arr1"))) # breaks - drops units
  expect_that(rbind(uarr11,uarr22), throws_error("All elements must have the same units")) # breaks - doesn't check units
  
})

#### cbind.unitted ###

test_that("cbind works", {
  # vectors
  vec <- 1:5
  uvec <- u(vec,"snaps")
  expect_that(cbind(uvec,uvec), equals(u(cbind(vec,vec),"snaps"))) # breaks; units not retained
  expect_that(cbind(u(vec,"snaps"),u(vec,"crackles")), throws_error("All elements must have the same units")) # breaks; units not retained or checked
  expect_that(cbind(vec,uvec), throws_error("All elements must have the same units")) # breaks - ignores units
  expect_that(cbind(vec,u(vec,"")), equals(u(cbind(vec,vec),""))) # breaks - new matrix is not unitted
  
  # data.frames
  df <- data.frame(co=1:4,balt=4:7)
  udf <- u(df, c("u1","u2^4"))
  expect_that(u(cbind(udf,udf)), equals(u(cbind(df,df),c("u1","u2^4","u1","u2^4")))) # breaks - drops column names
  expect_that(cbind(udf,data.frame(co=5,balt=8)), equals(u(cbind(data.frame(co=u(1:4,"u1"),balt=u(4:7,"u2^4")),data.frame(co=5,balt=8))))) # breaks - drops column names
  expect_that(cbind(data.frame(co=5,balt=8),udf), equals(cbind(data.frame(co=5,balt=8),data.frame(co=u(1:4,"u1"),balt=u(4:7,"u2^4"))))) # breaks - drops column names
  
  # matrices and arrays
  mat1 <- matrix(1:10,2)
  mat2 <- matrix(31:40,2)
  umat11 <- u(mat1,"mat1")
  umat21 <- u(mat2,"mat1")
  umat22 <- u(mat2,"mat2")
  expect_that(cbind(umat11,umat21), equals(u(cbind(mat1,mat2),"mat1"))) # breaks - drops units
  expect_that(cbind(umat11,umat22), throws_error("All elements must have the same units")) # breaks - doesn't check units
  arr1 <- array(1:60,c(4,5,3))
  arr2 <- array(letters[1:60],c(2,6,5))
  uarr11 <- u(arr1,"arr1")
  uarr21 <- u(arr2,"arr1")
  uarr22 <- u(arr2,"arr2")
  expect_that(cbind(uarr11,uarr21), equals(u(cbind(arr1,arr2),"arr1"))) # breaks - drops units
  expect_that(cbind(uarr11,uarr22), throws_error("All elements must have the same units")) # breaks - doesn't check units
  
})

#### data.frame.unitted ####

test_that("data.frame(unitted vec, mat) works", {
  vec <- 1:5
  mat <- matrix(1:20, 5, 4)
  uvec <- u(vec,"donuts")
  umat <- u(mat,"coffees")
  expect_that(u(data.frame(uvec, umat)), equals(u(data.frame(vec, mat), c("donuts",rep("coffees",4))))) #breaks - drops units from matrix
})
