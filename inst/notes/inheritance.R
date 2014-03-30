
# Define unitted and subclasses as S4 with the proper inheritance for routing. 
# If unitted is S3, then inheriting objects sometimes do the right thing but 
# don't always know that they're unitted (e.g., is(udf,"unitted")==FALSE). If 
# unitted_data_frame is only S3, it can't inherit from "unitted" or "data.frame"
# without having those classes in its S3 class list. But if unitted and 
# unitted_data_frame are both S4, then setOldClass("unitted_data_frame") creates
# an oldClass that inherits from both "unitted" and "data.frame" for the purpose
# of methods dispatch. If both "unitted" and "data.frame" methods are available,
# it will inherit from the one listed first in the contains argument to
# setClass("unitted_data_frame").
setClass("unitted")
setClass("unitted_data_frame", contains=c("unitted", "data.frame"))
setOldClass("unitted_data_frame", S4Class="unitted_data_frame")
showClass("unitted_data_frame")

# The S3 class assignment
udf <- data.frame(x=3:2, y=9:8)
oldClass(udf) <- c("unitted_data_frame", oldClass(udf))
class(udf)

# S4 properties magically appear
is(udf, "data.frame")
is(udf, "unitted")
is(udf)

# An S4 method with data.frame and unitted methods
setGeneric("f", function(x) standardGeneric("f"))
setMethod("f", signature("data.frame"), function(x) {
  print("data.frame")
  invisible()
})
showMethods("f")
f(udf) 
setMethod("f", signature("unitted"), function(x) {
  print("unitted")
  invisible()
})
showMethods("f")
f(udf)
setMethod("f", signature("unitted_data_frame"), function(x) {
  print("unitted_data_frame")
  callNextMethod()
  invisible()
})
showMethods("f")
f(udf) 


# An S3 method with data.frame and unitted methods - S3 won't dispatch on 'unitted'
g <- function(...)
  UseMethod("g")
g.default <- function(...) {
  print("s3 default")
}
methods(g)
g(udf)
g.unitted <- function(...) {
  print("s3 unitted")
}
methods(g)
g(udf)
g.data.frame <- function(...) {
  print("s3 data.frame")
}
methods(g)
g(udf)
g.unitted_data_frame <- function(...) {
  print("s3 unitted_data_frame")
}
methods(g)
g(udf)



# An S3 method with the same name as the S4 method just overwrites the S4
# generic entirely
showMethods(f)
f <- function(...)
  UseMethod("f")
f.default <- function(...) {
  print("s3 default")
}
showMethods(f)
methods(f)



# An S4 method with the same name as the S3 method just overwrites the S3
# generic entirely
methods(g)
setGeneric("g", function(x) standardGeneric("g"))
setMethod("g", signature("data.frame"), function(x) {
  print("data.frame")
  invisible()
})
showMethods("g")
methods(g)



# They say there are ways to combine S3 and S4 dispatch, though. Try this:
setClass("A", c("vector"))                    # define a class
f3 <- function(x, ...)                        # S3 generic, for S3 dispatch    
  UseMethod("f3")
setGeneric("f3")                              # S4 generic, for S4 dispatch, default is S3 generic
f3.A <- function(x, ...) {print("f3.A")}      # S3 method for S4 class
setMethod("f3", "A", f3.A)
f3.B <- function(x, ...) {print("f3.B")}

showMethods(f3)                               # showMethods knows about "A" only at this point
methods(f3)                                   # methods knows about "A" and "B"

f3(new("A"))                                  # f3 dispatch on A gets f3.A (S4)

b <- 3:5
oldClass(b) <- "B"
f3(b)                                         # f3 dispatch on B gets f3.B despite no call to setMethod("f3", "B")
showMethods(f3)                               # only just now, showMethods has figured out what to do with x="B"

d <- 3:5
oldClass(d) <- "C"
f3(d)                                         # f3 dispatch on c gets nothing, mentions UseMethod (the S3 dispatcher)...

showMethods(f3)                               # showMethods now knows about "B" and "C" as inherited from "ANY", knows "A" directly
methods(f3)                                   # methods still just knows about "A" and "B"



# So that's pretty cool. But read ?Methods, because I think they're saying that
# sometimes the S4 dispatcher still won't be called if the first argument is S3
# and the code is internal. Here's the example they give:

## Need to define an S3 as well as S4 method to use on an S3 object
## or if called from a package without the S4 generic

MathFun <- function(x) { # a smarter "data.frame" method for Math group
  for (i in seq(length = ncol(x))[sapply(x, is.numeric)])
    x[, i] <- (getFunction(.Generic))(x[, i])
  x
}
setMethod("Math", "data.frame", MathFun)

## S4 method works for an S4 class containing data.frame,
## but not for data.frame objects (not S4 objects)

try(logIris <- log(iris)) #gets an error from the old method

## Define an S3 method with the same computation

Math.data.frame <- MathFun

logIris <- log(iris)


# What about unitted_data_frame? Can that be routed by S4 methods?
udf <- iris
oldClass(udf) <- c("unitted_data_frame", oldClass(udf))
class(udf)
MathFun2 <- function(x) { # a smarter "data.frame" method for Math group
  print("MathFun2")
  for (i in seq(length = ncol(x))[sapply(x, is.numeric)])
    x[, i] <- (getFunction(.Generic))(x[, i])
  x
}
setMethod("Math", "unitted_data_frame", MathFun2)
logIris <- log(udf) # no, it's calling MathFun instead of MathFun2
Math.unitted <- MathFun2
logIris <- log(udf) # no, it's STILL calling MathFun instead of MathFun2
Math.unitted_data_frame <- MathFun2
logIris <- log(udf) # hooray!

udf2 <- iris
oldClass(udf2) <- c("unitted", oldClass(udf2))
Math.unitted <- function(x){print("MathFun3")}
logIris <- log(udf2) # hooray!
