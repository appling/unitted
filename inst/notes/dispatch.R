
# I would like to define my own behavior (method) for the multiplication of a
# data.frame with an object of a new S3 class. But I can't figure out how to get
# methods dispatch to find my method.

# Define S3 objects 'a' (oldClass "A") and 'df' (oldClass "data.frame")
a <- 4
oldClass(a) <- "A"
df <- data.frame(x=1:2,y=3:4)

# Use trace(Ops.data.frame, edit=TRUE) to add print("Ops.data.frame") at the
# first line. Then we'll know when Ops.data.frame gets called.
a*df
# [1] "Ops.data.frame"
# x  y
# 1 4 12
# 2 8 16

# Define an S3 method for class "A"
Ops.A <- function(e1, e2) {
  print("Ops.A")
  oldClass(e1) <- oldClass(e1)[oldClass(e1) != "A"]
  oldClass(e2) <- oldClass(e2)[oldClass(e2) != "A"]
  callGeneric(e1, e2)
}

# This successfully calls Ops.A
a*a
# [1] "Ops.A"
# [1] 16

# But this throws an error
a*df
# Error in a * df : non-numeric argument to binary operator
# In addition: Warning message:
#   Incompatible methods ("Ops.A", "Ops.data.frame") for "*" 

remove(Ops.A)

# What about using an S4 method instead?
setClass("A", list("numeric")) # Required to define a method for "A"
setGeneric("ATypicalMethod", function(e1, e2) {print("ATypicalMethod - default")})
setMethod("ATypicalMethod", signature=c("A","A"), function(e1, e2) {print("ATypicalMethod - A,A")})
ATypicalMethod(a,a)

setMethod("Ops", signature=c("A","data.frame"), function(e1, e2) {
  print("Ops(A,data.frame)")
  callGeneric(e1@.Data, e2)
})

# Nope - when the scalar is an S3 object, we never find Ops(A,data.frame):
a*df
# [1] "Ops.data.frame"
# x  y
# 1 4 12
# 2 8 16

# This behavior of a*df is explained by Martin Morgan 
# (http://stackoverflow.com/a/12101238/3203184) and ?Methods, who say that if
# the S3 generic is called directly, then the S4 methods will never be found;
# this appears to be happening in a*df because both a and df are S3 objects.

# When the scalar is an S4 object, we get the dispatch I want:
a <- new("A", 4)
a*df
# [1] "Ops(A,data.frame)"
# [1] "Ops.data.frame"
# x  y
# 1 4 12
# 2 8 16

# But I'm really hoping to keep 'a' as an S3 object. Is there a way to both (1)
# let 'a' be S3 and (2) define my own method for Ops('A', 'data.frame')?
