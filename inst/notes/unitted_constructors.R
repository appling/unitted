
#' What are the likely classes for which a unitted implementation would be
#' wanted?
basicClasses <- unlist(getAnywhere(".BasicClasses")[1:34])
specialClasses <- .OldClassesList

#' How should the unitted classes be structured among themselves?
#' 
#' Should "unitted" contain a units slot or not? setClass("unitted", slots=c(units="ANY"))
#' 
#' see ?setIs, ?setClassUnion. I think "unitted" should be a virtual classUnion
#' of its subclasses. There's apparently a difference between defining each
#' subclass to contain "unitted" and defining "unitted" to be a superclass of
#' the subclasses, but I'm not sure how important it is.
setClass("unitted_numeric", contains=c("numeric"))
setClass("unitted_logical", contains=c("logical"))
setClass("unitted_integer", contains=c("integer"))
setClass("unitted_complex", contains=c("complex"))
setClass("unitted_raw", contains=c("raw"))
setClass("unitted_character", contains=c("character"))
setClass("unitted_list", contains=c("list"))
setClassUnion("unitted", c("unitted_numeric","unitted_logical","unitted_integer","unitted_complex","unitted_raw","unitted_character","unitted_list"))
#' or else:
setClass("unitted")
setClass("unitted_numeric", contains=c("numeric","unitted"))
setClass("unitted_logical", contains=c("logical","unitted"))
setClass("unitted_integer", contains=c("integer","unitted"))
setClass("unitted_complex", contains=c("complex","unitted"))
setClass("unitted_raw", contains=c("raw","unitted"))
setClass("unitted_character", contains=c("character","unitted"))
setClass("unitted_list", contains=c("list","unitted"))
getClass("unitted_list")

#' Is it possible to define a single unitted class, or a small handful, that can
#' behave like many of S3 the classes?
#' 
#' No luck so far. Tried class unions
setClassUnion("basic", c("numeric","logical","integer","character","raw","complex"))
setClass("unitted_basic", representation("basic", id="character"))
ub <- new("unitted_basic", 7, id="hi")
str(ub)
S3Part(ub, strictS3=TRUE) #fails here because 'basic' is not basic

#' Is it possible to automate the creation of all those unitted subclasses?
#' 
#' Should be. It's all text.
setClass("unitted", slots=c(units="ANY"))
for(bc in c("logical","numeric","character","complex","integer","raw","expression","list","data.frame")) {
  setClass(paste0("unitted_",bc), contains=c(bc,"unitted"))
}

#' How can I see which unitted subclasses are defined?
names(getClass("unitted")@subclasses)

#' Is there any reason to use setOldClass("unitted")?
#' 
#' No. Now all my unitted classes are S4 and staying that way.
#' setOldClass(c("unitted"), S4Class="unitted")

#' Is it possible or necessary to let the user define additional unitted S3 
#' classes?
#' 
#' Should be. Maybe I should provide a function.
setClass("unitted", slots=c(units="ANY"))
new_unitted_class <- function(data.type, overwrite=FALSE) {
  class_def <- getClassDef(data.type)
  if(is.null(class_def)) {
    stop(paste0(data.type," must be registered with setOldClass or defined with setClass before calling new_unitted_class()"))
  }
  new_name <- paste0("unitted_",data.type)
  if(new_name %in% names(getClass("unitted")@subclasses)) {
    if(overwrite)
      warning(paste(new_name,"was already a registered unitted class; calling setClass again anyway"))
    else
      stop(paste(new_name,"is already a registered unitted class; set overwrite=TRUE to proceed anyway"))
  }
  setClass(new_name, contains=c(data.type,"unitted"))
}

# Throws error and/or warning, as it should:
new_unitted_class("B") 
new_unitted_class("data.frame") 
new_unitted_class("data.frame", overwrite=TRUE) 

# Try to construct unitted class for an S3 structure using setOldClass alone - fails to create S3 data part
setOldClass("A")
new_unitted_class("A", overwrite=TRUE)  
a <- new("unitted_A", units="apples")
S3Part(a, strictS3=TRUE) # returns an S4 object of class A

# Try to wrap an entire S3 structure in an S4 slot - OK, but resulting objects don't behave much like S3 structures
setClass("S3structure", slots=c(data="ANY"))
new_unitted_class("S3structure", overwrite=TRUE)  
a <- new("unitted_S3structure", 
         data={adata <- data.frame(x=3,y=2); class(adata) <- c("round", class(adata)); adata},
         units="apples") # works
print(a) # gets printed like the s4 object it is
print.unitted <- function(x) { print("unitted"); print(a@data) }
print(a) # method for 'unitted' gets discovered and used
print.round <- function(x) {print("round and round and round"); NextMethod()}
print(a) # the data slot is printed according to its class
a # but S4 objects on the command line are displayed with show(), not print()
summary.round <- function(x) {print("summary(round)")}
summary(a) # and any S3 methods for which a 'unitted' method is undefined will still get called on the S4 wrapper rather than its S3 contents
a@data # on the bright side, it's easy to recover the S3 part...but I'm afraid this isn't sufficient to compensate for the other problems

# Try to construct unitted class for an S3 basic type, define class later - S4ness is destroyed
setClass("A", contains="data.frame")
new_unitted_class("A", overwrite=TRUE)  
a <- new("unitted_A", data.frame(s=1:4), units="apples") # can be constructed with empty data.frame according to default prototype
oldClass(a) <- c("A", oldClass(a)) # makes it S3 again, stripping "unitted"

# Try to construct unitted class for an S3 basic type with special attributes, defined before or after - attributes stay, no problem
a <- new("unitted_A", data.frame(s=1:4), units="apples")
attr(a,"corn") <- "husk"
attributes(a) # yes, visible to S4
a@corn # it added it as a slot!
attr(a, "corn")
attr(a, ".S3Class") # other slots can be accessed with attr, too
attributes(S3Part(a, strictS3=TRUE)) # still yes (keeps attributes)

# Try to construct unitted class for an S3 structure by making the structure S4 - most successful yet
setClass("A", contains="data.frame")
new_unitted_class("A", overwrite=TRUE)  
getClass("unitted_A") # it knows it's a data.frame, pretty much
a <- new("unitted_A", units="apples") # can be constructed with empty data.frame according to default prototype
a <- new("unitted_A", data.frame(s=4:10), units="apples") # can be constructed with empty data.frame according to default prototype
expect_that(summary(data.frame(s=4:10)), equals(summary(a))) # data.frame methods work
print.A <- function(x) print("I'm an A") # A methods work (and override data.frame methods)
print(a)
getDataPart(a) # not nearly as useful as S3Part, next line
str(S3Part(a, strictS3=TRUE)) # S3Part is entirely data.frame; loses A class because it A is an S4 class
# so far as I can tell, then, you really can't wrap an entire S3 object in an S4 unitted
# wrapper and expect it to be dispatched according to its S3 traits; if you want
# dispatch, you need to make a new S4 class.
