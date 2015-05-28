context("write")

test_that("write_unitted and read_unitted work", {
  # define some practice data.frames
  temps <- tempfile(pattern=as.character(1:5))
  practice1 <- u(data.frame(a=1,b=2),c("A","B"))
  practice2 <- u(data.frame(x=1:5, y=9:5), c(x="kg ha^-1 d^-1","mg L^-1"))
  practice3 <- u(
    data.frame(x=1:5, y=9:5, "zeta bomb"=c("hi","mom","dad","and","my fans"), beta=u(ordered(c("o","o","e","i","a"), c("a","e","i","o","u")),"letter^-2")),
    c(x="kg ha^-1 d^-1","mg L^-1",NA,"letters"))
  practice4 <- "not a data.frame"
  practice5 <- data.frame(a=1:7, b="q")
  
  # write_unitted
  expect_output(write_unitted(practice1), 'a\tb\n#"A"\t"B"\n1\t2')
  write_unitted(practice2, file=temps[1]) # simple write; expect no error
  write_unitted(practice2, file=temps[2])
  expect_warning(write_unitted(rbind(practice2,practice2)[6:10,], file=temps[2], append=TRUE), "appending column names to file") # expect warning
  write_unitted(practice2, file=temps[2])
  write_unitted(rbind(practice2,practice2)[6:10,], file=temps[2], append=TRUE, col.names=FALSE) # write with append
  write_unitted(practice2, file=temps[3], row.names=TRUE)
  write_unitted(practice2, file=temps[4], col.names=FALSE, row.names=FALSE) # doesn't make sense to remove col.names w/o removing row.names
  write_unitted(practice2, file=temps[5], comment.char="C")
  expect_error(write_unitted(practice4))
  expect_error(write_unitted(practice5))
  
  # read_unitted
  norownames <- function(x) { rownames(x) <- NULL; x }
  withrownames <- function(x, rnames) { rownames(x) <- rnames; x }
  expect_equal(norownames(practice2), read_unitted(temps[1]))
  expect_equal(v(norownames(practice2)), read_unitted(temps[1], attach.units=FALSE))
  expect_equal(norownames(rbind(practice2,practice2)), read_unitted(temps[2]))
  expect_equal(withrownames(practice2, as.character(1:5)), read_unitted(temps[3]))
  expect_equal(setNames(u(v(practice2)), c("V1","V2")), read_unitted(temps[4], header=FALSE))
  expect_error(read_unitted(temps[5]), "comment.char")
  expect_equal(practice2, read_unitted(temps[5], comment.char="C"))
  
  # cleanup
  file.remove(temps)
})