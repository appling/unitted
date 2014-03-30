library(testthat)

library(unitted)
test_package("unitted")

# while in development mode, these calls may be useful:
#test() #calls load_all() internally
#test_file("inst/tests/test-access.R", reporter="tap")
#auto_test_package(".", reporter = "summary")

