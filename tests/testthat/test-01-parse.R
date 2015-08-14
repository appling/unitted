context("parse")
knownbug <- function(expr, notes) invisible(NULL)

#### unitted:::parse_units ####

test_that("unitted:::parse_units works", {
  
  # well-formed unit strings using only the space delimiter
  expect_that(unitted:::parse_units("kg ha^-1 yr^-1"), equals(list(data.frame(Unit=c("kg","ha","yr"),Power=c(1,-1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("apples^-1 oranges^2.5"), equals(list(data.frame(Unit=c("apples","oranges"),Power=c(-1,2.5),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("mg dm^-1 dm^-2 ug mg^-1"), equals(list(data.frame(Unit=c("mg","dm","dm","ug","mg"),Power=c(1,-1,-2,1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("kg_C^1 ha^-1"), equals(list(data.frame(Unit=c("kg_C","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  
  # empty unit strings - NA and "" are treated the same
  expect_that(unitted:::parse_units(""), equals(list(data.frame(Unit=character(),Power=numeric(),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units(NA), equals(list(data.frame(Unit=character(),Power=numeric(),stringsAsFactors=FALSE))))
  
  # vectors of unit strings
  expect_that(unitted:::parse_units(c("","mg dm^-1 dm^-2 ug mg^-1",NA)), equals(list(
    data.frame(Unit=character(),Power=numeric(),stringsAsFactors=FALSE),
    data.frame(Unit=c("mg","dm","dm","ug","mg"),Power=c(1,-1,-2,1,-1),stringsAsFactors=FALSE),
    data.frame(Unit=character(),Power=numeric(),stringsAsFactors=FALSE))))
  
  # delimited unit strings
  expect_that(unitted:::parse_units("|kg C|^1 ha^-1"), equals(list(data.frame(Unit=c("kg C","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("|kg C|^1 ha^-1",delimiter="|"), equals(list(data.frame(Unit=c("kg C","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("|kg C|^1 ha^-1",delimiter="?"), equals(list(data.frame(Unit=c("|kg","C|","ha"),Power=c(1,1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("?kg C?^1 ha^-1",delimiter="?"), equals(list(data.frame(Unit=c("kg C","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("$kg C$^1 ha^-1",delimiter="$"), equals(list(data.frame(Unit=c("kg C","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  
  # unit strings with tabs, newlines, special characters, and delimiters
  expect_that(unitted:::parse_units("  mg         dm^-1 dm^-2   ug    mg^-1   "), equals(list(data.frame(Unit=c("mg","dm","dm","ug","mg"),Power=c(1,-1,-2,1,-1),stringsAsFactors=FALSE))), info="extra spaces")
  expect_that(unitted:::parse_units("mg \t dug\nmg^-1   \t\n\t"), equals(list(data.frame(Unit=c("mg","dug","mg"),Power=c(1,1,-1),stringsAsFactors=FALSE))), info="space and tab")
  expect_that(unitted:::parse_units("|kg \t\nC  |^1 ha^-1",delimiter="|"), equals(list(data.frame(Unit=c("kg \t\nC  ","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("|kg ?_+.({[*C  |^1 ha^-1",delimiter="|"), equals(list(data.frame(Unit=c("kg ?_+.({[*C  ","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("|kg ?_+.({[*\\C  |^1 ha^-1",delimiter="|"), equals(list(data.frame(Unit=c("kg ?_+.({[*\\C  ","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("Xkg ?_+.({[*\\C  X^1 ha^-1",delimiter="X"), equals(list(data.frame(Unit=c("kg ?_+.({[*\\C  ","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  
  # extra scary unit strings - ^ and \\ were problems at one point
  expect_that(unitted:::parse_units("|kg ?_+^.({[*\\C  |^1 ha^-1",delimiter="|"), equals(list(data.frame(Unit=c("kg ?_+^.({[*\\C  ","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("Xkg ?_+^.({[*\\C  X^1 ha^-1",delimiter="X"), equals(list(data.frame(Unit=c("kg ?_+^.({[*\\C  ","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("|kg ?_+^.({[*C  |^1 ha^-1",delimiter="|"), equals(list(data.frame(Unit=c("kg ?_+^.({[*C  ","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("Xkg ?_+^.({[*C  X^1 ha^-1",delimiter="X"), equals(list(data.frame(Unit=c("kg ?_+^.({[*C  ","ha"),Power=c(1,-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("|kg C^1 ha|^-1",delimiter="|"), equals(list(data.frame(Unit=c("kg C^1 ha"),Power=c(-1),stringsAsFactors=FALSE))))
  expect_that(unitted:::parse_units("Xkg C^1 haX^-1",delimiter="X"), equals(list(data.frame(Unit=c("kg C^1 ha"),Power=c(-1),stringsAsFactors=FALSE))))
  
  # still problematic
  knownbug(expect_error(unitted:::parse_units("^and")), "breaks - doesn't notice")
  knownbug(expect_error(unitted:::parse_units("or^and")), "'or^and' shouldn't be parsed into 'or'")
  
  # badly formed numeric substrings
  knownbug(expect_that(unitted:::parse_units("m^-1/2"), throws_error()), "breaks; bad parse not caught")
  expect_that(unitted:::parse_units("m^-(1/2)"), throws_error("Invalid number"))
  knownbug(expect_that(unitted:::parse_units("m^(-1/2)"), throws_error("Invalid number")), "breaks; bad parse not caught")
  
})


#### unitted:::merge_units ####

test_that("unitted:::merge_units works", {
  
  # neither unitted:::parse_units nor unitted:::merge_units does any reordering, so we should get
  # out what we put in, with possible exceptions for 1. number formatting and 2.
  # delimiter removal
  
  # straight translation:
  expect_that(unitted:::merge_units(unitted:::parse_units("kg ha^-1 yr^-1")), equals("kg ha^-1 yr^-1"))
  expect_that(unitted:::merge_units(unitted:::parse_units("apples^-1 oranges^2.5")), equals("apples^-1 oranges^2.5"))
  expect_that(unitted:::merge_units(unitted:::parse_units("mg dm^-1 dm^-2 ug mg^-1")), equals("mg dm^-1 dm^-2 ug mg^-1"))

  # number formatting exception - dropping ^1
  expect_that(unitted:::merge_units(unitted:::parse_units("kg_C^1 ha^-1")), equals("kg_C ha^-1"))
  expect_that(unitted:::merge_units(unitted:::parse_units("kg_C^1.0000000000000001 ha^-.99999999999999999")), equals("kg_C ha^-1"))
  expect_that(unitted:::merge_units(unitted:::parse_units("kg_C^1.1 ha^-.99")), equals("kg_C^1.1 ha^-0.99"))
  
  # delimiter removal exception - delimiters get dropped in parsing
  expect_that(unitted:::merge_units(unitted:::parse_units("|kg_C|^1 ha^-1")), equals("kg_C ha^-1"))
  expect_that(unitted:::merge_units(unitted:::parse_units("|kg SO_4^2-|^1 ha^-1"), rule="never"), equals("kg SO_4^2- ha^-1"))
  
  # empty unit strings - NA and "" are treated the same
  expect_that(unitted:::merge_units(unitted:::parse_units("")), equals(""))
  expect_that(unitted:::merge_units(unitted:::parse_units(NA)), equals(""))
  
  # vectors of unit strings
  expect_that(unitted:::merge_units(unitted:::parse_units(c("","mg dm^-1 dm^-2 ug mg^-1",NA))), equals(c("","mg dm^-1 dm^-2 ug mg^-1","")))
  
  # potentially ambiguous strings wtih delimiters
  expect_equal(unitted:::parse_units(c("|hi there| Wei","|or^1 and|", "Joe")), unitted:::parse_units(unitted:::merge_units(unitted:::parse_units(c("|hi there| Wei","|or^1 and|", "Joe")), "|", "disambiguate")))
  expect_equal(unitted:::parse_units(c("|hi there| Wei","|or^1 and|", "Joe")), unitted:::parse_units(unitted:::merge_units(unitted:::parse_units(c("|hi there| Wei","|or^1 and|", "Joe")), "|", "always")))
  expect_equal(unitted:::parse_units(c("hi there Wei","or and", "Joe")), unitted:::parse_units(unitted:::merge_units(unitted:::parse_units(c("|hi there| Wei","|or^1 and|", "Joe")), "|", "never")))
  expect_equal(unitted:::merge_units(unitted:::parse_units(c("|hi there| Wei","|or^1 and|", "Joe")),"*"), c("*hi there* Wei", "*or^1 and*", "Joe"))
})


#### unitted:::simplify_units ####

test_that("unitted:::simplify_units works", {
  
  # since we've just tested unitted:::parse_units and unitted:::merge_units, let's use them here as
  # if they're [probably] functioning correctly.
  expect_that(unitted:::merge_units(unitted:::simplify_units(unitted:::parse_units("kg ha^-1 yr^-1"))), equals("kg ha^-1 yr^-1"))
  expect_that(unitted:::merge_units(unitted:::simplify_units(unitted:::parse_units("kg kg ha^-1 kg^-2 yr^-1"))), equals("ha^-1 yr^-1"))
  expect_that(unitted:::merge_units(unitted:::simplify_units(unitted:::parse_units("kg kg ha^-1 kg^-2 yr^-1"))), equals("ha^-1 yr^-1"))
  expect_that(unitted:::merge_units(unitted:::simplify_units(unitted:::parse_units("ha^-1 ha^2.1 kg kg ha^-1 kg^-2 yr^-1"))), equals("ha^0.1 yr^-1"))
  
  # empty unit strings - NA and "" are treated the same
  expect_that(unitted:::merge_units(unitted:::simplify_units(unitted:::parse_units(""))), equals(""))
  expect_that(unitted:::merge_units(unitted:::simplify_units(unitted:::parse_units(NA))), equals(""))
  
  # vectors of unit strings
  expect_that(unitted:::merge_units(unitted:::simplify_units(unitted:::parse_units(c("uni^3 tt^2 ed^1", "", NA, "hex^2 a^3 gon^4")))), equals(c("uni^3 tt^2 ed", "", "", "hex^2 a^3 gon^4")))
  
})


#### unitted:::sort_units ####

test_that("unitted:::sort_units works", {
  # since we've just tested unitted:::parse_units and unitted:::merge_units, let's use them here as
  # if they're [probably] functioning correctly.
  expect_that(unitted:::merge_units(unitted:::sort_units(unitted:::parse_units("kg ha^-1 yr^-1"))), equals("kg ha^-1 yr^-1"))
  expect_that(unitted:::merge_units(unitted:::sort_units(unitted:::parse_units("kg ha^-1 kg^-2 yr^-1  kg"))), equals("kg kg ha^-1 kg^-2 yr^-1"))
  
  # gives units with positive powers first, sorted alphabetically, and then units with negative powers, also sorted alphabetically
  expect_that(unitted:::merge_units(unitted:::sort_units(unitted:::parse_units("a^-1 b^-2 c^-1 d^9 e^1 f^3"))), equals("d^9 e f^3 a^-1 b^-2 c^-1"))
  expect_that(unitted:::merge_units(unitted:::sort_units(unitted:::parse_units("b^-1 c^-2 a^-1 f^9 e^1 d^3"))), equals("d^3 e f^9 a^-1 b^-1 c^-2"))
  
  # empty unit strings - NA and "" are treated the same
  expect_that(unitted:::merge_units(unitted:::sort_units(unitted:::parse_units(""))), equals(""))
  expect_that(unitted:::merge_units(unitted:::sort_units(unitted:::parse_units(NA))), equals(""))
  
  # vectors of unit strings
  expect_that(unitted:::merge_units(unitted:::sort_units(unitted:::parse_units(c("uni^3 tt^2 ed^1", "", NA, "hex^2 a^3 gon^4")))), equals(c("ed tt^2 uni^3", "", "", "a^3 gon^4 hex^2")))
  
})
