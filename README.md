unitted
=======

An R package that lets you attach smart, self-updating units to your data

Examples
--------

Attaching units is easy to do, easy to read:
```
> apple7 <- u(7, "apples")   # attaches 'apples' to the number 7
> apple3 <- u(3:5, "apples") # attaches 'apples' to the vector c(3, 4, 5)
```

Arithmetical operations work just like they should:
```
> apple3 + apple7
# unitted numeric (apples)
# [1] 10 11 12
```

and tell you when the units don't make sense.
```
> orange5 <- u(5, "oranges")
> apple7 + orange5
#  Error in require_e2_units(e1) : 
#   Units of e2 are invalid in 'e1 + e2'. Expected 'apples', found 'oranges' 
```

Complex data types can have units, too.
```
> (apple_eating <- u(
+     data.frame(Sally=1:3, Juan=4, Ahmad=3.2, row.names=paste("Day",1:3)), 
+     rep("apples",3)))
#        Sally   Juan  Ahmad
# U     apples apples apples
# Day 1      1      4    3.2
# Day 2      2      4    3.2
# Day 3      3      4    3.2
```

Now your units conversions will be explicit, both to you and to readers of your code.
```
> apple_eating * u(100000/1,"cal apples^-1") * u(1/1000,"kcal cal^-1")
#       Sally Juan Ahmad
# U      kcal kcal  kcal
# Day 1   100  400   320
# Day 2   200  400   320
# Day 3   300  400   320
```

