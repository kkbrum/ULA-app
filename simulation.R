# install.packages('matchingMarkets')
# Be careful with `rJava` -- make sure that the same version of R and Java are
# installed on the machine (ex. 64 bit R with 64 bit Java) and make sure the path
# variable is set correctly to accommodate for this
library(matchingMarkets)

# -----------------------------------------------------------------------------
# Toy version of college admission (hospital/residents) problem
# Using language relevant to the ULA problem

# 7 students, 2 professors with 3 slots each, random preferences
m1 <- hri(nStudents=7, nSlots=c(3, 3), seed=64)

# 7 students, 2 professors with 3 slots each, given preferences
# Make matrix with student preferences, one col per student,
# one row per class
s.prefs <- matrix(c(1,NA, 1,2, 1,NA, 2,1, 1,2, 1,2, 1,2), 2, 7)
# Make matrix with professor preferences, one col per professor,
# one row per student
c.prefs <- matrix(c(1,2,3,4,5,6,7, 1,2,3,4,5,NA,NA), 7, 2)
m2 <- hri(s.prefs=s.prefs, c.prefs=c.prefs, nSlots=c(3,3))
plot(m2, energy=TRUE)

# 100 students, 100 colleges with 1 slot each
res <- hri(nStudents=200, nColleges=200, seed=64)
plot(res)
plot(res, energy=TRUE)

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# ULA problem data structures

# dim(s.prefs) <- nrow=nclasses, ncol=nstudents
# dim(c.prefs) <- nrow=nstudents, ncol=nclasses
# always need to pad with NA's to ensure proper matrix dims
# need to map students and classes to numbers

# -----------------------------------------------------------------------------

