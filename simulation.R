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

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# ULA problem data structures

# dim(s.prefs) <- nrow=nclasses, ncol=nstudents
# dim(c.prefs) <- nrow=nstudents, ncol=nclasses
# always need to pad with NA's to ensure proper matrix dims
# need to map students and classes to numbers

# install.packages("hash")
library(hash, quietly=TRUE)
courses <- read.csv("courses.csv", as.is=TRUE)
course.mapping <- hash(keys=courses$course, values=seq(1, nrow(courses)))

get.value <- function(key, hash_table) {
  return(eval(parse(text=hash_table))[[key]])
}

get.id <- function(string) {
  return(substring(string, 1, regexpr("_", string) - 1))
}

get.name <- function(first, last) {
  
}

get.studentinfo <- function() {
  temp <- list.files(pattern="*_preferences.csv")
  s.id <- unlist(lapply(temp, get.id))
  myfiles <- lapply(temp, read.csv)
  
  temp <- list.files(pattern="*_[0-9]")
  
  #temp_mapping <- hash(keys=students, values=myfiles)
}



# -----------------------------------------------------------------------------


