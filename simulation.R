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

# CASE OF INTEREST
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

get.name <- function(file) {
  temp <- read.csv(file)
  return(paste(temp$first_name, temp$last_name))
}

get.studentinfo <- function() {
  temp.prefs <- list.files(pattern="*_preferences.csv")
  s.id <- unlist(lapply(temp.prefs, get.id))
  s.prefs <- lapply(temp.prefs, read.csv, as.is=TRUE)
  
  temp.meta <- list.files(pattern="*_[0-9]")
  s.name <- unlist(lapply(temp.meta, get.name))
  
  s.pref.matrix <- matrix(ncol=length(s.id), nrow=nrow(courses))
  for (i in 1:length(s.prefs)) {
    s.temp <- rep(NA, nrow(courses))
    for (j in 1:nrow(s.prefs[[i]])) {
      s.temp[j] <- get.value(s.prefs[[i]]$Title[j], "course.mapping")
    }
    s.pref.matrix[,i] <- s.temp
  }
  
  colnames(s.pref.matrix) <- s.id
  
  return(s.pref.matrix)
  #temp_mapping <- hash(keys=students, values=myfiles)
}



# -----------------------------------------------------------------------------


