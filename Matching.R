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

# Maria defined functions

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

# Reading in files, doing manipulations, creating hash table mappings

# Courses being offered for a given semester
courses <- read.csv("courses.csv", as.is=TRUE)

# Get student preferences
temp.prefs <- list.files(pattern="*_preferences.csv")
s.id <- unlist(lapply(temp.prefs, get.id))
s.prefs <- lapply(temp.prefs, read.csv, as.is=TRUE)

# Get student meta data, in particular, first and last name
temp.meta <- list.files(pattern="*_[0-9]")
s.name <- unlist(lapply(temp.meta, get.name))

# Create hash table mapping student name to student number
# This number is based on the order in which the student files are read in
student.mapping <- hash(keys=s.name, values=seq(1, length(s.id)))

# Get professor preferences
temp.profs <- list.files(pattern="[A-Z]{2}[0-9]{4}")
p.info <- unname(unlist(lapply(temp.profs, read.table, 
                               stringsAsFactors=FALSE, header=FALSE)))

# Create hash table mapping course name to course number
# This number is based on the order in which the professor files are read in
course.mapping <- hash(keys=courses$course, values=seq(1, nrow(courses)))

# Create matrix of student preferences
s.pref.matrix <- matrix(ncol=length(s.id), nrow=nrow(courses))
for (i in 1:length(s.prefs)) {
  s.temp <- rep(NA, nrow(courses))
  for (j in 1:nrow(s.prefs[[i]])) {
    s.temp[j] <- get.value(s.prefs[[i]]$Title[j], "course.mapping")
  }
  s.pref.matrix[,i] <- s.temp
}

# Create matrix of professor preferences, collect number of slots per class
num.ula <- rep(NA, nrow(courses))
p.pref.matrix <- matrix(ncol=nrow(courses), nrow=length(s.id))
for (i in 1:length(p.info)) {
  info <- eval(parse(text=p.info[i]))
  num.ula[get.value(info[[1]], "course.mapping")] <- info[[2]]
  
  temp <- rep(NA, length(s.id))
  for (j in 1:length(info[[3]])) {
    temp[j] <- get.value(info[[3]][j], "student.mapping")
  }
  p.pref.matrix[,get.value(info[[1]], "course.mapping")] <- temp
}

# For the below simulation to work: remove S&DS 100, S&DS 230 from courses
# and remove all references to those classes from student preferences
# Need to do this because there's currently no mechanism to deal with
# professors who don't input their preferences.
# hri(s.prefs=s.pref.matrix, c.prefs=p.pref.matrix, nSlots=num.ula)

# 30 May 2018
# Things to work out:
# Setting up default number of ULAs, should be in prof files and loaded upon login
# Ordering of preferences if professor doesn't submit rankings
# What to do if class not done by professor or students

empty.cols <- which(apply(p.pref.matrix, 2, sum, na.rm=TRUE) == 0)

# logic:
# loop through each class
# check s.pref.matrix to see if students have class ranked
# if yes, save a tuple with the student number and their pref
# after looking through whole matrix, look through temp list
# to determine how to order the students: first by pref, any ties broken by
# grade, senority, num words 

for (i in empty.cols) {
  temp.prefs <- list(rep(NA, ncol(s.pref.matrix)))
  for (j in ncol(s.pref.matrix)) {
    if (any(s.pref.matrix[,j]) == i) {}
    temp.prefs[[j]] <- unlist(list(j, which(s.pref.matrix[,j] == i)))
  }
}


# -----------------------------------------------------------------------------


