# install.packages('matchingMarkets')
# Be careful with `rJava` -- make sure that the same version of R and Java are
# installed on the machine (ex. 64 bit R with 64 bit Java) and make sure the path
# variable is set correctly to accommodate for this
library(matchingMarkets)

# -----------------------------------------------------------------------------
# Toy version of college admission (hospital/residents) problem
# Using language relevant to the ULA problem

# CASE OF INTEREST
# 7 students, 2 professors with 3 slots each, given preferences
# Make matrix with student preferences, one col per student,
# one row per class
s.prefs <- matrix(c(1,NA, 1,2, 1,NA, 2,1, 1,2, 1,2, 1,2), 2, 7)
# Make matrix with professor preferences, one col per professor,
# one row per student
c.prefs <- matrix(c(1,2,3,4,5,6,7, 1,2,3,4,5,NA,NA), 7, 2)
h <- hri(s.prefs=s.prefs, c.prefs=c.prefs, nSlots=c(3,3))
plot(h, energy=TRUE)

# -----------------------------------------------------------------------------
# ULA problem data structures

# dim(s.prefs) <- nrow=nclasses (with interest), ncol=nstudents
# dim(c.prefs) <- nrow=nstudents, ncol=nclasses (with interest)
# always need to pad with NA's to ensure proper matrix dims
# need to map students and classes to numbers

# install.packages("hash")
library(hash)

# Maria defined functions

get.value <- function(key, hash_table) {
  return(eval(parse(text=hash_table))[[key]])
}

get.id <- function(string) {
  return(substring(string, 1, regexpr("_", string) - 1))
}

get.name <- function(file) {
  temp <- read.csv(file, as.is=TRUE)
  return(paste(temp$first_name, temp$last_name))
}

get.grade <- function(s.num, course) {
  temp <- s.prefs[[s.num]]
  x <- temp$Grade[which(unlist(lapply(temp$Title, get.value, hash_table="course.mapping")) == course)]
  ifelse (x == "", return("Not taken"), return(x))
}

get.year <- function(file) {
  temp <- read.csv(file, as.is=TRUE)
  return(temp$year)
}

# NB: `course` needs to be the course number from the hash table; returns
# negative response length (in characters) because the sorts are done with the 
# order decreasing
get.response <- function(s.num, course) {
  temp <- s.prefs[[s.num]]
  return(-nchar(temp$Suitable[which(unlist(lapply(temp$Title, get.value, hash_table="course.mapping")) == course)]))
}

get.sorted <- function(mat, course) {
  mat$grade <- NA
  mat$year <- NA
  mat$response <- NA
  for (i in 1:nrow(mat)) {
    if (!is.na(mat[i,2])) {
      mat$grade[i] <- get.value(toString(get.grade(mat[i,1], course)), hash_table="grade.mapping")
      mat$year[i] <- get.year(meta[i])
      mat$response[i] <- get.response(mat[i,1], course=course)
    }
  }
  mat <- mat[order(mat$grade, mat$year, mat$response, decreasing=TRUE),]
  return(mat)
}

"%!in%" <- Negate("%in%")

# Reading in files, doing manipulations, creating hash table mappings

# Courses being offered for a given semester
courses <- read.csv("courses.csv", as.is=TRUE)
courses$interest <- 0

# Get student preferences
temp.prefs <- list.files(pattern="*_preferences.csv")
s.id <- unlist(lapply(temp.prefs, get.id))
s.prefs <- lapply(temp.prefs, read.csv, as.is=TRUE)

# Get course interest
for (i in 1:length(s.prefs)) {
  for (j in 1:nrow(s.prefs[[i]])) {
    if (any(courses$course == s.prefs[[i]][j,1])) {
      temp <- which(courses$course == s.prefs[[i]]$Title[j])
      courses$interest[temp] <- courses$interest[temp] + 1
    }
  }
}

courses.interest <- courses[courses$interest != 0,]
courses.nointerest <- courses[courses$interest == 0,]

# Get student meta data, in particular, first and last name
meta <- list.files(pattern="*_[0-9]")
s.name <- unlist(lapply(meta, get.name))
s.year <- unlist(lapply(meta, get.year))

# Create hash table mapping student name to student number
# This number is based on the order in which the student files are read in
student.mapping <- hash(keys=s.name, values=seq(1, length(s.id)))
student.mapping.inverted <- invert(student.mapping)

# Get professor preferences
temp.profs <- list.files(pattern="[A-Z]{2}[0-9]{4}")
p.info <- unname(unlist(lapply(temp.profs, read.table, 
                               stringsAsFactors=FALSE, header=FALSE)))

# Create hash table mapping course name to course number
# This number is based on the order in which the professor files are read in
course.mapping <- hash(keys=courses.interest$course, values=seq(1, nrow(courses.interest)))
course.mapping.inverted <- invert(course.mapping)

# Create hash table mapping grade options to point values
g <- c("A", "A-", "B+", "Not taken", "B", "B-", "C+", "C, C-, D+", "D, D-, F", "CR, P")
grade.mapping <- hash(keys=g, values=seq(1, length(g)))

# Create matrix of student preferences
s.pref.matrix <- matrix(ncol=length(s.id), nrow=nrow(courses.interest))
for (i in 1:length(s.prefs)) {
  s.temp <- rep(NA, nrow(courses.interest))
  for (j in 1:nrow(s.prefs[[i]])) {
    s.temp[j] <- get.value(s.prefs[[i]]$Title[j], "course.mapping")
  }
  s.pref.matrix[,i] <- s.temp
}

# Create matrix of professor preferences, collect number of slots per class
if (nrow(courses.nointerest) > 0) {
  
  ula.notinterested <- as.data.frame(cbind(courses.nointerest$course, NA, 0), 
                                     stringsAsFactors=FALSE)
  names(ula.notinterested) <- c("course", "desired", "assigned")
  ula.notinterested$desired <- as.numeric(ula.notinterested$desired)
  
}

ula.interested <- rep(NA, nrow(courses.interest))
p.pref.matrix <- matrix(ncol=nrow(courses.interest), nrow=length(s.id))

for (i in 1:length(p.info)) {
  
  info <- eval(parse(text=p.info[i]))
  
  if (info[[1]] %in% courses.interest$course) {
    ula.interested[get.value(info[[1]], "course.mapping")] <- info[[2]]
    
    temp <- rep(NA, length(s.id))
    for (j in 1:length(info[[3]])) {
      ifelse(is.null(info[[3]][j]), 
             temp[j] <- NA, temp[j] <- get.value(info[[3]][j], "student.mapping"))
    }
    
    p.pref.matrix[,get.value(info[[1]], "course.mapping")] <- temp
    
  } else {
    ula.notinterested$desired[ula.notinterested$course == info[[1]]] <- info[2]
  }
  
}

# Deal with case where professor has not submitted preferences, but students
# have ranked the class
empty.cols <- which(apply(p.pref.matrix, 2, sum, na.rm=TRUE) == 0)
for (i in empty.cols) {
  temp.prefs <- as.data.frame(matrix(nrow=ncol(s.pref.matrix), ncol=2))
  for (j in 1:nrow(temp.prefs)) {
    if (any(s.pref.matrix[,j] == i, na.rm=TRUE)) {
      temp.prefs[j,] <- c(j, which(s.pref.matrix[,j] == i))
    } else {
      temp.prefs[j,] <- c(j, NA)
    }
    temp.prefs <- temp.prefs[order(temp.prefs[,2], decreasing=FALSE),]
    if (sum(is.na(temp.prefs[,2])) + length(unique(temp.prefs[,2][!is.na(temp.prefs[,2])])) < nrow(temp.prefs)) {
      temp.prefs <- get.sorted(temp.prefs, i)[,1:2]
    }
    temp.prefs$V1[is.na(temp.prefs$V2)] <- NA
  }  
  p.pref.matrix[,i] <- temp.prefs[,1]
}

# Original handling of no student or faculty interest -- randomly assign a 
# student to a class. Legacy code to be deleted, just check that nothing gets
# messed up if it is
# empty.cols <- which(apply(p.pref.matrix, 2, sum, na.rm=TRUE) == 0)
# for (i in empty.cols) {
#  p.pref.matrix[,i] <- sample(1:ncol(s.pref.matrix), replace=FALSE)
# }

# -----------------------------------------------------------------------------

# A working matching!
m <- hri(s.prefs=s.pref.matrix, c.prefs=p.pref.matrix, nSlots=ula.interested)
# CURRENTLY BROKEN
# NEED TO TEST TO SEE UNDER WHAT CONDITIONS THE MATCHING ALGORITHM DOES NOT CONVERGE

# -----------------------------------------------------------------------------

# Extract information from matching regarding student assignment and generate
# a list of unmatched students and their potential interest
assignments <- as.data.frame(cbind(m$matchings$college, m$matchings$student))
names(assignments) <- c("course", "student")

for (i in 1:nrow(assignments)) {
  assignments$course[i] <- get.value(toString(assignments$course[i]), 
                                     "course.mapping.inverted")
  assignments$student[i] <- get.value(toString(assignments$student[i]), 
                                      "student.mapping.inverted")
}

# Find unassigned students and get a list of their course preferences in order
unassigned <- as.data.frame(keys(student.mapping)[which(keys(student.mapping) %!in% assignments$student)], 
                            stringsAsFactors=FALSE)
names(unassigned) <- c("student")
unassigned$prefs <- NA
for (i in 1:nrow(unassigned)) {
  temp.mat <- s.prefs[[get.value(unassigned$student[i], "student.mapping")]]
  unassigned$prefs[i] <- toString(list(temp.mat$Title[order(temp.mat$Rank)]))
}

# Extract information regarding ULA counts per class and bind with information
# from classes that have not been assigned any ULAs
ula.demand <- as.data.frame(cbind(seq(1:nrow(courses.interest)), ula.interested), 
                            stringsAsFactors=FALSE)
names(ula.demand) <- c("course", "desired")
for (i in 1:nrow(ula.demand)) {
  ula.demand$course[i] <- get.value(toString(ula.demand$course[i]), 
                                    "course.mapping.inverted")
}

temp.assignments <- as.data.frame(table(assignments$course), 
                                  stringsAsFactors=FALSE)
names(temp.assignments) <- c("course", "assigned")


ula.demand <- merge(ula.demand, temp.assignments, by="course")

if (exists("ula.notinterested")) {
  ula.demand <- rbind(ula.demand, ula.notinterested)
  ula.notinterested$desired <- as.numeric(ula.notinterested$desired)
  ula.notinterested$assigned <- as.numeric(ula.notinterested$assigned) 
}

ula.demand$needed <- ula.demand$desired - ula.demand$assigned

# For this script to run fully right now, you need to make up an unassigned
# student because there are currently no students that are unassigned

# Write csvs with course assignment numbers, assigned student information, and
# unassigned student information
write.csv(assignments, "Assignments.csv", row.names=FALSE)
write.csv(unassigned, "Unassigned-Students.csv", row.names=FALSE)
write.csv(ula.demand, "Demand.csv", row.names=FALSE)
