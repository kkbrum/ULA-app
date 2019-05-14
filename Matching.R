# ==============================================================================
# ULA problem data structures

# dim(s.prefs) <- nrow=nclasses (with interest), ncol=nstudents
# dim(p.prefs) <- nrow=nstudents, ncol=nclasses (with interest)
# always need to pad with NA's to ensure proper matrix dims
# ==============================================================================


# Functions


get.id <- function(string) {
  return(substring(string, 1, regexpr("_", string) - 1))
}


get.name <- function(file) {
  temp <- read.csv(file, as.is=TRUE)
  return(paste(temp$first_name, temp$last_name))
}


get.year <- function(file) {
  temp <- read.csv(file, as.is=TRUE)
  return(temp$year)
}


get.pin <- function(string) {
  return(substring(string, regexpr("_", string) + 1, nchar(string) - 4))
}

# Courses being offered for a given semester, input manually
courses <- read.csv("courses.csv", as.is=TRUE)

temp.s.prefs <- list.files(pattern="*_preferences.csv")
s.id <- unlist(lapply(temp.s.prefs, get.id))
s.prefs <- lapply(temp.s.prefs, read.csv, as.is=TRUE)

# Get course interest
courses$interest <- 0
for (i in 1:length(s.prefs)) {
  for (j in 1:nrow(s.prefs[[i]])) {
    if (any(courses$course == s.prefs[[i]][j,1])) {
      temp <- which(courses$course == s.prefs[[i]]$Title[j])
      courses$interest[temp] <- courses$interest[temp] + 1
    }
  }
}

# We carry out matching only on courses where at least some students have
# demonstrated interest in serving as ULAs. Classes with no student interest will
# need to be recruited for and matched later.
courses.interest <- courses[courses$interest != 0,]
courses.interest$course_number <- seq(1:nrow(courses.interest))
courses.nointerest <- courses[courses$interest == 0,]

# Get student meta data, in particular, first and last name
s.meta <- list.files(pattern="*_[0-9]")
s.name <- unlist(lapply(s.meta, get.name))
s.year <- unlist(lapply(s.meta, get.year))

# Write file with all student preferences and name
student_preferences <- s.prefs
names(student_preferences) <- s.name
saveRDS(student_preferences, "student_preferences.RDS")

# Write file with student names, netids, and pins
s.pin <- unlist(lapply(meta, get.pin))
student_credentials <- as.data.frame(cbind(s.name, s.id, s.pin))
names(student_credentials) <- c("student", "netid", "pin")
write.csv(student_credentials, "student_credentials.csv", row.names=FALSE)

# Assign students numbers
student.mapping <- as.data.frame(cbind(s.name, seq(1:length(s.name))),
                                 stringsAsFactors=FALSE)
names(student.mapping) <- c("name", "student_number")


# Get professor preferences
temp.p.prefs <- list.files(pattern="[A-Z]{2}.csv")
p.info <- unname(unlist(lapply(temp.p.prefs, read.table, 
                               stringsAsFactors=FALSE, header=FALSE)))
# Get rid of empty rankings
p.info <- gsub(", \"<Please select a student>\"", "", p.info)

# Get rid of initial rankings if a professor ranked multiple times because we
# use file append in the event that two professors miscommunicate and submit
# rankings for the same class.
p.info.courses <-  substr(p.info, start=7, stop=14)
p.info.to.delete <- length(p.info.courses) + 1 - which(duplicated(rev(p.info.courses)))
p.info <- p.info[-p.info.to.delete]

# Create matrix of student preferences
s.pref.matrix <- matrix(ncol=nrow(student.mapping), nrow=nrow(courses.interest))
for (i in 1:length(s.prefs)) {
  s.temp <- rep(NA, nrow(courses.interest))
  for (j in 1:nrow(s.prefs[[i]])) {
    s.temp[j] <- courses.interest$course_number[courses.interest$course == s.prefs[[i]]$Title[j]]
  }
  s.pref.matrix[,i] <- s.temp
}

# Create matrix of professor preferences, collect number of slots per class
if (nrow(courses.nointerest) > 0) {
  ula.notinterested <- courses.nointerest[,c("course", "number", "interest")]
  names(ula.notinterested) <- c("course", "desired", "assigned")
}

ula.interested <- rep(NA, nrow(courses.interest))
p.pref.matrix <- matrix(ncol=nrow(courses.interest), nrow=nrow(student.mapping))
for (i in 1:length(p.info)) {
  info <- eval(parse(text=p.info[i]))
  if (info[[1]] %in% courses.interest$course) {
    ula.interested[which(courses.interest$course == info[[1]])] <- info[[2]]
    temp <- rep(NA, nrow(student.mapping))
    for (j in 1:length(info[[3]])) {
      ifelse(is.null(info[[3]][j]), 
             temp[j] <- NA, 
             temp[j] <- as.numeric(student.mapping$student_number[student.mapping$name == info[[3]][j]]))
    }
    p.pref.matrix[,courses.interest$course_number[courses.interest$course == info[[1]]]] <- temp
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

# Create faculty interest output
faculty_preferences <- list()
list_names <- rep(NA, length(p.info))
for (i in 1:length(p.info)) {
  info <- eval(parse(text=p.info[i]))
  list_names[i] <- info[[1]]
  faculty_preferences[[i]] <- info[[3]]
}
names(faculty_preferences) <- list_names

saveRDS(faculty_preferences, "faculty_preferences.RDS")





# ============================== Below works when using deprecated script up to HRI match

library(matchingR)

fill_column <- function(col_values, max_val) {
  not_ranked <- setdiff(seq(1:max_val), na.omit(col_values))
  return(c(na.omit(col_values), sample(not_ranked)))
}

s.pref.matrix_temp <- apply(s.pref.matrix, 2, fill_column, max_val=nrow(courses.interest))
p.pref.matrix_temp <- apply(p.pref.matrix, 2, fill_column, max_val=length(s.prefs))

galeShapley.collegeAdmissions(studentPref=s.pref.matrix_temp, 
                              collegePref=p.pref.matrix_temp, 
                              slots=courses.interest$number)

