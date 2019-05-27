# ==============================================================================
# ULA problem data structures

# dim(s.prefs) <- nrow=nclasses (with interest), ncol=nstudents
# dim(p.prefs) <- nrow=nstudents, ncol=nclasses (with interest)
# always need to pad with NA's to ensure proper matrix dims
# ==============================================================================

# ===================================   SETUP   ================================
library(matchingR) # requires Rcpp



get.id <- function(string) {
  return(substring(string, 1, regexpr("_", string) - 1))
}


get.name <- function(file) {
  temp <- read.csv(file, as.is=TRUE, row.names=NULL)
  return(paste(temp$first_name, temp$last_name))
}


get.year <- function(file) {
  temp <- read.csv(file, as.is=TRUE, row.names=NULL)
  return(temp$year)
}


get.pin <- function(string) {
  return(substring(string, regexpr("_", string) + 1, nchar(string) - 4))
}


# Insert NULL values for courses where profs haven't ranked any students, but 
# submitted blank preferences and students were interested
insert_null <- function(tmp, desired_length=3) {
  if (length(tmp) < desired_length) {
    tmp[length(tmp) + 1] <- list(NULL) 
  }
  return(tmp)
}


fill_column <- function(col_values, max_val) {
  not_ranked <- setdiff(seq(1:max_val), na.omit(col_values))
  return(c(na.omit(col_values), sample(not_ranked)))
}


# ============================   CREATE MATCH SETUP   ==========================
# Courses being offered for a given semester, input manually
courses <- read.csv("courses.csv", as.is=TRUE)

temp.s.prefs <- list.files(pattern="*_preferences.csv")
s.id <- unlist(lapply(temp.s.prefs, get.id))
s.prefs <- lapply(temp.s.prefs, read.csv, as.is=TRUE)

# Get course interest from students
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
# need to be recruited for and matched later. We'll deal with no faculty interest
# later when we correct matching.
courses.interest <- courses[courses$interest != 0,]
courses.nointerest <- courses[courses$interest == 0,]
courses.interest$course_number <- seq(1:nrow(courses.interest))

# Get student meta data, in particular, first and last name
s.meta <- list.files(pattern="*_[0-9]")
s.name <- unlist(lapply(s.meta, get.name))
s.year <- unlist(lapply(s.meta, get.year))

# Write file with all student preferences and name
student_preferences <- s.prefs
names(student_preferences) <- s.name
saveRDS(student_preferences, "student_preferences.RDS")

# Write file with student names, netids, and pins
s.pin <- unlist(lapply(s.meta, get.pin))
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
p.info <- gsub(', \"<Please select a student>\"|\"<Please select a student>\"|, \n', "", p.info)

# Get rid of initial rankings if a professor ranked multiple times because we
# use file append in the event that two professors miscommunicate and submit
# rankings for the same class.
p.info.courses <-  substr(p.info, start=7, stop=14)
p.info.to.delete <- length(p.info.courses) + 1 - which(duplicated(rev(p.info.courses)))
if (length(p.info.to.delete ) > 0) {
  p.info <- p.info[-p.info.to.delete]
}

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
  info <- insert_null(eval(parse(text=p.info[i])))
  if (info[[1]] %in% courses.interest$course) {
    ula.interested[which(courses.interest$course == info[[1]])] <- info[[2]]
    temp <- rep(NA, nrow(student.mapping))
    for (j in 1:length(info[[3]])) {
      ifelse(is.null(info[[3]][j]), 
             temp[j] <- NA, 
             temp[j] <- as.numeric(student.mapping$student_number[student.mapping$name == info[[3]][j]]))
      p.pref.matrix[,courses.interest$course_number[courses.interest$course == info[[1]]]] <- temp
    }
  }
}

# Deal with case where professor has not submitted preferences, but students
# have ranked the class
# Ties broken randomly
empty.cols <- which(apply(p.pref.matrix, 2, sum, na.rm=TRUE) == 0)
for (i in empty.cols) {
  temp.prefs <- as.data.frame(matrix(nrow=ncol(s.pref.matrix), ncol=2))
  for (j in 1:nrow(temp.prefs)) {
    ifelse(i %in% s.pref.matrix[,j],
           temp.prefs[j,] <- c(j, which(s.pref.matrix[,j] == i)),
           temp.prefs[j,] <- c(j, NA))
  }
  temp.prefs <- temp.prefs[order(temp.prefs[,2], decreasing=FALSE),]
  temp.prefs[!is.na(temp.prefs[,2]),] <- temp.prefs[sort.list(temp.prefs[,2], 
                                                              decreasing=FALSE, 
                                                              method="quick", 
                                                              na.last=NA),]
  p.pref.matrix[,i] <- temp.prefs[,1]
}

# Create faculty interest output
faculty_preferences <- list()
list_names <- rep(NA, length(p.info))
for (i in 1:length(p.info)) {
  info <- insert_null(eval(parse(text=p.info[i])))
  list_names[i] <- info[[1]]
  faculty_preferences[[i]] <- info[[3]]
}
names(faculty_preferences) <- list_names

saveRDS(faculty_preferences, "faculty_preferences.RDS")

# ================================   FORCE MATCH   =============================
# Force a Gale-Shapley matching by filling in all NA values for all students and 
# professors (i.e. randomly rank courses or students) and then do a post-match
# correction. 
s.pref.matrix_temp <- apply(s.pref.matrix, 2, fill_column, max_val=nrow(courses.interest))
p.pref.matrix_temp <- apply(p.pref.matrix, 2, fill_column, max_val=length(s.prefs))

matching <- galeShapley.collegeAdmissions(studentPref=s.pref.matrix_temp, 
                                          collegePref=p.pref.matrix_temp, 
                                          slots=courses.interest$number)

assignments <- cbind(student.mapping, matching$matched.students)
names(assignments) <- c("student_name", "student_number", "course_number")
assignments <- merge(assignments, courses.interest[,c("course", "course_number")], all.x=TRUE)
names(assignments) <- c("course_number", "student_name", "student_number", "course_name")

# Now correct matches to reflect three scenarios:
# - student is not in the professor's list
# - the professor has no preferences, and therefore student preferences should rule
# - the professor doesn't like any of the students that have ranked. 

# First, get special course interest  markers from faculty
# Redundancy in checking for existence of `checkboxes.csv` is for transparency,
# not functionality. 
if (file.exists("checkboxes.csv")) {
  checkboxes <- read.csv("checkboxes.csv", stringsAsFactors=FALSE)
  checkboxes <- checkboxes[!rev(duplicated(rev(checkboxes$course))),]
  no.prof.preferences <- checkboxes$course[checkboxes$no_preferences]
  no.prof.desire <-  checkboxes$course[checkboxes$no_desire]
}

# Now, fix assignments following two cases
if (file.exists("checkboxes.csv")) {
  for (i in 1:nrow(assignments)) {
    temp_pref <- unname(unlist(faculty_preferences[assignments$course_name[i]]))
    if ((!(assignments$student_name[i] %in% temp_pref) &
         (assignments$course_number[i] %in% no.prof.preferences)) |
        assignments$course_name[i] %in% no.prof.desire) {
      assignments$course_number[i] <- NA
      assignments$course_name[i] <- NA
    }
  }
} else {
  for (i in 1:nrow(assignments)) {
    temp_pref <- unname(unlist(faculty_preferences[assignments$course_name[i]]))
    if (!(assignments$student_name[i] %in% temp_pref)) {
      assignments$course_number[i] <- NA
      assignments$course_name[i] <- NA
    }
  }
}

# ===============================   PREP OUTPUTS   =============================
assigned <- assignments[!is.na(assignments$course_number),c("course_name", "student_name")]
names(assigned) <- c("course", "student")

unassigned <- assignments[is.na(assignments$course_number),]
if (nrow(unassigned) > 0) {
  unassigned$prefs <- NA
  for (i in 1:nrow(unassigned)) {
    unassigned$prefs[i] <- toString(s.prefs[[as.numeric(unassigned$student_number[i])]]$Title)
  }
}
unassigned <- unassigned[,c("student_name", "prefs")]

ula.desired<- courses[,c("course", "number")]
names(ula.desired) <- c("course", "desired")

ula.assigned <- as.data.frame(table(assignments$course_name))
names(ula.assigned) <- c("course", "assigned")

ula.demand <- merge(ula.desired, ula.assigned, all.x=TRUE)
ula.demand$assigned[is.na(ula.demand$assigned)] <- 0
ula.demand$needed <- ula.demand$desired - ula.demand$assigned

# Write csvs with course assignment numbers, assigned student information, and
# unassigned student information
write.csv(assigned, "Assignments.csv", row.names=FALSE)
write.csv(unassigned, "Unassigned-Students.csv", row.names=FALSE)
write.csv(ula.demand, "Demand.csv", row.names=FALSE)

# done.
