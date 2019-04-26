# SCRIPT 1 ----
# Use this script to get a list of emails of people who applied
# You can then send these people the link to view and accept or decline their assignments

all_files <- list.files(pattern= '.*_[0-9]+')
emails <- c()
for (file in all_files) {
  emails <- c(emails, read.csv(file, as.is=TRUE)$email)
}

write.csv(emails, "ULA_emails.csv")




# SCRIPT 2 ----
# Use this to view all the responses to the assignments and generate a starting point for the CSV you will send Karen
# Also use this to get info about what slots need to be filled and what waitlisted students you have

# load in all the decision data
decisions <- list.files(pattern= '.*_decision')
decisions_data <- data.frame("course"=0, "decision"=0, "netid"=0)
for (file in decisions) {
  current_decision <- read.csv(file, as.is=TRUE)
  for (row in 1:nrow(current_decision)) {
    decisions_data <- rbind(decisions_data, unlist(c(current_decision[row,], substr(file, 1, nchar(file)-13))))
  }
}
decisions_data <- decisions_data[-1,]

# load in all the netid/email data
all_files <- list.files(pattern= '.*_[0-9]+')
netids <- data.frame("netid"=0, "first_name"=0, "last_name"=0, "email"=0, "year"=0)
for (file in all_files) {
  netids <- rbind(netids, read.csv(file, as.is=TRUE)[,c('netid', 'first_name', "last_name", "email", "year")])
}
netids$name <- paste(netids$first_name, netids$last_name)

# load in all the assignment data
assignments <- read.csv("Final_assignments.csv")[,-1]
assignments <- assignments[assignments$course != "unassigned",]

# fill in netids and emails
assignments$netid <- ""
assignments$email <- ""
assignments$year <- ""
for (i in 1:length(netids$name)) {
  assignments$netid[assignments$student == netids$name[i]] <- netids$netid[i]
  assignments$email[assignments$student == netids$name[i]] <- netids$email[i]
  assignments$year[assignments$student == netids$name[i]] <- netids$year[i]
}

# fill in decisions
assignments$decision <- ""
for (i in 1:length(decisions_data$course)) {
  assignments$decision[assignments$course == decisions_data$course[i] & assignments$netid == decisions_data$netid[i]] <- decisions_data$decision[i]
}

# sort by class
assignments <- assignments[order(assignments$course),]

# save the csv
write.csv(assignments, "Assignments_full.csv")

# save the csv of students who accepted, without any declined (for hiring)
write.csv(assignments[assignments$decision=="Yes",], "Assignments_accepted.csv")

# WAITLIST INFO ----
# get a list of waitlisted students for yourself that you can then ask to fill in for the declined assignees
# the "faculty_ranked" column records whether the faculty ranked the student as someone they would work with
# if faculty did not rank the student, you can still ask them whether they would like that student now that their options are limited

# read in faculty desires
faculty_preferences <- readRDS("faculty_preferences.RDS")

# record waitlist info
waitlist_netids <- decisions_data$netid[decisions_data$course=="unassigned" & decisions_data$decision=="Yes"]
n <- length(waitlist_netids)
waitlisted <- data.frame("course"=0, "faculty_ranked"=0, "name"=0, "email"=0, "netid"=0)
for (i in 1:n) {
  netid <- waitlist_netids[i]
  name <- netids$name[netids$netid==netid]
  email <- netids$email[netids$netid==netid]
  interested <- read.csv(paste0(netid, "_preferences.csv"), as.is=TRUE)[,1]
  for (course in interested) {
    if (name %in% faculty_preferences[[course]]) {
      waitlisted <- rbind(waitlisted, c(course, "Y", name, email, netid))
    } else {
      waitlisted <- rbind(waitlisted, c(course, "N", name, email, netid))
    }
  }
}
waitlisted <- waitlisted[-1,]

# sort by class 
waitlisted <- waitlisted[order(waitlisted$course),]

write.csv(waitlisted, "Waitlisted_students.csv")
