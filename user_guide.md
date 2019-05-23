# ULA-app
This user guide is meant to describe the exact steps you should go through to conduct the whole ULA matching process. For a more general overview, checkout `README.md`.

## Server
0. The ULA app is run from the S&DS server. You should contact Jay about getting access to this. In addition to login credentials, he should also be able to provide you information about where the ULA app exists within the server (`/srv/shiny-server/ULA`). After navigating to the ULA app in the server you should move all of the old files into a file named based on the past semester. You can do this by running the following two commands: first `mkdir <NEW DIRECTORY NAME>`, then `mv *.csv *.R *.RDS <NEW DIRECTORY NAME>`. When you complete the steps outlined below, you can download files from the github repo and upload them to the server as you change them for the upcoming semester. Some commands relevant to the process of navigating around the server, editing, uploading, and downloading files are the following:
- `cd <DIRECTORY NAME>`:
    - Use `cd`, for "change directory", to navigate around the server
- `pwd`:
    - Use `pwd`, for "print working directory", to show where you are within the server
- `ls`:
    - Use `ls`, for "list", to show the files in your current working directory
- `cat <FILE NAME>`:
    - Use `cat`, for "concatenate", to show the contents of a file in their entirety
- `rm <FILE NAME>`:
    - Use `rm`, for "remove", to delete a file
    - Be thoughtful when using this command
- `vim <FILE NAME>`:
    - On the server you can edit the contents of a file using the Vim text editor
    - The command `vim <FILE NAME>` will open up a file in text form allowing for you to edit it directly on the server
    - Vim is a modal text editor and there are two modes relevant to you:
        - Normal mode, which is how you navigate text
        - Insert mode, which is how you edit text
    - Normal mode is Vim's default mode. You can enter it at any time by pressing `ESC`
        - In normal mode on the server you can navigate around the document using the arrow keys
    - You can enter Insert mode at anytime by pressing `i`. Here you can edit text using your keyboard as you would normally
    - Some notes on exiting Vim:
        - `:w` will save your work
        - `:q` will quit, returning you to your shell
        - `:wq` will save your work, then quit
        - `:q!` will quit without saving any of your changes
- `scp <FILE NAME> <USER@MACHINE:DESTINATION>`:
    - Use this command to upload files to the server from your local machine
    - `USER@MACHINE` will be the full login you use to ssh into the server
    - `DESTINATION` is the absolute path of the directory where you would like to copy the file; you can use `pwd` on the server to get the absolute path if you are in the directory where you would like to copy
- `async -avz <ORIGIN> <DESTINATION>`:
    - Use this command to download files from the server to your local machine
    - `-avz` is just MG's preferred default flags, these are not important and if you have different thoughts about them, they can be changed
    - `<ORIGIN>` follows the construction `<USER@MACHINE:PATH-TO-FILE>`, similar to what's above. `PATH-TO-FILE` is the absolute path of the file on the server you wish to download to your local machine
    - `<DESTIONATION>` is the directory on your local machine where you wish to download the file to
- `touch restart.txt` will force the app to restart even though you haven't changed `app.R`. This is useful if you changed the csv files the app relies on and want the changes to be reflected.

## Timeline
1. First, you should establish a timeline for the whole process. You should get this approved by the DUS/DGS. Here are the timelines we have used in the past:
#### Fall 2018: 
- Student Input: Aug 13-Aug 24
- Faculty Input: Aug 25-Aug 29
- Matching & Admin Approval: Aug 30-Aug 31
- Decisions Released: Sep 1
#### Spring 2019:
- Student Input: Dec 17-Dec 31
- Faculty Input: Jan 2-Jan 7
- Matching & Admin Approval: Jan 8-Jan 9
- Decisions Released: Jan 10

2. You need to update `dates.csv` with the dates from your timeline. 
- Make sure to enter them in the MM/DD/YYYY format. 
- You can set the starting date for the student app to be today's date, as no one will start filling anything out without the link anyways! 
- If you ever need to test a different app, you can just comment out the section of `app.R` where the dates determine the `app_number` and set `app_number` to be whatever you'd like as indicated by the key at the top of the file.
- Run `touch restart.txt` to have the app recognize your changes to `dates.csv`.

## Initial Set Up
3. Upload the most recent `app.R` from github to the server. If you made any improvements in the last semester, make sure those get incorporated.

4. Update `courses.csv` with the courses being taught in the upcoming semester that need ULAs and input the desired number of ULAs.
- You will need to get this information from the DUS/DGS. 

5. Update `Profs.csv` with the professors of the courses in `courses.csv`. Make sure the `prof` column in the two files matches exactly. 
- For courses with multiple professors, just have one line for them and treat them as one person.
- We have been setting usernames to the initials of the professors and the pins to random 4 digit numbers that we generate randomly (for example https://numbergenerator.org/random-4-digit-number-generator). Make sure not to put these on github as this is a public repo. Just update this on the server.

6. Update `Admins.csv` with user credentials for whoever will be approving your matching. This is probably the DUS/DGS. Again, generate random 4 digit pins but only store them on the server.

The server should now have all the files needed to get started.
- Run `touch restart.txt` to have the app recognize your changes to these files.


## Student Preferences
7. Test the student interface by filling it out for yourself (you can delete this later). 
- Test that when you submit and have errors in your form, you get an error message but the file saves two files on the server as `netid_pin.csv` and `netid_save.csv`, and similarly when you hit the save button. 
- When you hit the submit button with no errors, the `netid_save.csv` file should go away and `netid_preferences.csv` should be created. Check that these look okay and everything is in order.
- If you want to remove these file, you can run `rm file_name` on the server.

8. Send an email, which is drafted on github in `Emails/1_initial_student_recruitment.txt`, to the DUS/DGS to be sent to students about the opening of the application. Send a similar email when there are 4-7 days left of recruitment and one when there is 1 day left. 

9. Keep track of how many applications you've received vs. how many total ULAs have been requested (look in your `courses.csv` doc to know how many requested). If the application time is coming to a close and there are too few applications, there are a couple suggestions (Maria and Katherine have done the first two, haven't done the third but could be done).
- Email anyone who has a `netid_save.csv` file but not a `netid_preferences.csv` file (although they should never coexist) and ask if they are planning on finishing the application and that they should do so before the deadline. This email is drafted under `Emails/2_saved_student_recruitment.txt`.
- Email anyone who ULAed last semester but has not yet applied and has not graduated. You can get a list of past ULAs and their graduation years from Maria and Katherine for the first round. This email is drafted under `Emails/3_past_student_recruitment.txt`.
- Email neighboring departments such as applied math and computer science in case any of their students want to ULA an S&DS class and aren't on our panlist. This is not drafted as it hasn't been necessary to date.

## Faculty Preferences
10. Test the faculty interface. 
- Log in to each professor's account and check that the student data is loading in properly.
- Pay particular attention to professors teaching multiple classes (if there are any). They should have multiple tabs, one for each course.
- Check that the faculty preferences are saving properly, especially in the case of multiple classes.
- Remove any files you generated using `rm INITIALS.csv`

11. Email each faculty individually with their user and pin to log in to rank their preferences. The email is drafted under `Emails/4_faculty_credentials.txt`.
- If you noticed during the testing that a class had either no students who ranked it or just one or two, the professor is going to need to do some of their own recruiting. This is especially likely for higher level challenging classes. You should instead send them an email like `Emails/5_faculty_low_interest.txt` and 'cc the DUS/DGS so that they are aware of the situation.

12. If a faculty hasn't filled out their preferences, send them a followup email a couple days later, drafted in `Emails/6_faculty_poke.txt`, or visit their office if you are in town.

## Matching
13. Run `matching.R`. The only dependency necessary to do this is the package `matchingR`, which also requires `Rcpp`. There are two options here:
- Run the matcher on your personal machine. (More control over package management)
    1. Download all files from the server onto your personal machine using `rsync`.
    2. Open `Matching.R`, making sure that it's in the same directory as all of the relevant input files. 
    3. If necessary, install `matchingR` from CRAN if necessary using the command `install.packages("matchingR")`.
    4. Run the script sequentially in its entirety. Bonus points for reproducibility if you run it from the command line. Open `R` from terminal by typing the command `R` and hitting enter and then running `source("Matching.R")`. You'll need to be in the same directory as your script in order for this to work.
    5. Upload all necessary files back up to the server. These files are: FILL ME IN HERE.
- Run the matcher on the server. (Easier in terms of file management)
    1. Install `matchingR` from CRAN. Unless someone loads this onto the server for all users, each individual looking to run the matching script will need to install the package into a personal library using the command `install.packages("matchingR")`, following instructions from R regarding CRAN mirrors, etc.
    2. Run the script sequentially in its entirety. Bonus points for reproducibility if you run it from the command line. Open `R` from terminal by typing the command `R` and hitting enter and then running `source("Matching.R")`. You'll need to be in the same directory as your script in order for this to work.

14. If you need to do some manual matching, here are some tips:
- Log in as one of the admins to give you a visual representation of the preferences of each student and each class.
- If you click on `Preferences` under each course, it will list the students the faculty ranked in that order. Click on this for every class. 
- Start with the classes that have the fewest ranked students relative to the number of ULAs needed. Go through each student in the list and add them to the class if it was their highest ranked option where the professor also ranked them. You can see the student's rankings by clicking their name, and you can move them to a class by clicking their name and then the course name. If you haven't reached enough ULAs for that class, add more based on which students preferred that class the most.
- Continue going through courses, potentially moving people around so that you satisfy the demand of all courses most fully. While matching student and faculty interest as fully as possible would be great, this unfortunately sometimes makes it such that not all students can be utilized and not all courses get enough ULAs. Prioritize getting the most number of students assigned in ULA positions.

## Admin approval
15. Test the admin interface. Make sure the credentials work and then switch a student's assignment and press `Finalize Assignments` to make sure the file saves nicely in `Final_assignments.csv`. Switch the student back and finalize the assignments again. 

16. You should get approval from the DUS/DGS on your rankings. Send them an email, drafted in `Emails/7_admin_approval.txt` with their credentials so they can take a brief look at the situation. 

## Student Decisions
17. Log in as a student and check that the decision interface looks okay.

18. Generate a list of applicants' emails by running the first script in `ULA_scripts.R`. Send an email to them announcing the release of decisions, drafted in `Emails/8_decisions_released.txt`.

## Collecting Decisions and Moving Forward
19. Every day from that point, I would run the second script in `ULA_scripts.R` and see if anyone has declined their assignment. If someone has declined:
- Hopefully there is someone on the waitlist (provided by the second script as well) who could take their spot and should get emailed: 
  - If they are on the waitlist and were ranked by the faculty, they can be sent an email such as `Emails/9_waitlist_offer.txt`. 
  - If they were not ranked by faculty, and there is no one else available, send the professor an email such as `Emails/10_waitlist_prof.txt` before sending the offer. 
  - Anyone you assign off the waitlist who accepts their offer via email should be added manually to the `Final_assignments.csv` file and their decision file `netid_decision.csv` should also get updated. 
- If there is no one to take their spot, you can wait to see how many vacant total spots the class has (once everyone has responded to their assignment) and then send the professor an email such as `Emails/11_vacant_spots.txt`, 'cc-ing the DUS/DGS so they are aware of the vacancy.

20. Once it's been a few days since decisions were released, send people who were assigned a course and have not yet responded to their offer an email like `Emails/12_decision_poke.txt`. If students are not responding, try reaching out to them via facebook/in person/etc. to chat about ULAing and what they are waiting for in order to make their decision.

21. Once all decisions have been made, OR if some students are not responding but you want to inform professors of their ULAs, send the professors emails such as `Emails/13_faculty_announcement.txt`. You might have to deal with a few stragglers after this.

22. Once all possible ULAs have been assigned and all decisions made, run the second script in `ULA_scripts.R` again and send the resulting `Assignments_accepted.csv` to Karen, or whoever will be doing the actual hires, in an email like `Emails/14_hiring.txt`.

CONGRATULATIONS! You have now run the department :D
