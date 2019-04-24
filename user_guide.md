# ULA-app
This user guide is meant to describe the exact steps you should go through to conduct the whole ULA matching process. For a more general overview, checkout `README.md`.

## Server
0. Make sure you have access to the S&DS server (contact Jay about this). Move all the files into a folder that is named based on the past semester. (MARIA PUT COMMAND HERE TO DO THIS). Now when you complete all the following steps, you can download this github repo and put files onto the server as you change them for the upcoming semester with the following useful commands (CUE MARIA).

## Timeline
1. First, you should establish the timeline for the whole process. You should get this approved by the DUS/DGS. Here are the timelines we have used in the past:
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
- If you ever need to test a different app, you can just comment out the section of `app.R` where the dates determine the `app_number` and set `app_number` to be whatever you'd like.

## Initial Set Up
3. Upload the most recent `app.R` from github to the server. If you made any improvements in the last semester, make sure those get incorporated.

4. Update `courses.csv` with the courses being taught in the upcoming semester that need ULAs and input the desired number of ULAs.
- You will need to get this information from the DUS/DGS. 

5. Update `Profs.csv` with the professors of the courses in `courses.csv`. Make sure the `prof` column in the two files match exactly. 
- For courses with multiple professors, just have one line for them and treat them as one person.
- We have been setting usernames to the initials of the professors and the pins to random 4 digit numbers that we generate randomly (for example https://numbergenerator.org/random-4-digit-number-generator). Make sure not to put these on github as this is a public repo. Just update this on the server.

6. Update `Admins.csv` with user credentials for whoever will be approving your matching. This is probably the DUS/DGS. Again, generate random 4 digit pins but only store them on the server.

The server should now have all the files needed to get started.

## Student Preferences
7. Test the student interface by filling it out for yourself (you can delete this later). 
- Test that when you submit and have errors in your form, you get an error message but the file saves two files on the server as `netid_pin.csv` and `netid_save.csv`, and similarly when you hit the save button. 
- When you hit the submit button with no errors, the `netid_save.csv` button should go away and `netid_preferences.csv` should be created. Check that these look okay and everything is in order.
- To remove these files if you don't want them, you can run `rm file` on the server.

8. Send an email, which is drafted on github in `Emails/1_initial_student_recruitment.txt`, to the DUS/DGS to be sent to students about the opening of the application. Send a similar email when there are 4-7 days left of recruitment and one when there is 1 day left. 

9. Keep track of how many applications you've received vs. how many total ULAs have been requested (look in your `courses.csv` doc to know how many requested). If the application time is coming to a close and there are too few applications, there are a couple suggestions (Maria and Katherine have done the first two, haven't done the third but could be done).
- Email anyone who has a `netid_save.csv` file but not a `netid_preferences.csv` file (although they should never coexist) and ask if they are planning on finishing the application and that they should do so before the deadline. This email is drafted under `Emails/2_saved_student_recruitment.txt`.
- Email anyone who ULAed last semester but has not yet applied and has not graduated. You can get a list of past ULAs and their graduation years from Maria and Katherine for the first round. This email is drafted under `Emails/3_past_student_recruitment.txt`.
- Email neighboring departments such as applied math and computer science in case any of their students want to ULA an S&DS class and aren't on our panlist. This is not drafted as it hasn't been done so far.

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
13. Run `matching.R`. This currently does not do much matching for you, but will be updated in the coming weeks to do a better job.

14. If you need to do some manual matching, here are some tips:
- Log in as one of the admins to give you a visual representation of the preferences of each student and each class.
- If you click on `Preferences` under each course, it will list the students the faculty ranked in that order. Click on this for every class. 
- Start with the classes that have the fewest ranked students relative to the number of ULAs needed. Go through each student in the list and add them to the class if it was their highest ranked option where the professor also ranked them. You can see the student's rankings by clicking their name, and you can move them to a class by clicking their name and then the course name. If you haven't reached enough ULAs for that class, add more based on who ranked that course more highly.
- Continue going through courses, potentially moving people around so that you satisfy the demand of all courses most fully. While matching student and faculty interest as fully as possible would be great, this unfortunately sometimes makes it such that not all students can be utilized and not all courses get enough ULAs. Prioritize getting the most number of students assigned in ULA positions.

## Admin approval
14. Test the admin interface. Make sure the credentials work and then switch a student's assignment and press `Finalize Assignments` to make sure the file saves nicely in `Final_assignments.csv`. Switch the student back and finalize the assignments again. 

15. You should get approval from the DUS/DGS on your rankings. Send them an email, drafted in `Emails/7_admin_approval.txt` with their credentials so they can take a brief look at the situation. 

## Student Decisions
16. Log in as a student and check that the decision interface looks okay.

17. Generate a list of applicants' emails by running the first script in `ULA_scripts.R`. Send an email to them announcing the release of decisions, drafted in `Emails/8_decisions_released.txt`.

## Collecting Decisions and Moving Forward
18. Every day from that point, I would run the second script in `ULA_scripts.R` and see if anyone has declined their assignment. If someone has declined:
- Hopefully there is someone on the waitlist (provided by the second script as well) who could take their spot and should get emailed: 
  - If they are on the waitlist and were ranked by the faculty, they can be sent an email such as `Emails/9_waitlist_offer.txt`. 
  - If they were not ranked by faculty, and there is no one else available, send the professor an email such as `Emails/10_waitlist_prof.txt` before sending the offer. 
  - Anyone you assign off the waitlist who accepts their offer via email should be added manually to the `Final_assignments.csv` file and their decision file `netid_decision.csv` should also get updated. 
- If there is no one to take their spot, you can wait to see how many vacant total spots the class has (once everyone has responded to their assignment) and then send the professor an email such as `Emails/11_vacant_spots.txt`.

19. Once it's been a few days since decisions were released, send people who were assigned a course and have not yet responded to their offer an email like `12_decision_poke.txt`. If students are not responding, try reaching out to them via facebook/in person/etc to chat about ULAing and what they are waiting for in order to make their decision.

20. Once all decisions have been made, OR if some students are not responding but you want to inform professors of their ULAs, send the professors emails such as `13_faculty_announcement.txt`. You might have to deal with a few stragglers after this.

21. Once all possible ULAs have been assigned and all decisions made, run the second script in `ULA_scripts.R` again and send the resulting `Assignments_full.csv` to Karen, or whoever will be doing the actual hires.

CONGRATULATIONS! You have now run the department :D
