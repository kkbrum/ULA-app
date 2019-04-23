# ULA-app
This user guide is meant to describe the exact steps you should go through to conduct the whole ULA matching process. For a more general overview, checkout the Readme.

## Server
0. Make sure you have access to the S&DS server. Move all the files into a folder named based on the past semester. (MARIA PUT COMMAND HERE TO DO THIS). Now when you complete all the following steps, you can download this github repo and put files onto the server as you change them for the upcoming semester.

## Timeline
1. First, you should establish the timeline for the whole process. You should get this approved by the DUS/DGS. Here are the timelines we have used in the past:
#### Fall 2018: 
- Student Input: Aug 13-Aug 24
- Faculty Input: Aug 25-29
- Matching & Admin Approval: Aug 30-31
- Decisions Released: Sep 1
#### Spring 2019:
- Student Input: Dec 17-Dec 31
- Faculty Input: Jan 2-Jan 5
- Matching: Jan 6-Jan 7
- Admin Approval: Jan 8-Jan 9
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

## Faculty Preferences

## Matching

## Admin approval

## Student Decisions

## Collecting Decisions and Moving Forward

CONGRATULATIONS! You have now run the department :D
