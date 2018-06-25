# ULA-app

## Background
This year, the Yale S&DS Department adopted the Undergraduate Learning Assistant (ULA) program in many of its classrooms. ULAs have both grading and teaching responsbilities and serve in very similar capacities to graduate student TAs/TFs.

## Problem
No system currently exists to organize the ULA recruitment and class assignment processes. The protocol that is currently in place is inefficient and the process is frustrating for students applying to be ULAs, faculty hoping to have ULAs in their classrooms, and administrators who oversee the process/hire students.

## General Overview (Project Structure)
* Student Interface
    * Students provide basic information about themselves and the courses that they've taken
    * Students rank their preferences in terms of classes that they would like to ULA
* Faculty Interface
    * Faculty see the names and meta-data of students interested in ULAing their class (minus student preferences)
    * Faculty ranks students to be their ULAs according to their preferences
* Stable Matching Algorithm
* Administrator Interface
    * Administrator observes matching algorithm results and decide on final assignments.
* Decision Interface
	* After the administrator makes final assignments, students will be able to log in to view their ULA assignment and accept or reject.
	* The information obtained from these student responses will be used to make the hiring process easier and more straightforward from an administrative perspective.

## How it works
The components of the application need to be executed sequentially, starting with the student interface. The student interface can be accessed by running the `Student.R` file. There you can either fill in all of the required fields, including rankings, as a new student, or login using `mg2396` or `kb797` as a username and `1234` as the pin. After exploring the student interface, run the `Faculty.R` file to view the faculty interface. There you can login using either `JL` or `WL` as a username and `1234` as a pin. Here you will be able to tab through the classes being taught by the faculty member and rank interested students for each one. After faculty preferences have been selected, run `Admin.R` and login with the username `DS` and the pin `1234` to view the administrator interface. Upon launching the interface, a stable matching algorithm will be run to assign ULAs to classes, but aministrators will have the opportunity to manually change these assignments before they are finalized. Finally, launch `Decision.R` to view the decision interface. Once again, you can login using `mg2396` or `kb797` as the username and `1234` as the pin, or using the credentials that you created when you launched `Student.R`.
