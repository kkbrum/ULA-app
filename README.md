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
    * Compute a stable match for the ULA problem (which is the same as the hospital residency or college admissions problem)
* Administrator Interface
    * Administrator observes matching algorithm results and decide on final assignments.
* Decision Interface
	* After the administrator makes final assignments, students will be able to log in to view their ULA assignment and accept or reject.
	* The information obtained from these student responses will be used to make the hiring process easier and more straightforward from an administrative perspective.

## How it works
All components of the application are tied together in `app.R`, but individual components can be found in `Archived/`. The components of the application need to be executed sequentially, and this will be controlled based on access date -- certain people will have access to the app at certain times. For now, you can control what piece of the app you want to interact with by setting the `app_number` variable according to the instructions listed in `app.R`. The student interface is run first. In this portion of the app you can either fill in all of the required fields, including rankings, as a new student, or login using `sid1`, `sid2`, ... , `sid10` as a username and `1234` as a pin. After exploring the student interface, reset the `app_number` variable accordingly to view the faculty interface. There you can login using `JE`, `JL`, `SW`, or `WL` as a username and `1234` as a pin. Here you will be able to tab through the classes being taught by the faculty member and rank interested students for each one. If you're considering working all the way through the student decision interface of the app, you should fill out preferences for all four professors. After you are finished with faculty preferences, reset the `app_number` variable to view the administrator interface. You can login here using either `DS` or `ST` as a username and `1234` as a pin. Upon launching this interface, a stable matching algorithm will be run to assign ULAs to classes. This may take a moment to run, so be patient if the screen doesn't change immediately. In the event that the algorithm doesn't converge, which can and does happen, a secondary algorithm will run to match in the scenario that there is a mutual first choice between a professor and a student. Administrators have the opportunity to manually change any suggested assignments before they are finalized, and will have access to both student and faculty preferences when considering changes. Finally, reset the `app_number` variable accordingly to view the student decision interface. Once again, you can login using `sid1`, `sid2`, ... , `sid10` as a username and `1234` as a pin, or using any credentials that you created when you launched the student interface. Here you will be able to accept or decline your ULA assignment. This information can then be accessed by relevant members of the S&DS Department for hiring purposes.
