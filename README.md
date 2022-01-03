# racket-autograder-gradescope

Yet another Racket autograder for Gradescope.

Background
----------

Forthcoming.

Basic Setup
-----------

1. Create a Racket test/grade file.  (See the example below for guidance.)
   It need not be in this directory.

2. Create any additional files you might need.
   Once again, they need not be in this directory.

3. Run `make-zip GRADE_FILE.rkt [other-files]`

4. You should now have a zip file named `autograder-NAME.zip`.

5. Go to the page for the assignment on Gradescope or create a new 
   programming assignment (see below for details on creating a
   programming assignment).

6. The second checkbox should be "Configure Autograder".  Select that.

7. Upload your autograder.  Then click "Update Autograder" (or
   "Replace Autograder").  It should take a few minutes.  Ideally,
   you'll see a bunch of stuff under "Build Output" and nothing
   under "Build Errors".

   The "post-installing collections" step in Racket seems to take the
   longest.

   At some point, it should say "Successfully built ....".

8. Click "Test Autograder".

9. Upload a Racket file and any other files the student might upload.

10. Wait for the Autograder to finish.

11. If things don't go as expected, you may have to debug via SSH.
    The documentation is at 
    <https://gradescope-autograders.readthedocs.io/en/latest/ssh/>.

Sample Grade File
-----------------

See `examples/grade-example01.rkt`.

Creating Assignments
--------------------

Assignment creation in Gradescope is supposed to be obvious.  These notes
are just to remind us of some common steps.

1. Click on "Assignments" on the left.

2. Click on "Create Assignment" (or "Duplicate Assignment" on the
   lower right.

3. Click on "Programming Assignment"

4. Click "Next" (duh)

5. Assign one (1) Autograder point.

6. Enable Manual Grading.

7. Fill in the remainder as appropriate.  DO NOT enable a leaderboard.

8. We usually end up on a the Outline screen.  Add a new question for
   the human grader.  (I usually call it "Human Grader".)  It's worth
   three points for MPs and 0 points for labs.  Unfortunately, we can't
   add the rubric here.

9. Click on "Configure Autograder" and follow the steps above.

10. When the sample grading finishes, you may want to update the rubric
under the "Manage Submissions" bubble.  We usually use.

    * 1 Some Issues/Needs Revision (R)
    * 2 Meets Expectations (M)
    * 3 Excellent (E)
    * 4 Incomplete, incorrect, or no submissions (I)

