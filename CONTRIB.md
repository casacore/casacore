casacore is an open source project and everybody is encouraged to help improve
the quality of the code. You can help by reporting issues or even better
fix issues yourself. We use github as a central communication and development
platform. Issues can be reported there. If you have a patch for casacore 
we use the github pull request system. Also to keep the casacore code
quality high we have written down a procedure for contribution, see below.

General considerations:

 * if you have problems or questions with/about git or github, first check [1]
 * If you modify any code, make sure the test suit still runs. If it fails,
   fix the code or the relevant test
 * If you change a function/method fingerprint, update the documentation
   accordingly
 * If you add a function or method, add a test for it add it to the
   documentation
 * The tested code coverage line count should increase, not decrease
 * Follow the google style code as much as possible [2]
 
 

To propose a change to the casacore code base:

 1. Create a github account, setup SSH keys
 2. Fork the casacore repository on github [3]
 3. Create a branch and commit your changes to this branch
 4. Push your branch to your github fork (not the original casacore, you
    probably don't have permission)
 5. Issue a pull request [4]
 6. When the pull request is reviewed and there are no problems it will be
    accepted (peer review)
 7. It can happen there are some mistakes here and there, we use the github
    commenting system to discuss these issues.
 8. If there is a problem with the commit you need to fix it. you can commit
    to the same branch, the PR will be updated automatically.

General notes about Pull requests:

 * please create descriptive commits containing atomic changes.
 * You can rewrite the history of the commits in your branch using rebase,
   but don't rewrite the history before the first commit of your new branch.
 * If you rewrite your history of your branch you can force push those changes
   to your branch. the PR will be updated.
 * We like to keep the history clean, and prevent a lot of 'merge branch x' or
   'update README' commit messages.
 
 
 * [1] http://help.github.com 
 * [2] https://google-styleguide.googlecode.com/svn/trunk/cppguide.html
 * [3] https://help.github.com/articles/fork-a-repo/
 * [4] https://help.github.com/articles/creating-a-pull-request/
