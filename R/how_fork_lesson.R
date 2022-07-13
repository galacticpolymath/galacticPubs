#' How to "fork" a lesson to make a new language version?
#'
#' Instructions for the steps needed to copy a whole galacticPubs lesson
#'
#' @export
#'

how_fork_lesson<- function(){
  utils::browseURL("https://github.com/galacticpolymath")
  message(
    "\nSTEPS FOR FORKING A LESSON\n",
    "___________________________\n",
    "0) If you don't have 'gh cli' installed, do so\n",
    "   - https://cli.github.com/manual/installation\n",
    "1) On GitHub find the lesson repository\n",
    "2) Click Code and copy the SSH link\n",
    "3) In RStudio, click File>New Project>Version Control>Clone Git Repo\n",
    "   - Paste link from #2 under URL field. \n",
    "   - **Add desired locale suffix to repo name\n",
    "4) Check 'Open in new session to keep this window open'\n",
    "   - Make sure it's going in Edu/Lessons Folder and click 'Create Project'\n",
    "5) IMPORTANT, you have to push this new repo to our Github page with gh cli",
    "   - run gh_push_forked_repo()"
  )

}
