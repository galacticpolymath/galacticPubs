#' Add this repository to galacticpolymath github
#'
#' This functino expects that:
#' - You have [gh cli](https://cli.github.com/manual/installation) (github command line) installed
#' - You're set up with git in Rstudio
#' - You have credentials to write to galacticpolymath github repos
#'
#' @export
#'

gh_add_this_repo<-function(){
  #test if this project is in the lessons directory, otherwise throw error
  WD<-getwd()
  project<-basename(WD)
  parent<-gsub(paste0("^.*/([^/]*)/",project,"$"),"\\1",WD)
  if(parent!="Lessons"){
    stop("This project doesn't appear to be in the 'Lessons' folder")
  }else{
    #Ref: https://docs.github.com/en/get-started/importing-your-projects-to-github/importing-source-code-to-github/adding-locally-hosted-code-to-github
    #initial commit
    git_cmd<-paste0("git add . && git commit -m 'initial commit'")
    tryCatch(system2("cd", paste0(WD," && ",git_cmd)), error=function(e){e})

    #push new repo to github
    gh_cmd<-paste0("gh create galacticpolymath/",project)
    tryCatch(system2("cd", paste0(WD," && ",gh_cmd)), error=function(e){e})

    }
}
