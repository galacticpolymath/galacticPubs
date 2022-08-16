#' Check for and remove project from GP Catalog
#'
#' Useful for when you rename a github repo of a lesson project and you have an orphaned catalog.galacticpolymath.com folder.
#'
#' @param gh_proj_name The (OLD) unique project title of this lesson as it is named on [https://github.com/galacticpolymath](https://github.com/galacticpolymath).
#' @returns logical; TRUE= success, FALSE= fail, NA= project not found
#' @export

gh_remove_from_GPcatalog <- function(gh_proj_name){
  #Get catalog info from latest Catalog build
  current_catalog <- jsonlite::read_json("https://catalog.galacticpolymath.com/index.json")
  current_cat_names<-sapply(1:length(current_catalog), function(i) {
    gsub("^.*/(.*)\\.git$","\\1", current_catalog[[i]]$GitHubPath)
  })
  #Is the current project in the catalog?
  proj_in_cat<-gh_proj_name %in% current_cat_names
  if(!proj_in_cat){
    message("Project '",gh_proj_name,"' not found in gp-catalog")
    out<-NA
  } else{
    #Check for virtualized Google Drive catalog repo
    catalog_path <-
      fs::path(
        "/",
        "Volumes",
        "GoogleDrive",
        "Shared drives",
        "GP-Misc-Shared",
        "GitHub_Meta-Projects",
        "gp-catalog"
      )
    catalog_lessons_path <- fs::path(catalog_path, "lessons")
    loc_cat_exists <- dir.exists(catalog_path)
    if (!loc_cat_exists) {
      warning("Directory not found!\nMake sure you have access to:\n ",
              catalog_path)
      out <- FALSE
    } else{
      #Pull latest
      test_pull <- catch_err(gert::git_pull(repo = catalog_path))
      if (!test_pull) {
        warning("Git pull unsuccessful.")
        out <- FALSE
      } else{
        #Check again that the directory in question exists
        local_proj_path<-fs::path(catalog_lessons_path,gh_proj_name)
        test_proj_is_local <- dir.exists(local_proj_path)

        if (!test_proj_is_local) {
          warning("Project not found on virtualized folder: \n ",catalog_lessons_path)
        } else{
          #if the local github project in question exists, delete it
          to_delete<-fs::dir_ls(local_proj_path,recurse = TRUE)
          test_rm<-catch_err(fs::dir_delete(local_proj_path))

          test_commit <- catch_err(gert::git_commit_all(message = paste0("Automated call to galacticPubs::gh_remove_from_GPcatalog\n*Delete orphaned/renamed gp-catalog entry: ",gh_proj_name),repo = catalog_lessons_path) )

          test_push <- catch_err( gert::git_push(repo= catalog_lessons_path))

          if(test_rm & test_commit & test_push){
            out<-TRUE
          }else{
            warning("Something doesn't look right.")
            out<-FALSE
          }
          print(dplyr::tibble(Status=convert_T_to_check(c(test_proj_is_local,test_rm,test_commit,test_push)),
                              Test=c("Project found","Deleted locally","Committed","Pushed to the Web")))
        }}

    }
  }
  out
}
