#' Get comments for a Google Drive file
#'
#' Input a google drive dribble, get a tibble of comments
#'
#' @param drive_file A google drive dribble. Should be a single file
#' @returns A tibble of comments and relevant info
#' @export
#'

drive_get_comments <- function(drive_file) {
  test_single_file <-
    googledrive::confirm_single_file(drive_file) %>% catch_err()
  if (!test_single_file) {
    warning("drive_file is either not a dribble or not a single file")
    out<-dplyr::tibble(filename = character(),
                  comm_auth=character(),
                  comment=character(),
                  mod_time=character(),
                  id = character()
                  )
  } else{
    #Create Drive API GET request
    req_comments <-
      gargle::request_build(
        path = paste0("drive/v3/files/",drive_file$id,"/comments"),
        token = googledrive::drive_token(),
        params = list(fileID = drive_file$id, fields =
                        "*")
      )
    #Request comments
    response_comments<-googledrive::do_request(req_comments,includeDeleted=FALSE)$comments

    #Make tibble of unresolved comments
    comments<-lapply(response_comments,function(li){
      #Stupidly, resolved boolean only exists in resolved comments
      if(!"resolved" %in% names(li)){
      dplyr::tibble(filename=drive_file$name,
                    comm_auth=li$author$displayName,
                    comment=li$content,
                    mod_time=li$modifiedTime,
                    id=drive_file$id
                    )
      }
    }) %>% dplyr::bind_rows()

    #Need to create my own Oauth system to validate the Google Docs API.
    #TOO Friggin difficult for right now ffs
    #
    # #Create Google Docs API GET request
    # req_revisions <-
    #   gargle::request_build(
    #     path = paste0("/v1/documents/",drive_file$id),
    #     params = list(suggestionsViewMode="SUGGESTIONS_INLINE"),
    #     token = googledrive::drive_token(),
    #     base_url = "https://docs.googleapis.com"
    #   )
    #
    # #Make sure authorization exists
    # googledrive::drive_auth(scopes="https://www.googleapis.com/auth/documents")
    #
    # #Request revisions
    # response_revisions<-googledrive::do_request(req_revisions)
    # #Make tibble of unresolved comments
    # comments<-lapply(response_revisions,function(li){
    #   #Stupidly, resolved boolean only exists in resolved comments
    #   if(!"resolved" %in% names(li)){
    #   dplyr::tibble(filename=drive_file$name,
    #                 comm_auth=li$author$displayName,
    #                 comment=li$content,
    #                 mod_time=li$modifiedTime,
    #                 id=drive_file$id
    #                 )
    #   }
    # }) %>% dplyr::bind_rows()
    out <- comments
  }
  return(out)
}
