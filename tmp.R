oauth_id <-
  "1095510414161-jo8dbgm27asec4dm9h05iqf0t18hviv2.apps.googleusercontent.com"

oauth_secret <-
  httr2::obfuscated("LJZonP3Q0vVpNm_Z9vJp25gIZYvkKdHGUOGmZ0Y5qG36A9ssZNFweIl4cI1YPQ-3KBf-")

oauth_client_obj <- httr2::oauth_client(id = oauth_id,
                                        token_url = "https://dev.galacticpolymath.com/api/get-jwt-token",
                                        secret = oauth_secret) %>% httr2::with_verbosity()

auth <-
  httr2::oauth_flow_auth_code(client = oauth_client_obj,
                              auth_url = "https://dev.galacticpolymath.com/api/auth/signin",
                              port = 3000) %>% httr2::with_verbosity()



###New endpoint (Get Token)...only works one time after running signin line...not ideal
email <- "matt@galacticpolymath.com"
token_request <-
  httr2::request("https://dev.galacticpolymath.com/api/get-jwt-token") %>%
  httr2::req_body_json(list(email = email))


token_resp <- token_request %>% httr2::req_perform(verbosity = 3)

token <- token_resp %>%
  httr2::resp_body_json() %>% unlist()

#
# httr2::request("https://www.galacticpolymath.com/api/insert-lesson") %>%
#   httr2::req_oauth_auth_code(client, auth_url = "https://dev.galacticpolymath.com/api/auth/signin", redirec) %>%
#   httr2::req_perform() %>% httr2::with_verbosity()






manual_token <-
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6Im1hdHRAZ2FsYWN0aWNwb2x5bWF0aC5jb20iLCJyb2xlcyI6WyJ1c2VyIiwiZGJBZG1pbiJdLCJuYW1lIjoiTWF0dCBXaWxraW5zIiwiZXhwIjoxNjk1NTkyMTE0LCJpYXQiOjE2OTU1MDU3MTQuMTI1LCJqdGkiOiI0RHZ1SVUyMEdCaHZrWFZMYVdkQncifQ.DJqXmaAqUjqntbpyqQwOBtiRL4ZCKzzeAN_L0GnEyws"



# update lesson -----------------------------------------------------------
payload <- list(
  filterObj = list(numID = list(as.character(6))),
  keysAndUpdatedValsObj = list(SponsorLogo = c("https://storage.googleapis.com/gp-cloud/lessons/AnimalCollective_en-US/UMass_Boston_logo.png","https://storage.googleapis.com/gp-cloud/lessons/AnimalCollective_en-US/UMass_Boston_logo.png"))
)

req0 <-
  httr2::request("https://dev.galacticpolymath.com/api/update-lessons")


req <-
  req0 %>% httr2::req_auth_bearer_token(token = token) %>%
   httr2::req_method("PUT") %>%
  httr2::req_body_json(data=payload)

  req %>% httr2::req_dry_run()


b <-
  req %>% httr2::req_perform(verbosity = 3) %>% catch_err(keep_results = T)


#alt approach with more manual construction
req <- req0 %>%
  httr2::req_auth_bearer_token(token = token) %>%
  httr2::req_method("PUT") %>%
  httr2::req_url_query(filterObj =       (jsonlite::toJSON(list(numID = (
    6
  )), auto_unbox = FALSE))) %>%
  httr2::req_url_query(keysAndUpdatedValsObj =
                         jsonlite::toJSON(list(Title = "blahblahblah"), auto_unbox = TRUE))




# Delete path -------------------------------------------------------------
lesson_id <- get_fm("_id","?")
req <-
  httr2::request("https://dev.galacticpolymath.com/api/delete-lesson") %>%
  httr2::req_auth_bearer_token(token = token) %>%
  req_method("DELETE") %>%
  req_url_query(`_id`=lesson_id)

httr2::req_dry_run(req)
httr2::req_perform(req,verbosity = 3)


gp_api_query(numID=7,c("Title","Subtitle"))

# Insert Lesson -----------------------------------------------------------
lsn_path <- fs::path(get_wd_git("?"),"LESSON.json")
lsn <- jsonlite::read_json(lsn_path)

req <-
  httr2::request("https://dev.galacticpolymath.com/api/insert-lesson") %>%
  httr2::req_auth_bearer_token(token = token) %>%
  req_method("POST") %>%
  req_body_json(data=list(lesson=lsn))

httr2::req_dry_run(req)
res <- httr2::req_perform(req,verbosity = 3)%>%
  catch_err(keep_results = TRUE)

http_code <- res$result$status



#check new value has changed
gp_api_query(c("Title", "Subtitle"), numID=6)
