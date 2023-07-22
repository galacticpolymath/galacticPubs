#' Get front-matter values
#'
#' Imports meta/front-matter.yml value(s) to environment as a list.
#'
#' If you ask for only one key, output will be a vector, rather than a list
#'
#' @param key which entry (or entries) do you want to import? default=NULL will import everything; Supports "starts with", case-insensitive matching for a single key if prefixed with '~'
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]. The basename of this working directory will then be used to find a match in the gp-lessons git project folder by calling [get_git_gp_lessons_path()]. It's a little roundabout, but is consistent with lookups centering on the Google Drive project working directory.
#' @param WD_git default=NULL. If you already know the path to the gp-lessons folder, this is more efficient.
#' @param yaml_path default=NULL. An alternate way to just provide a full path to read in a yml file.
#' @param checkWD passed to [safe_read_yaml()]; default=FALSE; set to FALSE to suppress warnings if for example you're missing teach-it.gsheet or some other item expected to be in a lesson directory
#' @param auto_init logical; do you want to automatically create a front-matter.yml file if it's not found? Runs [init_fm()]; default=FALSE
#' @param check string referring to a check function(x) to pass to [checkmate::assert()]; e.g. check="checkmate::check_character(x,min.chars=10)" will throw an error if an output is not a string of at least 10 characters. default=NULL
#' @param always_list logical; do you want to always return a list? default=FALSE will unlist() the result if it seems to be a single item
#' @param standardize_NA logical; do you want all "",NULL,list(), etc. to be read in as NA using [is_empty()]? passed to [safe_read_yaml()]; default=TRUE
#' @param ... additional args passed to check function
#' @examples
#' get_fm()
#' get_fm(key=c("Title","ShortTitle","locale"))
#' get_fm(key="Title",WD=pick_lesson())
#' #partial matching for all front-matter entries starting with 'gdrive' (case insensitive)
#' get_fm("gdrive","?")
#' @export

get_fm <-
  function(key = NULL,
           WD = "?",
           WD_git = NULL,
           yaml_path = NULL,
           checkWD = FALSE,
           auto_init = FALSE,
           check = NULL,
           always_list = FALSE,
           standardize_NA = TRUE,
           ...) {
    #In case key isn't supplied, interpret "?" as WD
    if (!is.null(key) & identical(TRUE, key %in% c("?", "??"))) {
      WD = key
      key = NULL
    }

    #if yaml_path not supplied, look it up
    if (is.null(yaml_path)) {
      if (is.null(WD_git)) {
        #WD is for the google drive side of things (not the gp-lessons dir)
        WD <- parse_wd(WD)
        #Basename must always match b/w Google Drive & gp-lessons
        WD_git_root <- get_git_gp_lessons_path()
        WD_git <- fs::path(WD_git_root, "Lessons", basename(WD))
      }

      checkmate::assert_directory_exists(
        WD_git,
        .var.name = paste0(
          "Check for 'gp-lessons' folder matching Gdrive lesson project: '",
          basename(WD_git),
          "'"
        )
      )


      y <- safe_read_yaml(
        yaml_path = fs::path(WD_git,"front-matter.yml"),
        checkWD = checkWD,
        auto_init = auto_init,
        standardize_NA = standardize_NA
      )
    } else{
      y <- safe_read_yaml(
        yaml_path = yaml_path,
        checkWD = checkWD,
        auto_init = auto_init,
        standardize_NA = standardize_NA
      )
    }
    KEYS <- names(y)

    #output whole front-matter if no key specifically requested
    if (is.null(key)) {
      results <- y

      #otherwise check for key existence & output

    } else{
      checkmate::assert_vector(key, .var.name = "key")

      # check for partial matching prefix '~' only if length==1 -----------------
      if (length(key) == 1 & substr(key[1], 1, 1) == "~") {
        #check starts with; case insensitive, removing ~)
        matches <- startsWith(x = tolower(KEYS),
                              prefix = tolower(gsub("~", "", key[1]))) %>%
          which()

        if (length(matches) > 0) {
          #Output partial match keys if found
          results <-  y[KEYS[matches]]


          if (is_empty(results) & standardize_NA) {
            results <- NA
          }
          if (is.null(names(results))) {
            names(results) <- KEYS[matches]
          }
        } else{
          results <- NULL
          warning("\nNo partial matching results for keys using string: ",
                  key)
          warning(
            "** Fix your key string, rerun with standardize_NA=T, or try updating your front-matter with update_fm()"
          )

        }

        # Otherwise try to exact match --------------------------------------------
      } else{
        #First validate that these match keys in the file

        valid_names <- key %in% KEYS
        test_valid <- sum(valid_names) == length(key)
        if (!test_valid) {
          warning("*  If you meant to do partial key matching, add a '~' prefix.")
          warning("** Try updating your front-matter with update_fm()")
          stop("Invalid keys supplied: \n  - ",
               paste0(key[!valid_names], collapse = "\n  -"))
        }

        results <- purrr::map(1:length(key), \(i) {
          key_checks_i <- checkmate::test_choice(key[i], KEYS)
          if (key_checks_i) {
            res_i <- y[[key[i]]]
            if (is_empty(res_i) & standardize_NA) {
              res_i <- NA
            }
          } else{
            res_i <- NULL
          }

          return(res_i)
        })
        names(results) <- key
      }





    }#End logic for key provided



    # Do validation checks for output -----------------------------------------

    if (!is.null(check)) {
      results %>%  purrr::map(\(x) {
        checkmate::assert(eval(parse(text = check)),
                          .var.name = x[1])

      })

    }

    #Don't return a list unless more than one item AND contains nested list
    if (length(results) == 1 &
        !is.list(results[[1]]) & !always_list) {
      unlist(results)
    } else{
      # Output certain keys as dataframes ---------------
      #these are keys we want to handle as dataframes
      df_keys <-
        c("Versions",
          "Authors",
          "Credits",#deprecated
          "Acknowledgments",
          "GoogleCloudStorage")

      results2 <- purrr::map(1:length(results), \(i) {
          res_i <- results[[i]]
          if (!is.null(res_i) & names(results)[i] %in% df_keys) {
            res_i <- dplyr::as_tibble(res_i)
          }else{
          res_i
          }
        })
      names(results2) <- names(results)

      results2


    }
  }
