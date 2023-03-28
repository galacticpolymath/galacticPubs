#' Get front-matter values
#'
#' Imports meta/front-matter.yml value(s) to environment as a list.
#'
#' If you ask for only one key, output will be a vector, rather than a list
#'
#' @param key which entry (or entries) do you want to import? default=NULL will import everything; Supports "starts with", case-insensitive matching for a single key if prefixed with '~'
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]
#' @param checkWD passed to [safe_read_yaml()]; default=TRUE; set to FALSE to suppress warnings if for example you're missing teach-it.gsheet or some other item expected to be in a lesson directory
#' @examples
#' get_fm()
#' get_fm(key=c("Title","ShortTitle","locale"))
#' get_fm(key="Title",WD=pick_lesson())
#' #partial matching for all front-matter entries starting with 'gdrive' (case insensitive)
#' get_fm("gdrive","?")
#' @export

get_fm <- function(key = NULL, WD = getwd(), checkWD=TRUE) {
  if (WD == "?") {
    WD <- pick_lesson()
  }

  y <- safe_read_yaml(WD=WD,checkWD=checkWD)
  KEYS <- names(y)

  #output whole front-matter if no key specifically requested
  if (is.null(key)) {
    results <- y
    #otherwise check for key existence & output

  } else{

    #check for partial matching prefix '~' only if length==1
    if (length(key) == 1 & substr(key[1], 1, 1) == "~") {
      #check starts with; case insensitive, removing ~)
      pmatches <- startsWith(x = tolower(KEYS),
                             prefix = tolower(gsub("~", "", key[1]))) %>%
        which()

      if (length(pmatches) > 0) {
        #Output partial match keys if found
        results0 <-  y[KEYS[pmatches]]
      } else{
        results0 <- NULL
        warning("\nNo partial matching results for keys using string: ",key)
      warning("** Fix your key string or try updating your front-matter with update_fm()")

      }
    #Otherwise try to exact match
    } else{
      results0 <- purrr::map(1:length(key), \(i) {
        key_checks_i <- checkmate::test_choice(key[i], KEYS)
        if (key_checks_i) {
          y[[key[i]]]
        } else{
          NULL
        }

      })
      names(results0) <- key
    }


    #remove result items where no key found
    results <-
      results0[lengths(results0) > 0]
    missings <-
      which(lengths(results0) == 0)
    if (length(missings) > 0) {
      warning("\nKeys not found:\n -",
              paste0("'", key[missings], "'", collapse = "\n -"))
      warning("*  If you meant to do partial key matching, add a '~' prefix.")
      warning("** Try updating your front-matter with update_fm()")
    }

  }#End logic for key provided

  #Don't return a list unless more than one item returned
  if (length(results) <= 1) {
    unlist(results)
  } else{
    results
  }
}
