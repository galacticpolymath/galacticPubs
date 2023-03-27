#' Get front-matter values
#'
#' Imports meta/front-matter.yml value(s) to environment as a list.
#'
#' If you ask for only one key, output will be a vector, rather than a list
#'
#' @param key which entry (or entries) do you want to import? default=NULL will import everything
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]
#' @examples
#' get_fm()
#' get_fm(key=c("Title","ShortTitle","locale"))
#' get_fm(key="Title",WD=pick_lesson())
#' @export

get_fm <- function(key = NULL, WD = getwd()) {
  if(WD=="?"){WD <- pick_lesson()}
  y <- safe_read_yaml(fs::path(WD, "meta", "front-matter.yml"))
  KEYS <- names(y)

  #output whole front-matter if no key specifically requested
  if (is.null(key)) {
    results <- y
    #otherwise check for key existence & output
  } else{
    key_checks <- results <- rep(NA, length(key))
    for (i in 1:length(key)) {
      key_i  <-  key[i]
      key_checks[i] <- checkmate::test_choice(key_i, KEYS)
      if (key_checks[i]) {
        results[i] <- y[key_i]
      }
    }

    names(results) <- key
    #remove result items where no key found
    results <- results[key_checks]

    if (sum(!key_checks) > 0) {
      warning("\nKeys not found:\n -",
              paste0("'", key[which(!key_checks)], "'", collapse = "\n -"))
      warning("*Try updating your front-matter with update_fm()")
    }

  }
  if (length(results) == 1) {
    unlist(results)
  } else{
    results
  }
}
