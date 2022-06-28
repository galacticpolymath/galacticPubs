#' overwrite_matching
#'
#' For 2 lists, take matching values from List 1 and overwrite them on List 2. This is pretty much the list version of [hard_left_join()]
#'
#' The lists don't have to have the same dimensions. Output will look like list2 with matching elements from list1 subbed in
#'
#' @param list1 The source list (i.e. with newer data)
#' @param list2 The target list (i.e. where new data will replace old data)
#' @return List2 with matching elements of List1 overwritten over (outdated) List 2 values
#' @export

overwrite_matching<-function(list1,list2){
  matches<-names(list1)[which(names(list1) %in% names(list2))]
  out<-list2
  for(k in matches){
    out[[k]] <- list1[[k]]
  }
  out
}
