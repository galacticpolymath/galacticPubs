#' add_missing_fields
#'
#' Internal function for [update_fm()]. Adds missing list items to a list (list_obj), based on a template
#'
#' @param list_obj the list (for our purposes, probably your front-matter.yml after being read in)
#' @param template the list template to compare to
#' @param reorder do you want to reorder the resulting list, based on template order? default=F
#' @return list_obj, with any missing fields added and reordered by template if requested
#' @export

add_missing_fields<-function(list_obj, template,reorder=FALSE) {

  #Add missing level 1 params
  #workhorse function
  addMissing <- function(l, t) {
    #If template sublevels added, give l a NA name, so NA items can be matched (preventing duplication of NAs)
    if (length(names(t)) > 1 & length(names(l)) == 0) {
      names(l) <- NA
    }
    #which template names are missing from list

    missing <-
      match(names(t), names(l)) %>% is.na() %>% which()
    names(t)[missing]
    new <- l
    if (sum(missing) > 0) {
      for (i in missing) {
        #put at front if it's the first in the template
        if (i == 1) {
          new <- c(t[i], new)
          #put it at the end if it's at the end of the template
        }else if(i==length(t)){
          new <- c(new,t[i])

          #otherwise put it in the middle of the vector
        } else{
          new <- c(new[1:(min(i - 1,length(new)-1))], t[i], new[min(length(new),i):length(new)])
        }
      }
    }

    # names(new) <- names(t)
    return(new)
  }

  #Add missing level 1 params
  L1<-addMissing(list_obj,template)
  #Add missing level 2 params
  L2<-lapply(names(L1),function(i){
    addMissing(L1[[i]],template[[i]])
  })
  names(L2)<-names(L1)

  #Reorder new with preference for template order, but preserving new additions
  #(suprisingly hard)

  if(reorder) {
    A<-names(template)
    B<-names(L2)
    A_matching <- A[which(A %in% B)]

    C <- rep(NA, length(B)) #ordered vector to be constructed
    for (i in 1:length(B)) {
      BmatchA <- match(B[i], A)
      # special case...always put last template item after the penultimate template item (to preserve favored order)
      if (length(A_matching) == 1) {
        C[i] <- A_matching[1]
        #insert dummy placeholder
        B <- c(B[1:(i - 1)], "REPLACED DUMMY VALUE", B[i:length(C)])
        #remove the last remaining B val found in A, since we're moving it
        B <- B[-max(which(B %in% A))]
        A_matching <- A_matching[-1]
        #if no match in template, go with B[i] & move on
      } else if (is.na(BmatchA)) {
        C[i] <- B[i]
        #if it's the last matching item, put it next and remove the old match

      } else{
        # if
        C[i] <- A_matching[1]
        A_matching <- A_matching[-1]
      }
    }
    out<-L2[C]

  } else{
    out <- L2
  }

  out

}
