#' gpColors
#'
#' output a vector of hex codes, given named colors; list=TRUE lists all options
#'
#' @param colorNames a vector of quoted named colors (partial name matching is allowed)
#' @param list T/F, do you want to list the options?
#' @return a vector of hex codes for the requested colors
#' @export
gpColors<-function(colorNames,list=FALSE){
  gpPal=NULL
  utils::data(gpPal)
  allPal.0<-do.call(dplyr::bind_rows,gpPal)
  allPal<-subset(allPal.0,!duplicated(allPal.0$name))
  if(list==TRUE){cat("GP PALETTE OPTIONS",paste0(rep("-",20),collapse=""),"\n",paste(allPal[,1],collapse=", "))}
  indx=charmatch(colorNames,allPal$name)
  colVec<-allPal$hex[indx]
  names(colVec)<-allPal$name[indx]
  return(colVec)
}
