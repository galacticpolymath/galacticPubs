#' gpColors
#'
#' output a vector of hex codes, given named colors; list=TRUE lists all options
#'
#' @param colorNames a vector of quoted named colors (partial name matching is allowed)
#' @param List T/F, do you want to list the options?
#' @return a vector of hex codes for the requested colors
#' @export
gpColors<-function(colorNames=NULL,List=FALSE){
  gpPal=NULL
  utils::data(gpPal,package="GPpub",envir = environment())
  allPal.0<-do.call(rbind,gpPal)
  allPal<-subset(allPal.0,!duplicated(allPal.0$name))
  if(List==TRUE|is.null(colorNames)){
    cat("GP PALETTE OPTIONS",paste0(rep("-",20),collapse=""),
        "\n",paste(allPal[,1],collapse=", "))
    return()
    }else{
    indx=charmatch(colorNames,allPal$name)
    colVec<-allPal$hex[indx]
    names(colVec)<-allPal$name[indx]
    return(colVec)}

}
