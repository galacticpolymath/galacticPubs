#' show_gpPal
#'
#' Show the GP palette
#'
#' @param pal which palette? 1=6 color basic, 2= extended, 3= subject palette
#' @param subset the first n colors to show or x1:x2 or c(1,3,5) for a specific subset pal is 1 by default
#' @param ... additional parameters for \code{\link[colorspace]{swatchplot}}
#' @export
show_gpPal<-function(pal=1,subset=NULL,...){
  gpPal=NULL
  utils::data(gpPal,package="galacticPubs",envir = environment())
  P<-gpPal[[pal]]$hex
  if(length(subset!=0)){
    s <- if(length(subset)==1){eval(quote(1:subset))}else{eval(quote(subset))}
    labs<-subset
    }else{
      s=1:length(P)
      labs<-1:length(P)}
  nColors<-length(s)
  Pnames<-gpPal[[pal]]$name
  Ptitle<-names(gpPal)[pal]


  colorspace::swatchplot(P[s],...)
  whereAtStart<-1/nColors/2
  whereAt<-seq(whereAtStart,1-whereAtStart,1/nColors)
  graphics::mtext(labs,side=1,line=0,at=whereAt)
  graphics::mtext(paste0("palette = ",Ptitle),side=3,line=2,0,font=1)
  }
