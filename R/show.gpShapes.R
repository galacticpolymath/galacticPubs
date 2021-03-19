#' show.gpShapes
#'
#' Show the default (preferred) GP shapes, which are different from the default ggplot shapes
#'
#'
show.gpShapes<-function(){
   gpShapes=NULL
  utils::data(gpShapes,package="g",envir = environment())
plot(0:length(gpShapes),0:length(gpShapes),pch=gpShapes,cex=2,xlim=c(-1,length(gpShapes)+1),ylim=c(-1,length(gpShapes)+1))
}
