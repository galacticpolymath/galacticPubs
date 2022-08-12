#' gpsave
#'
#' Wrapper for [ggplot2::ggsave()] with galacticPubs defaults
#'
#' Most important thing it does is default to saving in the assets/R_outputs/ folder and save with reasonable dimensions that maintain large, readable text size of graph labels
#'
#' @param filename expects filename, with file extension (e.g. "plot.png"); can also include subfolder (e.g. "newfolder/plot.png")
#' @param width plot width in inches (default= 7)
#' @param height plot height in inches (default= 3.94, i.e. 16:9 aspect ratio)
#' @param dpi dots per inch resolution (default= 300); note changing this will unfortunately also change the output relative text size
#' @param bg background color (default= "transparent")
#' @param ... other parameters from [ggplot2::ggsave()]
#' @export

gpsave<-function(filename,width=7, height=3.94,dpi=300,bg="transparent",  ...){
  fn<-fs::path(getwd(),"assets","~R_outputs",filename)
  ggplot2::ggsave(filename=fn,width=width,height=height,dpi=dpi,...)
  message("@Saved: ",fn)
}
