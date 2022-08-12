#' learningEpaulette
#'
#' Create a Galactic Polymath Learning Epaulette which is a special kind of mosaic plot showing proportion of lesson by subject. Outputs a horizontal and a vertical version for mobile
#' @param heightScalar for horizontal epaulette, multiplier for image height which affects amount of padding between label text and epaulette; default=1
#' @param epauletteHeight relative size of the epaulette; default=0.2
#' @param randomSeed random number for getting slightly different (but repeatable) repelled text labels
#' @param saveFile T/F, save file or just print to screen?
#' @param destFolder where do you want to save the folder; by default in the "assets/_learning-plots" folder, 1 level up from the working directory
#' @param fileName expects "somefilename" for ggsave output image file
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param font_size size of font in pts;default=11
#' @param width plot width in inches
#' @param height plot height in inches
#' @param dpi resolution in dots per inch; by default 150
#' @param showPlot plot to screen or just save to file? default=T
#' @param ... additional parameters for \code{\link[ggplot2]{ggsave}}
#'
#' @return returned plot as a ggplot object; plot saved to assets/GP_Learning_Epaulette.png by default
#' @importFrom rlang .data
#' @export
#########################################
### GP Learning Mosaic Plot/Epaulet graphic

learningEpaulette<-function(heightScalar=1,epauletteHeight=0.2,randomSeed=101,saveFile=TRUE,destFolder,fileName="GP-Learning-Epaulette",WD=getwd(),font_size=10,width=11,height=1.6,dpi=200,showPlot=TRUE,...){

  if(missing(destFolder)){destFolder<-fs::path("assets","_learning-plots")}
  # #test for NULL values being supplied; replace with defaults
  # a<-match.call(expand.dots=TRUE)[-1] %>% as.list()
  #
  # browser()

  #if WD supplied that is not getwd(), append it to destFolder
  if(!identical(WD,getwd())){destFolder<-fs::path(WD,destFolder)}


# Standards exist?
standardsFile<-fs::path(WD,"meta","standards.RDS")
standardsFound <- file.exists(standardsFile)

###########
# Do all this only if compiled standards found ----------------------------

if(!standardsFound){warning("Compiled Standards not found at: ",standardsFile)
  }else{

# Define important variables ----------------------------------------------
    # Load compiled standards
    importedData<-readRDS(standardsFile)
    targetSubj<-importedData$targetSubj
    rectangles<-importedData$rectangles
    xlabels<-importedData$xlabels
    clrs<-as.vector(gpColors(c("math","ela","science","socstudies")))

# #install compact font
# sysfonts::font_add_google(name="Kanit",regular.wt=400,bold.wt=600)
# showtext::showtext_auto()
## PLOT Epaulette
epaulette<-
  ggplot2::ggplot(rectangles)+
  ggplot2::geom_rect(ggplot2::aes_string(xmin="xmin",xmax="xmax",ymin="ymin",ymax="ymax",fill="subject"),size=1.2,show.legend = F)+
  ggplot2::scale_colour_manual(values=clrs,aesthetics=c("color","fill"))+
  #Add Target border(s) if necessary
  ggplot2::geom_rect(ggplot2::aes_string(xmin="xmin",xmax="xmax",ymin="ymin",ymax="ymax"),fill="transparent",colour=rectangles$border,size=2.3,show.legend = F)+
  ggplot2::scale_x_continuous(limits = c(0,1),expand=ggplot2::expansion(c(0,0),0))+
  ggplot2::theme_void()+
    ggplot2::theme(plot.background=ggplot2::element_blank(),panel.background = ggplot2::element_blank())


subject_labels<-subset(xlabels,xlabels$x.prop>0) %>% ggplot2::ggplot()+
  galacticEdTools::theme_galactic(font="Kanit")+
  ggrepel::geom_text_repel(ggplot2::aes_string(x="x",y=.2,label="lab",fontface="fontface",segment.size="stroke"),
                           seed=randomSeed,
                           family="Kanit",size=font_size,show.legend = FALSE,direction="both",col=gpColors("galactic black"),force=6,
                           point.padding=0.25)+
  #scale y to pad text according to user input
  ggplot2::scale_y_continuous(limits=c(0,0.2),expand=ggplot2::expansion(c(0,0),0))+
  ggplot2::scale_x_continuous(limits = c(0,1),expand=ggplot2::expansion(c(0,0),0))+
   ggplot2::theme_void()+
    ggplot2::theme(plot.background=ggplot2::element_blank(),panel.background = ggplot2::element_blank())

###
# HORIZONTAL EPAULETTE + LABELS
# calculate relative label height; we need to scale the relative proportion of label with the overall image height
ep_abs_height<-epauletteHeight*height #Target epaulette height in inches (must stay constant)
scaled_height<-(height*heightScalar) #New height in inches
relEpauletteHeight<-(ep_abs_height)/scaled_height
relLabHeight <- 1-relEpauletteHeight

G <- epaulette / subject_labels  + patchwork::plot_layout(ncol = 1, heights = c(relEpauletteHeight, relLabHeight),widths=c(1,1)) &
    ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.margin=ggplot2::unit(c(0,0,5,0),"points")
    )




epaulette_vert<-  ggplot2::ggplot(rectangles)+
  ggplot2::geom_rect(ggplot2::aes_string(xmin="xmin",xmax="xmax",ymin="ymin",ymax="ymax",fill="subject"),size=1.2,show.legend = F)+
  ggplot2::scale_colour_manual(values=clrs,aesthetics=c("color","fill"))+
  #Add Target border(s) if necessary
  ggplot2::geom_rect(ggplot2::aes_string(xmin="xmin",xmax="xmax",ymin="ymin",ymax="ymax"),fill="transparent",colour=rectangles$border,size=2.3,show.legend = F)+
  ggplot2::scale_x_continuous(limits = c(0,1),expand=ggplot2::expansion(c(0.001,0.001)))+
  ggplot2::scale_y_continuous(expand=ggplot2::expansion(c(0.1,0.1)))+
  ggplot2::theme_void()+
    ggplot2::theme(plot.background=ggplot2::element_blank(),panel.background = ggplot2::element_blank())+
ggplot2::coord_flip()#+ggplot2::theme_bw()

xlabels$vertLabStart <- cumsum(xlabels$x.prop)

subject_labels_vert <- subset(xlabels,xlabels$x.prop>0) %>% ggplot2::ggplot()+
  galacticEdTools::theme_galactic(font="Kanit")+
  ggplot2::geom_text(
    ggplot2::aes_string(
      x = .1,
      y = "vertLabStart-.01",
      label = "subject",
      fontface = "fontface"
    ),
    hjust = 0,
    vjust = 1.2,
    size = font_size,
    family="Kanit",
    show.legend = FALSE,
    col = gpColors("galactic black")
  )+
  ggplot2::geom_segment(ggplot2::aes_string(x=.001,xend=2,y="vertLabStart",yend="vertLabStart"))+
  ggplot2::scale_x_continuous(expand=ggplot2::expansion(c(0,0)))+
  ggplot2::scale_y_continuous(limits = c(0,1),expand=ggplot2::expansion(c(0.001,0.001)))+
   ggplot2::theme_void()

###
# VERTICAL EPAULETTE + LABELS
G_vert <- epaulette_vert+subject_labels_vert+patchwork::plot_layout(ncol=2,widths=c(0.2,0.8)) &
    ggplot2::theme(plot.background=ggplot2::element_blank(),panel.background = ggplot2::element_blank())
#make a vertical version of this

if(showPlot){plot(G)}

#create folder if necessary
dir.create(destFolder,showWarnings=FALSE, recursive=TRUE)


givenExt=ifelse(grepl(".",fileName,fixed=TRUE),gsub(".*\\.(.{3,4}$)","\\1",fileName),NA) #extract file extension if provided
fileOut<-gsub("(^.*)\\..*$","\\1",basename(fileName)) #strip extension and full path from provided fileName

fileOut_vert<-paste0(fileOut,"_vert")

fileOutExt<-ifelse(is.na(givenExt),"png",givenExt) #provide png extension if not provided
output<-fs::path(destFolder,"/",fileOut,ext=fileOutExt)
output_vert <- fs::path(destFolder,"/",fileOut_vert,ext=fileOutExt)


#save the file
ggplot2::ggsave(filename=basename(output),plot=G,path=fs::path_dir(output),width=width,height=height*heightScalar,dpi=dpi,bg="transparent")
 # ,
#                 ...)
#save vertical version
ggplot2::ggsave(filename=basename(output_vert),plot=G_vert,path=fs::path_dir(output_vert),width=height*1,height=width*.6,dpi=dpi,bg="transparent")
#,
 #               ...)


#output object if they want to modify further
message("GP Learning Epaulette saved\n@ ",output)
prop<-rectangles$proportion
names(prop)<-rectangles$subject
return(invisible(list(horiz=G,vert=G_vert,proportions=prop)))
}
}
