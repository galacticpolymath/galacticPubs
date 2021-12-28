#' learningEpaulette
#'
#' Create a Galactic Polymath Learning Epaulette which is a special kind of mosaic plot showing proportion of lesson by subject
#' @param compiledAlignment the output of \code{\link{compileStandards}}
#' @param targetSubj which subject`(`s`)` is `(`are`)` the focus of the lesson? opts= "math","ela","science","social studies"
#' @param heightScalar for horizontal epaulette, multiplier for height which affects amount of padding between label text and epaulette; default=1
#' @param randomSeed random number for getting slightly different (but repeatable) repelled text labels
#' @param saveFile T/F, save file or just print to screen?
#' @param destFolder where do you want to save the folder; by default in the "assets/learningPlots" folder, 1 level up from the working directory
#' @param fileName expects "somefilename" for ggsave output image file
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param font_size size of font in pts
#' @param thickness is how thick to make the epaulette bar `(`range from 0 to 0.5`)`, 0.2 or 20\% of vertical plot space by default
#' @param width plot width in inches
#' @param height plot height in inches
#' @param dpi resolution in dots per inch; by default 150
#' @param ... additional parameters for \code{\link[ggplot2]{ggsave}}
#'
#' @return returned plot as a ggplot object; plot saved to assets/GP_Learning_Epaulette.png by default
#' @importFrom rlang .data
#' @export
#########################################
### GP Learning Mosaic Plot/Epaulet graphic

learningEpaulette<-function(compiledAlignment,targetSubj=NULL,heightScalar=1,randomSeed=101,saveFile=TRUE,destFolder="assets/learning-plots/",fileName="GP-Learning-Epaulette",WD=getwd(),font_size=19,thickness=0.2,width=11,height=1.6,dpi=200,...){

  #if WD supplied, append it to destFolder
  if(!identical(WD,getwd())){destFolder<-paste0(WD,destFolder)}

#bring in empty matrix to merge in, in case some subjects are missing
a_template <-  readRDS(system.file("emptyStandardsCountForAllDims.rds",package="galacticPubs"))
#super important to refactor subject on the imported data to ensure order
a_template$subject=factor(a_template$subject,levels=c("Math","ELA","Science","Social Studies"),ordered=T)

a_summ<-compiledAlignment$compiled %>% dplyr::group_by(.data$subject,.data$dimension) %>% dplyr::tally()

#gotta combine missing rows, sort, & repeat the entries N times
a_combined<-dplyr::anti_join(a_template,a_summ,by="dimension") %>% dplyr::bind_rows(a_summ) %>% dplyr::arrange(.data$subject,.data$dimension)%>% dplyr::mutate(binary=ifelse(.data$n>0,1,0))

#Account for bias in the number of standards
bias<-readRDS(system.file("standardCountsByDimension.rds",package="galacticPubs"))
bias_by_subj<-bias %>% dplyr::summarise(tot_n_subj=sum(.data$n),.groups="drop")
a_combined<-dplyr::left_join(a_combined, (bias %>% dplyr::rename("tot_n_dim"="n")),by = c("subject", "dimension") )
a_combined<-dplyr::left_join(a_combined,bias_by_subj,by = c("subject"))

#correct the lesson's n standards by Tot possible for the subject
#*Because there aren't an equal number of standards per dimension, (and they're not all equal),
#*It's more intuitive to treat them as if they are all equal.
#*So to make the correction, we'll weight the proportions by total N for subject
a_combined$n_adj<-a_combined$n/a_combined$tot_n_subj
a_combined$n_prop<-a_combined$n/sum(a_combined$n)
a_combined$n_prop_adj<-a_combined$n_adj/sum(a_combined$n_adj)

#Remind r that a_combined factors are ORDERED
a_combined$subject <- factor(a_combined$subject,levels=c("Math","ELA","Science","Social Studies"),ordered=T)

# xlabels<-xlabels %>% arrange(subj)
clrs<-gpColors(c("math","ela","science","socstudies")) %>% as.character()
  #c("ELA"="#d64325","Math"="#d6b824","Science"="#62d6aa","Soc. Studies"="#6812d1")

#Calculate corrected proportions
proportions=a_combined  %>% dplyr::group_by(.data$subject)%>% dplyr::summarise(proportion=round(sum(.data$n_prop_adj),2),.groups="drop")

# dummy proportions

proportions$proportion<-c(.7,.1,.1,0.1)

xlabels<-sapply(proportions$proportion,scales::percent) %>% dplyr::as_tibble()
xlabels$x.prop=(proportions$proportion)
xlabels$x=cumsum(proportions$proportion)-(proportions$proportion/2)




xlabels$subj<-c("math","ela","science","socstudies")
xlabels$subject<-c("Math","ELA","Sci","SocStd")
xlabels$lab<-paste(t(xlabels$value),t(xlabels$subject))
xlabels$hjust<-.5#c(0,0,1,1)
xlabels$fontface<-"plain"
xlabels$stroke<-1
xlabels$strokeCol<-clrs
xlabels$lightCol<-c("#fdebe8","#fef6ed","#f8f5fe","#efebf6")
xlabels$size<-9


xlabels$y<-0.6





rectangles<-dplyr::tibble(proportion=proportions$proportion,xmin=c(0,cumsum(proportions$proportion)[-4]),xmax=cumsum(proportions$proportion),ymin=1-thickness,ymax=1,subject=c("Math","ELA","Science","Soc. Studies")) %>% dplyr::filter(.data$proportion>0)
rectangles$subject<-factor(rectangles$subject,ordered=T,levels=c("Math","ELA","Science","Soc. Studies"))
rectangles$border<-"transparent"

# segs<-dplyr::tibble(x=xlabels$x,xend=xlabels$x,y=1-thickness-0.04,yend=xlabels$yend,subject=xlabels$subject,segCol=clrs,targetSegCol=NA)

#boldenize & embiggenate if targetSubj indicated
if(!is.null(targetSubj)){
  (targetRows<-which(!is.na(charmatch(tolower(xlabels$subj),tolower(targetSubj))) ))
  xlabels$stroke[targetRows]<-2
  xlabels$strokeCol[targetRows] <- gpColors("galactic black")
  xlabels$fontface[targetRows]<-"bold"
  xlabels$size[targetRows]<-11
  rectangles$border[targetRows]<- gpColors("galactic black")
}


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
  ggplot2::scale_x_continuous(limits = c(0,1),expand=ggplot2::expansion(c(0,0)))+
  ggplot2::theme_void()+
    ggplot2::theme(plot.background=ggplot2::element_blank(),panel.background = ggplot2::element_blank())


subject_labels<-subset(xlabels,xlabels$x.prop>0) %>% ggplot2::ggplot()+
  galacticEdTools::theme_galactic(font="Kanit")+
  ggrepel::geom_text_repel(ggplot2::aes_string(x="x",y=.2,label="lab",fontface="fontface",
                                               segment.size="stroke"),
                           seed=randomSeed,
                           family="Kanit",size=font_size,show.legend = FALSE,direction="y",col=gpColors("galactic black"),force=10,
                           box.padding=0.25,point.padding=0.25)+
  #scale y to pad text according to user input
  ggplot2::scale_y_continuous(limits=c(0,0.2),expand=ggplot2::expansion(c(0,0)))+
  ggplot2::scale_x_continuous(limits = c(0,1),expand=ggplot2::expansion(c(0,0)))+
   ggplot2::theme_void()+
    ggplot2::theme(plot.background=ggplot2::element_blank(),panel.background = ggplot2::element_blank())

###
# HORIZONTAL EPAULETTE + LABELS
(G <- epaulette / subject_labels  + patchwork::plot_layout(ncol = 1, heights = c(0.3, 0.7),widths=c(1,1)) &
    ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank()
    )
  )

browser()





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
(G_vert <- epaulette_vert+subject_labels_vert+patchwork::plot_layout(ncol=2,widths=c(0.2,0.8)) &
    ggplot2::theme(plot.background=ggplot2::element_blank(),panel.background = ggplot2::element_blank()))
#make a vertical version of this


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
return(list(plot=G,proportions=proportions))

}
