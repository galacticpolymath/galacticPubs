#' learningEpaulette
#'
#' Create a Galactic Polymath Learning Epaulette which is a special kind of mosaic plot showing proportion of lesson by subject
#' @param compiledAlignment the output of \code{\link{compileAlignment}}
#' @param targetSubj which subject(s) is (are) the focus of the lesson? opts= "math","ela","science","social studies"
#' @param vertSpacing 4 value vector ranging from 0 to 1 for manipulating label spacing
#' @param fileName expects somefilename.png for ggsave output image file
#' @param ... additional parameters for \code{\link[ggplot2]{ggsave}}
#' @return returned plot as a ggplot object; plot saved to assets/GP_Learning_Epaulette.png by default
#' @importFrom rlang .data
#' @export
#########################################
### GP Learning Mosaic Plot/Epaulet graphic

learningEpaulette<-function(compiledAlignment,targetSubj=NULL,vertSpacing=c(1,1,1,1),fileName="assets/GP_Learning_Epaulette.png",...){

#bring in empty matrix to merge in, in case some subjects are missing
a_template <-  readRDS(system.file("emptyStandardsCountForAllDims.rds",package="GPpub"))
#super important to refactor subject on the imported data to ensure order
a_template$subject=factor(a_template$subject,levels=c("Math","ELA","Science","Social Studies"),ordered=T)

a_summ<-compiledAlignment %>% dplyr::group_by(.data$subject,.data$dimension) %>% dplyr::tally()

#gotta combine missing rows, sort, & repeat the entries N times
a_combined<-dplyr::anti_join(a_template,a_summ,by="dimension") %>% dplyr::bind_rows(a_summ) %>% dplyr::arrange(.data$subject,.data$dimension)%>% dplyr::mutate(binary=ifelse(.data$n>0,1,0))

#Account for bias in the number of standards
bias<-readRDS(system.file("standardCountsByDimension.rds",package="GPpub"))
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
xlabels<-sapply(proportions$proportion,scales::percent) %>% dplyr::as_tibble()
xlabels$x.prop=(proportions$proportion)
xlabels$x=cumsum(proportions$proportion)-(proportions$proportion/2)
#make slight correction for labeling purposes
xlabels$x.lab <-xlabels$x + c(.01,.01,-.01,-.01)


#set manual vertical spacing, otherwise distribute as steps (from 0 to 1 (0 being lowest, 1, being highest))
#convert relative label 0, 1 scale to plot 0,1 scale units
if(!is.null(vertSpacing)){
  vertSpacingFormatted =vertSpacing*.35
  }else{vertSpacingFormatted =seq(.35,0,length.out=4)}


xlabels$yend=vertSpacingFormatted
xlabels$subj<-c("math","ela","science","socstudies")
xlabels$subject<-c("Math","ELA","Science","Soc. Studies")
xlabels$lab<-paste(t(xlabels$value),t(xlabels$subject))
xlabels$hjust<-.5#c(0,0,1,1)
xlabels$fontface<-"plain"
xlabels$stroke<-1
xlabels$strokeCol<-clrs
xlabels$size<-9





segs<-dplyr::tibble(x=xlabels$x,xend=xlabels$x,y=.59,yend=xlabels$yend,subject=xlabels$subject,segCol=clrs)


rectangles<-dplyr::tibble(xmin=c(0,cumsum(proportions$proportion)[-4]),xmax=cumsum(proportions$proportion),ymin=.6,ymax=1,subject=c("Math","ELA","Science","Soc. Studies"))
rectangles$subject<-factor(rectangles$subject,ordered=T,levels=c("Math","ELA","Science","Soc. Studies"))
rectangles$border<-"transparent"

#boldenize & embiggenate if targetSubj indicated
if(!is.null(targetSubj)){
  (targetRows<-which(!is.na(charmatch(tolower(xlabels$subj),tolower(targetSubj))) ))
  segs$segCol[targetRows]<-gpColors("galactic black")
  xlabels$stroke[targetRows]<-4
  xlabels$strokeCol[targetRows] <- gpColors("galactic black")
  xlabels$fontface[targetRows]<-"bold"
  xlabels$size[targetRows]<-11
  rectangles$border[targetRows]<-gpColors("galactic black")
}


## PLOT Epaulette
epaulette<-ggplot2::ggplot(rectangles)+ggGalactic()+
  ggplot2::geom_rect(ggplot2::aes_string(xmin="xmin",xmax="xmax",ymin="ymin",ymax="ymax",fill="subject"),size=1.2,show.legend = F)+
  #Add Target border(s) if necessary
  ggplot2::geom_rect(ggplot2::aes_string(xmin="xmin",xmax="xmax",ymin="ymin",ymax="ymax"),fill="transparent",colour=rectangles$border,size=2.3,show.legend = F)+
  ggplot2::scale_x_continuous(limits = c(-.01,1.01))+ggplot2::scale_y_continuous(limits=c(-.1,1.01))+
  ggplot2::geom_segment(data=segs,ggplot2::aes_string(x="x",xend="xend",y="y",yend="yend"),col=segs$segCol,size=3,
                        inherit.aes=F,show.legend = F)+
  ggplot2::scale_colour_manual(values=clrs,aesthetics=c("color","fill"))+
  ggplot2::geom_point(data=xlabels,ggplot2::aes_string(x="x",y="yend",fill="subject"),stroke=xlabels$stroke,col=xlabels$strokeCol,
             size=xlabels$size,pch=21,show.legend = F)+
  ggplot2::geom_label(data=xlabels,ggplot2::aes_string(x="x.lab",y="yend",label="lab",hjust="hjust"),colour="white",fill="white",size=7,show.legend = F,nudge_y=-.3)+
  ggplot2::geom_text(data=xlabels,ggplot2::aes_string(x="x.lab",y="yend",label="lab",hjust="hjust",fontface="fontface"),size=7,show.legend = F,nudge_y=-.3,col=gpColors("galactic black"))+
  ggGalactic()+ ggplot2::theme_void()

#output to user
plot(epaulette)
#Save the file
ggplot2::ggsave(fileName,width=10,height=1.6,...)
#output object if they want to modify further
return(epaulette)

}
