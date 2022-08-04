#' learningChart
#'
#' Make a GP Learning Chart
#'
#' @param caption quoted text you want to go at the bottom of the chart
#' @param captionN T/F, add the range of the number of standards per grade used to make the plot to caption?
#' @param centralText specify grades the chart is for; by default pulls most common gradeBand from compiledAlignment (e.g. "grades`\\n`5-6")
#' @param centralTextSize multiplier for font size of centralText
#' @param quotedTitle the quoted title used to attribute the learning chart (e.g. Knowledge and skills taught by 'quotedTitle')
#' @param saveFile T/F, save file or just print to screen?
#' @param destFolder where do you want to save the folder; by default in the "assets/learningPlots" folder, 1 level up from the working directory
#' @param fileName expects "somefilename" (file extension will be ignored)
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param dpi what resolution would you like the output in dots per inch? 300 by default
#' @param showPlot plot to screen or just save to file? default=T
#' @param ... other arguments passed to \code{\link[grDevices]{png}}
#' @return the learning chart plot object (grid format); the file is saved to assets/GP_Learning_Chart.png by default
#' @export
#' @importFrom rlang .data


learningChart=function(caption=NA,
                       captionN = TRUE,
                       centralText=NA,
                       quotedTitle=NA,
                       centralTextSize = 3.7,
                       saveFile = TRUE,
                       destFolder = "assets/learning-plots/",
                       fileName = "GP-Learning-Chart",
                       WD = getwd(),
                       dpi = 200,
                       showPlot=TRUE,
                       ...){

#if WD supplied, append it to destFolder
if(!identical(WD,getwd())){destFolder<-fs::path(WD,destFolder)}

if(is.na(quotedTitle)){quotedTitle<-"this lesson"}else{quotedTitle<-paste0("\"",quotedTitle,"\"")}
#deal with missing caption and add sample size if requested
if(is.na(caption)){caption=paste0("GP Learning Chart: Knowledge & skills taught in ",quotedTitle)}

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
    compiledAlignment<-importedData$data
    a_combined<-importedData$a_combined
    targetSubj<-importedData$targetSubj


if(captionN){

  avgN<-table(compiledAlignment$compiled$gradeBand) %>% as.vector() %>% mean(na.rm=T)
  caption <- paste0(caption," (~",floor(avgN)," standards per grade band)")
  }

if(is.na(centralText)){
  t_gradeBands<-compiledAlignment$compiled$gradeBand %>% table
  centralText<-paste0("grades\n",names(t_gradeBands)[which.max(t_gradeBands)])
}



subjPal<-gpColors(c("math","ela","science","socstudies"))


# Make a proportional Learning Chart --------------------------------------

#val for scale of the biggest ray
barScale<- max(a_combined$n_prop_adj)

#function for putting things a little beyond the max value in the dataset
smidge<-function(amt=1){
  barScale+(amt*barScale/10)
}

label_data2 <- a_combined
label_data2$hjust<-ifelse( 360 * (label_data2$id-0.5) /nrow(label_data2)<180 , 0, 1)
label_data2$y<-smidge(2)
label_data2$y[c(1,12)]<-smidge(3)

#make background rectangles for each set of dimensions
bgRec2<-dplyr::tibble(subject=c("math","ela","science","social studies"),xmin=seq(0.5,9.5,3),xmax=seq(3.5,12.5,3),ymin=rep(0,4),ymax=barScale,fill=subjPal)

targetRows<-which(bgRec2$subject%in%tolower(targetSubj))
outerFill<-bgRec2[targetRows,]
outerFill$ymin<-smidge(.1)
outerFill$ymax<-10

#make the badge!
(badge_prop0<-ggplot2::ggplot(a_combined,ggplot2::aes_string(x="as.factor(id)",y="n_prop_adj",fill="subject"),
                              col=gpColors("galactic black"))+
    galacticEdTools::theme_galactic()+
    ggplot2::theme(plot.margin = ggplot2::margin(t=0,r=0,b=-15,l=-20),# not sure why this correction is necessary, but without it, the plot is not centered
          #axis.line.y=element_line(colour="grey"),
          axis.ticks.y=ggplot2::element_blank(),
          panel.grid.major.y=ggplot2::element_line(size=.2,colour="grey"),
          panel.grid.minor.y=ggplot2::element_line(size=1.5,colour="grey"),
          panel.grid.major.x=ggplot2::element_line(size=.2,colour="grey"),
          axis.text.y=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),#element_text(margin=margin(t=10,b=10,l=0,r=0,unit="pt"),hjust=1),
          axis.title=ggplot2::element_blank(),#element_text(margin = margin(0)),
          axis.ticks.length=ggplot2::unit(0,"pt"),
          panel.spacing=ggplot2::unit(0,"pt"),
          panel.border=ggplot2::element_blank(),
          panel.background=ggplot2::element_rect(fill="white"),
          plot.background=ggplot2::element_rect(fill="white"),
          legend.background=ggplot2::element_rect(fill="transparent"),
          legend.position="none",
          legend.margin=ggplot2::margin(0),
          legend.box.margin=ggplot2::margin(0),
          legend.spacing.y=ggplot2::margin(0)
          # legend.box="vertical",
          # legend.box.spacing=unit(0,"npc"),
          # legend.margin=margin(0,0,0,0),
          # legend.title=element_text(face="bold")
          )  +
    ggplot2::geom_bar(stat="identity",col=gpColors("galactic black"),alpha=.9,position="stack")+
    ggplot2::scale_fill_manual(values=as.vector(subjPal))+
    ggplot2::scale_y_continuous(expand=c(0,0),breaks=seq(0, barScale,.1),limits=c(-.1, smidge(4)))+
    #cover outside circle crap with white box
    ggplot2::geom_rect(data=NULL,xmin=-Inf,xmax=Inf,ymin=smidge(.1),ymax=2,fill="white",col="transparent",inherit.aes=F)
  )#End badge_prop0

#Make target rectangle(s) where necessary
#because of a stupid clipping thing with aesthetics I need to add rectangles for out-of bounds blocks highlighting target quadrant(s)
if(length(targetSubj)>0){
for(i in 1:nrow(outerFill)){
    rect_i<-paste0("ggplot2::geom_rect(xmin=outerFill[",i,",]$xmin,xmax=outerFill[",i,",]$xmax,ymin= outerFill[",i,",]$ymin,ymax=outerFill[",i,",]$ymax,fill=outerFill[",i,",]$fill,col='transparent',alpha=0.03,inherit.aes=F)")
    badge_prop0 <- eval(parse(text=paste0("badge_prop0+",rect_i)))
}}

#Because \ gets escaped at some point, let's remove that and allow user to add newlines in centralText
centralText<-gsub("\\n","\n",centralText,fixed=T)

(badge_prop <- badge_prop0+
    #white background at center of circle
    ggplot2::geom_rect(data=NULL,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=-.001,fill="white",col="transparent")+
    ggplot2::geom_hline(yintercept=-.002,size=1,col=gpColors("galactic black"))+
    ggplot2::geom_hline(yintercept= barScale,size=1,col=gpColors("galactic black"))+
    #light colored backgrounds around inner badge
    ggplot2::geom_rect(inherit.aes=F,data=bgRec2,ggplot2::aes_string(xmin="xmin",ymin="ymin",xmax="xmax",ymax="ymax"),
                       fill=bgRec2$fill,alpha=.3)+
    ggplot2::geom_bar(stat="identity",col=gpColors("galactic black"),alpha=.9,position="stack")+
    ggplot2::labs(x="",y="")+#duplicate for...reasons...
    ggplot2::geom_point(y= smidge(),ggplot2::aes_string(fill="subject"),pch=21,size=2,stroke=.5)+#colored bullets
    # Dimension labels
    ggplot2::geom_text(data=label_data2, ggplot2::aes_string(x="id", y="y", label="dimAbbrev", hjust="hjust"),
                       lineheight=.9,col=gpColors("galactic black"), fontface="plain",alpha=1, size=3.5,
                       angle= 0, inherit.aes = FALSE )+
    # Central Text label
    ggplot2::annotate("text",x=Inf,y=-Inf,label=centralText,size=centralTextSize,fontface="bold",
                      col=gpColors("galactic black"),lineheight=0.7)+
    ggplot2::coord_polar(clip="off")+ggplot2::guides(fill="none")
  )
    #geom_label_npc(data=data.frame(x=.5,y=1),aes(npcx=x,npcy=y),label="djskfjadlsjldf")

#MANUALLY (ARG) add labels to corners of badge
gridLab<-function(x,y,label,fill,longestString,textCol="white",outlineCol="grey",outlineThickness=.05,lwd=1){
  #grid.polygon(x=c(.5,1,1,.99,.99,.5),y=c(1,1,.5,.5,.99,.99),gp=gpar(fill=fill,alpha=1,col="transparent"))
  #grid.polygon(x=c(.5,1,1,.99,.99,.5),y=c(1,1,.5,.5,.99,.99),gp=gpar(fill="transparent",alpha=1,col=gpColors("galactic black")))
  grid::grid.rect(x=x,y=y,width=grid::unit(1,"strwidth",data=paste0("  ",longestString,"  ")),height=grid::unit(6.5,"strheight",data=longestString),just="center",gp=grid::gpar(fill=fill,alpha=1,col=gpColors("galactic black"),lwd=lwd))

shadowtext::grid.shadowtext(label,x=x,y=y,bg.r=outlineThickness,bg.colour=outlineCol,just="center",gp=shadowtext::gpar(font=2,col=textCol,fontsize=14))
}

gridFooter<-function(bg,textCol,caption,x,y,fontsize=8,fillCol=gpColors("galactic black")){
  grid::grid.rect(x=.5,y=0,width=1,height=.06,just="bottom",gp=grid::gpar(fill=fillCol)) #"#090816"
  grid::grid.lines(x=c(0,1),y=c(.06,.06),gp=grid::gpar(col="white"))
  grid::grid.text(label=caption,x=x,y=y,just="left",gp=grid::gpar(col="white",fontsize=fontsize))
  grid::grid.raster(logoImg,x=grid::unit(1,"npc"),y=grid::unit(y,"npc"),height=grid::unit(.06,"npc"),just=c("right","center"))
  }

#import logo
  newURL<-"https://res.cloudinary.com/galactic-polymath/image/upload/v1594949366/logos/GP_logo_wordmark_horiz_white_transBG_300_kqc3ii.png"
  logoImg<-png::readPNG(RCurl::getURLContent(newURL),native=T)

  #make data frame to mark target subject(s)
  targetDF<-dplyr::tibble(subject=c("math","ela","science","social studies"),xText=c(.9,.9,.1,.1),yText=c(.8,.265,.265,.8),xBox=c(.9,.9,.1,.1),yBox=c(.91,.15,.15,.91),hjust="center",#c("left","left","right","right")
                   vjust="center",fill=subjPal)#c("bottom","top","top","bottom")))



# build learningChart -----------------------------------------------------


G<-grid::grid.grabExpr({
grid::grid.draw(badge_prop)
gridLab(.9,.91,"CC\nMath",subjPal[1],"C3 Soc Studies",outlineCol =gpColors("galactic black") ,outlineThickness=.1) #old outline color "#090816"
gridLab(.9,.15,"CC\nELA",subjPal[2],"C3 Soc Studies",outlineCol = gpColors("galactic black"),outlineThickness=.1)
gridLab(.1,.15,"NGSS\nScience",subjPal[3],"C3 Soc Studies",outlineCol = gpColors("galactic black"),outlineThickness=.1)
gridLab(.1,.91,"C3\nSoc Studies",subjPal[4],"C3 Soc Studies",outlineCol = gpColors("galactic black"),outlineThickness=.1)

#outline of plot area
# grid::grid.polygon(x=c(0,1,1,0),y=c(1,1,.06,.06),gp=grid::gpar(fill="transparent",col=gpColors("galactic black"),lwd=2))

gridFooter(caption=caption,x=0.01,y=.0275,fontsize=9)
invisible(sapply(targetSubj,function(x){
  d=targetDF %>% dplyr::filter(.data$subject==tolower(!!x))
  grid::grid.text(label="Target",x=grid::unit(d$xText,"npc"),y=grid::unit(d$yText,"npc"),just=c(d$hjust,d$vjust),
                  gp=grid::gpar(col=gpColors("galactic black"),fontface="bold",fontsize=14))
  grid::grid.rect(x=d$xBox,y=d$yBox,width=grid::unit(1,"strwidth",data=paste0("  C3 Soc Studies  ")),height=grid::unit(6.5,"strheight",data="C3 Soc Studies"),just="center",gp=grid::gpar(fill="transparent",alpha=1,col=gpColors("galactic black"),lwd=3))}
  ))
})
# output PNG of learning chart --------------------------------------------
dir.create(destFolder,showWarnings=FALSE, recursive=TRUE)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(fileName)),collapse=""),ext="png")

grDevices::png(outFile,width=7,height=4.5,units="in",res=dpi,...)
grid::grid.draw(G)
grDevices::dev.off()

#output to user
if(showPlot){
grid::grid.draw(G)
}

#tell user where file is saved
message("GP Learning Chart saved\n@ ",outFile)

#return object to user (wrapped in invisible to prevent meaningless gTree obj being printed)
return(invisible(G))

}
}
