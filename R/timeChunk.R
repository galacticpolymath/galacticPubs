#' timeChunk
#'
#' create visuals to show how lesson breakdown into chunks
#'
#' @param totTime the total estimated time of
#' @param segLengths lengths (in min) of procedural steps (format= c(x1,x2,x3))
#' @param destFolder where to save the output graph
#' @param prefix add a prefix to graph names?
#' @param Color color of current step bar graph fill color
#' @param fadedColor color of prior, completed steps bar graph fill color
#' @param lineWidth width of each bar in the graph
#' @return NULL; saves timeChunk
#'
timeChunk <- function(totTime,segLengths,destFolder="assets/timeChunk/",prefix,Color="#2c83c3",fadedColor="#1A4E75",lineWidth=7){
  if(missing(prefix)){prefix<-""}else{prefix<-paste0(prefix,"_")}
  starts<-c(0,cumsum(segLengths)[-length(segLengths)] )
  ends<-cumsum(segLengths)
  #Test if segments add to totTime
  if(max(ends)!=totTime){message(paste0("Sum of segment lengths (",max(segLengths),") \u2260 total time (",totTime,")"))}

  Gs<-list()
  for (i in 1:length(segLengths)){
    segLengths_i<-segLengths[i]
    df<-data.frame(x=seq(starts[i]+.5,ends[i]-.5,1))
    faded_x=data.frame(x=if(i==1){1}else{seq(0.5,ends[i-1]-.5,1)})
    faded_col=ifelse(i==1,"transparent",fadedColor)
    cust_breaks=seq(0,totTime,1)
    cust_labels=rep("",totTime+1)
    cust_labels[seq(1,totTime+1,5)]<-seq(0,totTime,5)


  #plot
  g<-ggplot2::ggplot(df)+ggplot2::geom_segment(ggplot2::aes_string(x="x",xend="x",y=0,yend=.5),colour=Color,lwd=lineWidth,show.legend=F)+
    ggplot2::geom_segment(inherit.aes=F,data=faded_x,ggplot2::aes_string(x="x",xend="x",y=0,yend=.15),colour=faded_col,lwd=lineWidth,show.legend=F)+ggplot2::scale_y_continuous(expand=c(0,0))+ggplot2::scale_x_continuous(breaks=cust_breaks,labels=cust_labels,limits=c(0,totTime+.5),expand=ggplot2::expansion(add=.6,mult=0))+ggplot2::xlab("")+ggplot2::ylab("")+
    galacticEdTools::theme_galactic()+
  ggplot2::theme(
    axis.text.y=ggplot2::element_blank(),
    axis.title=ggplot2::element_blank(),
    axis.line.x=ggplot2::element_line(colour="#090816",size=1),
    axis.ticks.y=ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_line(colour="#090816",size=.5),
    axis.ticks.length=ggplot2::unit(6,"pt"),
    panel.grid.major.y=ggplot2::element_blank(),
    panel.grid.minor.y=ggplot2::element_blank(),
    panel.grid.minor.x=ggplot2::element_blank(),
    panel.grid.major.x=ggplot2::element_blank(),
    panel.border=ggplot2::element_blank(),#element_rect(colour="gray50",size=.1),
    panel.background = ggplot2::element_rect(fill="#f0f4ff"),
    #panel.grid.major.x=element_line(size=.5,colour="#090816"),
    axis.text.x=ggplot2::element_text(colour="#090816"),
    plot.margin=ggplot2::margin(c(5,1,-10,-20),unit="pt")
  )

  #Save the file
  dir.create(destFolder,showWarnings=FALSE)#create folder if necessary

   ggplot2::ggsave(fs::path(destFolder,paste0(prefix,"seg_",i,"of",length(segLengths)),ext="png"),plot=g,width=14,height=1)
   # gPlotly<-plotly::ggplotly(g,width=290*2,height=21*10,staticPlot=T,tooltip = "none")
   # htmlwidgets::saveWidget(plotly::as_widget(gPlotly),"timeChunk.html")

   }#end for loop

  #lapply(1:length(Gs),function(i){
   #  ggsave(path(destFolder,paste0(prefix,"seg_",i,"of",length(segLengths)),ext="png"),plot=Gs[[i]],width=6,height=1)
 #})
}

