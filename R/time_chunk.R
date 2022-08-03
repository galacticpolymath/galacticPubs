#' Make a "Time Chunking" Graphic
#'
#' Creates a special graph showing what part of the allotted time period is being referred to for a set of steps in a lesson procedure.
#'
#' @param WD is working directory of the project; default= getwd()
#' @export
#'
time_chunk <- function(WD=getwd()){
  check_wd(throw_error = F)
  dest_folder <- fs::path(WD,"assets","lesson-plan-markdown","dynamic_images")
  if(!dir.exists(dest_folder)){
  dir.create(dest_folder,recursive=T)
  }

  #Test if json procedure exists
  json_proc_path<-fs::path(WD,"meta","JSON","procedure.json")
  if(!file.exists(json_proc_path)){
    stop("Procedure.json not found. Run compileProcedure().")
  }

  proc_json<-jsonlite::read_json(json_proc_path)


  #define plot function
  chunk_graf <- function(partDur,
                         chunkTitle,
                         chunkStart,
                         chunkDur,
                         t_unit="min",
                         primary_color=gpColors("hydro"),
                         faded_color="#1A4E75",
                         line_width=7) {
    chunkEnd<-chunkStart+chunkDur-1
    chunkMid<-mean(c(chunkStart,chunkEnd))
    df<- data.frame(x=seq(chunkStart+0.5,chunkEnd+0.5,1))
    df_faded=if(chunkStart==0){NULL}else{data.frame(x=seq(0.5,chunkStart-.5,1))}
    cust_labels=rep("",partDur+1)
    cust_labels[seq(1,partDur+1,5)]<-seq(0,partDur,5)

    bottomMargin<-ifelse(chunkStart==0,-26,-26) #relic..not using xlab anymore


    ggplot2::ggplot(df) +
      #Current segment plot
      ggplot2::geom_segment(
        ggplot2::aes(
          x = x,
          xend = x,
          y = 0,
          yend = .5
        ),
        colour = primary_color,
        lwd = line_width,
        show.legend = F
      ) +{
      #Previous (faded) segment plot
      if(chunkStart==0){}else{
      ggplot2::geom_segment(
        inherit.aes = F,
        data = df_faded,
        ggplot2::aes(
          x = x,
          xend = x,
          y = 0,
          yend = .15
        ),
        colour = faded_color,
        lwd = line_width,
        show.legend = F
      )}} +
      ggplot2::scale_y_continuous(limits=c(0,1),expand = c(0, 0)) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, partDur, 1),
        labels = cust_labels,
        limits = c(0, partDur + .5),
        expand = ggplot2::expansion(add = .6, mult = 0)
      ) + ggplot2::labs(y="",
                        title=chunkTitle) +
      # Only show xlab for first plot
      {
        if (chunkStart == 0) {
          ggplot2::xlab("")#"Class Time (min)")
        } else{
          ggplot2::xlab("")
        }

      }+
      #label time chunk
      ggplot2::annotate("text",x=chunkMid,y=0.6,label=paste0(chunkDur," ",t_unit),
                        size=9,fontface=2,vjust=0,
                        colour=gpColors("hydro"))+
      galacticEdTools::theme_galactic() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 24, colour = "#090816"),
        plot.title = ggplot2::element_text(
          size = 30,
          face = "bold",
          hjust = 0,
          colour=gpColors("galactic black")
        ),
        axis.line.x = ggplot2::element_line(colour = "#090816", size = 1),
        axis.ticks.y = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_line(colour = "#090816", size = .5),
        axis.ticks.length = ggplot2::unit(6, "pt"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "#f0f4ff"),
        #panel.grid.major.x=element_line(size=.5,colour="#090816"),
        plot.margin = ggplot2::margin(c(5, 5, bottomMargin, -30), unit = "pt")

      )
  }

#part loop
message("Generating 'Time Chunking Plots'")
graf_successes<-pbapply::pblapply(1:length(proc_json$Data$parts),function(i){

  part<-paste0("P",i)
  p_df<-proc_json$Data$parts[[i]]

  #chunk loop
  p_i_results<-lapply(1:length(p_df$chunks),function(ii){
    c_df<-p_df$chunks[[ii]]
    #Draw the graph
    chunk_graf_ii<-catch_err(
      chunk_graf(
        partDur = p_df$partDur,
        chunkTitle = c_df$chunkTitle,
        chunkStart = c_df$chunkStart,
        chunkDur = c_df$chunkDur
      ),keep_results = TRUE
    )
    #Save the graph
    ggplot2::ggsave(fs::path(dest_folder,paste0("P",i,"_chunk_",ii,"of",length(p_df$chunks)),ext="png"),plot=chunk_graf_ii$result,width=14,height=1.7,
                    bg = "transparent")

    return(chunk_graf_ii$success)
  })


}) %>% unlist()


message(sum(graf_successes)," of ", length(graf_successes)," chunking graphs successfully rendered")
}
