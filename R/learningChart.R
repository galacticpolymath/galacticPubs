#' learningChart
#'
#' Make a GP Learning Chart. Will also run [upload_assets()] when it's done.
#'
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]. The basename of this working directory will then be used to find a match in the gp-lessons git project folder by calling [get_wd_git()]. It's a little roundabout, but is consistent with lookups centering on the Google Drive project working directory.
#' @param caption quoted text you want to go at the bottom of the chart
#' @param captionN T/F, add the range of the number of standards per grade used to make the plot to caption? default=FALSE
#' @param centralText specify grades the chart is for; by default pulls most common gradeBand from compiledAlignment (e.g. "grades`\\n`5-6")
#' @param centralTextSize multiplier for font size of centralText
#' @param quotedTitle the quoted title used to attribute the learning chart (e.g. Knowledge and skills taught by 'quotedTitle')
#' @param saveFile T/F, save file or just print to screen?
#' @param destFolder where do you want to save the folder; by default in the "assets/learningPlots" folder, 1 level up from the working directory
#' @param fileName expects "somefilename" (file extension will be ignored)
#' @param dpi what resolution would you like the output in dots per inch? 300 by default
#' @param showPlot plot to screen or just save to file? default=T
#' @param ... other arguments passed to \code{\link[grDevices]{png}}
#' @return the learning chart plot object (grid format); Result can be plotted with grid::grid.draw(result). The chart file is saved to assets/GP_Learning_Chart.png by default
#' @export
#' @importFrom rlang .data


learningChart = function(WD = "?",
                         caption = NA,
                         captionN = FALSE,
                         centralText = NA,
                         quotedTitle = NA,
                         centralTextSize = 3.5,
                         saveFile = TRUE,
                         destFolder,
                         fileName = "GP-Learning-Chart",
                         dpi = 200,
                         showPlot = TRUE,
                         ...) {
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD = WD)


  if (missing(destFolder)) {
    destFolder <- fs::path(WD, "assets", "_learning-plots")
  }


  # Define preference order of subjects -------------------------------------

  supported_subjects <- factor(c("ELA",
                                 "Math",
                                 "Science",
                                 "Social Studies",
                                 "Sustainability"),
                               ordered = T)


  if(is_empty(quotedTitle)){
  quotedTitle <- get_fm("ShortTitle", WD)
  }

  if (is_empty(quotedTitle)) {
    quotedTitle <-
      "this lesson"
  } else{
    quotedTitle <- paste0("mini-unit: \"", quotedTitle, "\"")
  }
  #deal with missing caption and add sample size if requested
  if (is_empty(caption)) {
    caption = paste0("Knowledge & skills taught in ", quotedTitle)
  }

  # Standards exist?
  standardsFile <- fs::path(WD_git, "saves", "standards.RDS")
  standardsFound <- file.exists(standardsFile)

  ###########
  # Do all this only if compiled standards found ----------------------------

  if (!standardsFound) {
    warning("No Learning Chart created. Compiled Standards not found at: ",
            standardsFile)
    return(NULL)

  } else{
    # Define important variables ----------------------------------------------
    # Load compiled standards
    importedData <- readRDS(standardsFile)
    compiledAlignment <- importedData$data
    a_combined0 <- importedData$a_combined%>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("n"),  ~
                                                     ifelse(is.na(.), 0, .)))
    targetSubj <- importedData$targetSubj

    if (!importedData$learning_chart_friendly) {
      warning(
        "No Learning Chart created. The currently aligned standards sets are not currently supported."
      )
      return(NULL)

    } else{
      if (captionN) {
        avgN <-
          table(compiledAlignment$compiled$gradeband) %>% as.vector() %>%
          mean(na.rm = T)
        caption <-
          paste0(caption, " (~", floor(avgN), " standards per grade band)")
      }

      if (is_empty(centralText)) {
        Gs <- get_fm("ForGrades", WD)
        #Shorten long words (e.g. "9-university" becomes "9-uni")
        Gs <- gsub("(^[^-]*-.{1,3}).*$", replacement = "\\1", Gs)
        prefix <- get_fm("GradesOrYears", WD)

        centralText <-
          paste0(prefix, "\n", Gs)
      }




      # Make a Learning Chart --------------------------------------


      # Filter out certain incomplete subjects ----------------------------------
      #Only keep first 4 subjects in order of number of aligments

      ranked_subj_alignments <- a_combined0  %>%
        dplyr::group_by(.data$subject) %>%
        dplyr::summarise(N = sum(.data$n, na.rm = TRUE)) %>%
        dplyr::arrange(dplyr::desc(.data$N))


      to_keep <- ranked_subj_alignments$subject[1:4]


      a_combined <-
        a_combined0 %>% dplyr::filter(.data$subject %in% to_keep) %>%
        dplyr::left_join(., importedData$chart_labels, by = c(subject =
                                                                "full_subj"))

      unique_subj <- unique(a_combined$gp_pal_subj)
      subjPal <- gpColors(unique_subj)
      #Need to rename to agree with named subjects

      names(subjPal) <-
        a_combined$subject[match(unique_subj, a_combined$gp_pal_subj)]


      # # Make sure a_combined 'n' cols have no NA -----------------------------
      # a_combined <-
      #   a_combined %>% dplyr::mutate(dplyr::across(dplyr::starts_with("n"),  ~
      #                                                ifelse(is.na(.), 0, .))) %>%
      #   dplyr::filter(!is.na(.data$n))

      #Overwrite data (bit messy, but controls for NAs in incoming data)
      a_combined$id <- 1:nrow(a_combined)


      max_n <- max(a_combined$n, na.rm = T)

      # # Rescale n counts of standards to always fit in plot ---------------------
      a_combined <- a_combined %>%
        dplyr::mutate(n_rescale = .data$n / max_n)
      #

      #val for scale of the biggest ray,scaled appropriately
      barScale <- max(a_combined$n_rescale, na.rm = T)


      #function for putting things a little beyond the max value in the dataset
      smidge <- function(amt = 1) {
        barScale + (amt * barScale / 10)
      }


      label_data2 <- a_combined
      #tweak separation of dots and labels around circle
      label_data2$hjust <-c(rep(0,6),rep(1,6))
      label_data2$vjust <- c(0,rep(0.5,4),1,1,rep(0.5,4),0)
      label_data2$nudgeY <- c(rep(0,7),rep(0.05,4),0)
      label_data2$y <- smidge(2)
      label_data2$y[c(1, nrow(label_data2))] <- smidge(3)

      #make background rectangles for each set of dimensions

      bgRec2 <-
        dplyr::tibble(
          subject = unique_subj,
          xmin = seq(0.5, 9.5, 3),
          xmax = seq(3.5, 12.5, 3),
          ymin = rep(0, 4),
          ymax = barScale,
          fill = subjPal
        )

      # size of badge -----------------------------------------------------------
      badge_scale = 2
      central_circle_size = 0.4

      targetRows <- which(bgRec2$subject %in% tolower(targetSubj))
      outerFill <- bgRec2[targetRows, ]
      outerFill$ymin <- smidge(.1)
      outerFill$ymax <- 3 * badge_scale


      # make the badge! ---------------------------------------------------------


      suppressWarnings(
        badge_prop0 <-
          ggplot2::ggplot(
            a_combined,
            ggplot2::aes_string(x = "as.factor(id)", y = "n_rescale", fill = "subject"),
            col = gpColors("galactic black")
          ) +
          ggplot2::geom_bar(
            stat = "identity",
            col = gpColors("galactic black"),
            alpha = .9,
            position = "stack"
          ) +
          ggplot2::scale_fill_manual(values = subjPal) +
          galacticEdTools::theme_galactic(font = "sans") +
          ggplot2::theme(
            plot.margin = ggplot2::margin(
              t = 0,
              r = 0,
              b = -15,
              l = -20
            ),
            # not sure why this correction is necessary, but without it, the plot is not centered
            #axis.line.y=element_line(colour="grey"),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(size = .2, colour =
                                                         "grey"),
            panel.grid.minor.y = ggplot2::element_line(size = 1.5, colour =
                                                         "grey"),
            panel.grid.major.x = ggplot2::element_line(size = .2, colour =
                                                         "grey"),
            axis.text.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            #element_text(margin=margin(t=10,b=10,l=0,r=0,unit="pt"),hjust=1),
            axis.title = ggplot2::element_blank(),
            #element_text(margin = margin(0)),
            axis.ticks.length = ggplot2::unit(0, "pt"),
            panel.spacing = ggplot2::unit(0, "pt"),
            panel.border = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "white"),
            plot.background = ggplot2::element_rect(fill = "white"),
            legend.background = ggplot2::element_rect(fill = "transparent"),
            legend.position = "none",
            legend.margin = ggplot2::margin(0),
            legend.box.margin = ggplot2::margin(0),
            legend.spacing.y = ggplot2::margin(0)
            # legend.box="vertical",
            # legend.box.spacing=unit(0,"npc"),
            # legend.margin=margin(0,0,0,0),
            # legend.title=element_text(face="bold")
          )
        #suppress NA row warnings
      ) #End badge_prop0 &

      #Make target rectangle(s) where necessary
      #because of a stupid clipping thing with aesthetics I need to add rectangles for out-of bounds blocks highlighting target quadrant(s)
      if (length(targetSubj) > 0) {
        g_outerFill <- lapply(1:nrow(outerFill), \(i) {
          ggplot2::geom_rect(
            xmin = outerFill$xmin[i],
            xmax = outerFill$xmax[i],
            ymin = outerFill$ymin[i],
            ymax = outerFill$ymax[i],
            fill = outerFill$fill[i],
            col = 'transparent',
            alpha = 0.03,
            inherit.aes = F
          )
        })

      } else{
        g_outerFill <- {

        }
      }

      #Because \ gets escaped at some point, let's remove that and allow user to add newlines in centralText
      centralText <- gsub("\\n", "\n", centralText, fixed = T)



      suppressWarnings(
        badge_prop <- badge_prop0 +
          #cover outside circle crap with white box
          ggplot2::geom_rect(
            data = NULL,
            xmin = -Inf,
            xmax = Inf,
            ymin = 1.002,
            ymax = smidge(badge_scale * 8),
            fill = "white",
            col = "transparent",
            inherit.aes = F
          ) +
          g_outerFill +
          ggplot2::coord_polar(clip = "off") + ggplot2::guides(fill = "none") +
          ggplot2::scale_y_continuous(
            expand = c(0, 0),
            limits = c(-central_circle_size, smidge(badge_scale * 2))
          )  +
          #white background at center of circle
          ggplot2::geom_rect(
            data = NULL,
            xmin = -Inf,
            xmax = Inf,
            ymin = -Inf,
            ymax = -.001,
            fill = "white",
            col = "transparent"
          ) +
          ggplot2::geom_hline(
            yintercept = -.002,
            size = 1,
            col = gpColors("galactic black")
          ) +
          ggplot2::geom_hline(
            yintercept = barScale,
            size = 1,
            col = gpColors("galactic black")
          ) +
          #light colored backgrounds around inner badge
          ggplot2::geom_rect(
            inherit.aes = F,
            data = bgRec2,
            ggplot2::aes_string(
              xmin = "xmin",
              ymin = "ymin",
              xmax = "xmax",
              ymax = "ymax"
            ),
            fill = bgRec2$fill,
            alpha = .3
          ) +
          ggplot2::geom_bar(
            stat = "identity",
            col = gpColors("galactic black"),
            alpha = .9,
            position = "stack"
          ) +
          ggplot2::labs(x = "", y = "") + #duplicate for...reasons...
          ggplot2::geom_point(
            y = smidge(0.75),
            ggplot2::aes_string(fill = "subject"),
            pch = 21,
            size = 2,
            stroke = .5
          ) + #colored bullets
          # Dimension labels

          ggplot2::geom_text(
            data = label_data2,
            ggplot2::aes(
              x = .data$id ,
              y = smidge(1.5)+ .data$nudgeY,
              label = .data$dimAbbrev,
              hjust = .data$hjust,
              vjust= .data$vjust,
            ),

            lineheight = .9,
            col = gpColors("galactic black"),
            fontface = "plain",
            alpha = 1,
            size = 3.5,
            angle = 0,
            inherit.aes = FALSE
          ) +
          # Central Text label
          ggplot2::annotate(
            "text",
            x = Inf,
            y = -Inf,
            label = centralText,
            size = centralTextSize,
            fontface = "bold",
            col = gpColors("galactic black"),
            lineheight = 0.7
          )
      )


      # make data frame to mark target subject(s) -------------------------------
      targetDF <-
        dplyr::tibble(
          subject = unique(label_data2$subject),
          #c("math", "ela", "science", "social studies"),
          xText = c(.9, .9, .1, .1),
          yText = c(.8, .265, .265, .8),
          xBox = c(.9, .9, .1, .1),
          yBox = c(.91, .15, .15, .91),
          hjust = "center",
          #c("left","left","right","right")
          vjust = "center",
          fill = subjPal
        ) %>%
        dplyr::left_join(
          .,
          y = (
            importedData$chart_labels %>% dplyr::select("full_subj", "learning_chart_labs")
          ),
          by = c("subject" = "full_subj")
        )



      # badge_prop +
      #   ggplot2::geom_label(data=targetDF,
      #   x = 2.65,
      #   y = 3.35,
      #   ggplot2::aes(
      #   label = .data$learning_chart_labs,
      #   fill=subject
      #   ),
      #   hjust = 1,
      #   label.padding = ggplot2::unit(0.5,"lines")
      # )

      #geom_label_npc(data=data.frame(x=.5,y=1),aes(npcx=x,npcy=y),label="djskfjadlsjldf")

      #MANUALLY (ARG) add labels to corners of badge
      gridLab <-
        function(x,
                 y,
                 label,
                 fill,
                 longestString,
                 textCol = "white",
                 outlineCol = "grey",
                 outlineThickness = .05,
                 lwd = 1) {
          #grid.polygon(x=c(.5,1,1,.99,.99,.5),y=c(1,1,.5,.5,.99,.99),gp=gpar(fill=fill,alpha=1,col="transparent"))
          #grid.polygon(x=c(.5,1,1,.99,.99,.5),y=c(1,1,.5,.5,.99,.99),gp=gpar(fill="transparent",alpha=1,col=gpColors("galactic black")))
          grid::grid.rect(
            x = x,
            y = y,
            width = grid::unit(1, "strwidth", data = paste0("  ", longestString, "  ")),
            height = grid::unit(6.5, "strheight", data = longestString),
            just = "center",
            gp = grid::gpar(
              fill = fill,
              alpha = 1,
              col = gpColors("galactic black"),
              lwd = lwd
            )
          )

          shadowtext::grid.shadowtext(
            label,
            x = x,
            y = y,
            bg.r = outlineThickness,
            bg.colour = outlineCol,
            just = "center",
            gp = shadowtext::gpar(
              font = 2,
              col = textCol,
              fontsize = 14
            )
          )
        }

      gridFooter <-
        function(caption,
                 x,
                 y,
                 fontsize = 8,
                 fillCol = gpColors("galactic black")) {
          grid::grid.rect(
            x = .5,
            y = 0,
            width = 1,
            height = .06,
            just = "bottom",
            gp = grid::gpar(fill = fillCol)
          ) #"#090816"
          grid::grid.lines(
            x = c(0, 1),
            y = c(.06, .06),
            gp = grid::gpar(col = "white")
          )
          grid::grid.text(
            label = caption,
            x = x,
            y = y,
            just = "left",
            gp = grid::gpar(col = "white", fontsize = fontsize)
          )
          grid::grid.raster(
            logoImg,
            x = grid::unit(1, "npc"),
            y = grid::unit(y, "npc"),
            height = grid::unit(.06, "npc"),
            just = c("right", "center")
          )
        }

      #import logo
      newURL <-
        "https://res.cloudinary.com/galactic-polymath/image/upload/v1594949366/logos/GP_logo_wordmark_horiz_white_transBG_300_kqc3ii.png"
      logoImg <-
        png::readPNG(RCurl::getURLContent(newURL), native = T)



      # build learningChart -----------------------------------------------------


      G <- grid::grid.grabExpr({
        grid::grid.draw(badge_prop)

        lapply(1:4, \(i) {
          df_subj_i <- targetDF[i,]
          gridLab(
            x = df_subj_i$xBox,
            y = df_subj_i$yBox,
            label = df_subj_i$learning_chart_labs,
            fill = df_subj_i$fill,
            longestString = "C3 Soc Studies",
            outlineCol = gpColors("galactic black"),
            outlineThickness = .1
          )
        })

        gridFooter(
          caption = caption,
          x = 0.01,
          y = .0275,
          fontsize = 9
        )

        (sapply(targetSubj, function(x) {
          d = targetDF %>% dplyr::filter(tolower(.data$subject) == tolower(x))
          grid::grid.text(
            label = "Target",
            x = grid::unit(d$xText, "npc"),
            y = grid::unit(d$yText, "npc"),
            just = c(d$hjust, d$vjust),
            gp = grid::gpar(
              col = gpColors("galactic black"),
              fontface = "bold",
              fontsize = 14
            )
          )
          grid::grid.rect(
            x = d$xBox,
            y = d$yBox,
            width = grid::unit(1, "strwidth", data = paste0("  C3 Soc Studies  ")),
            height = grid::unit(6.5, "strheight", data = "C3 Soc Studies"),
            just = "center",
            gp = grid::gpar(
              fill = "transparent",
              alpha = 1,
              col = gpColors("galactic black"),
              lwd = 3
            )
          )
        }))
      })


      # output PNG of learning chart --------------------------------------------
      if (saveFile) {
        dir.create(destFolder,
                   showWarnings = FALSE,
                   recursive = TRUE)
        outFile <-
          fs::path(destFolder, paste0(
            sub(
              pattern = "(.*?)\\..*$",
              replacement = "\\1",
              x = basename(fileName)
            ),
            collapse = ""
          ), ext = "png")

        grDevices::png(
          outFile,
          width = 7,
          height = 4.5,
          units = "in",
          res = dpi,
          ...
        )

        grid::grid.draw(G)
        grDevices::dev.off()

      }

      #output to user
      if (showPlot) {
        grid::grid.draw(G)
      }

      #tell user where file is saved
      message("GP Learning Chart saved\n@ ", outFile)
      message("Running upload_assets() to upload updated learningChart & add to front-matter")
      upload_assets(WD = WD)

      #return object to user (wrapped in invisible to prevent meaningless gTree obj being printed)
      return(invisible(G))

    }
  }
}
