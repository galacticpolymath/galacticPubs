#' learningEpaulette
#'
#' Create a Galactic Polymath Learning Epaulette which is a special kind of mosaic plot showing proportion of lesson by subject. Outputs a horizontal and a vertical version for mobile
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]. The basename of this working directory will then be used to find a match in the gp-lessons git project folder by calling [get_wd_git()]. It's a little roundabout, but is consistent with lookups centering on the Google Drive project working directory.
#' @param learningplot_correction do you want to correct proportions (for learningEpaulette and learningChart) for the total possible standards in each subject? i.e. Common Core Math has a LOT more standards than C3 Social Studies. default=F, the proportions will just be raw proportions of each subject out of total standards aligned. If TRUE, this will scale proportions of subjects by the relative total proportions of standards in each subject.
#' @param heightScalar for horizontal epaulette, multiplier for image height which affects amount of padding between label text and epaulette; default=1
#' @param epauletteHeight relative size of the epaulette; default=0.2
#' @param randomSeed random number for getting slightly different (but repeatable) repelled text labels
#' @param saveFile T/F, save file or just print to screen?
#' @param destFolder where do you want to save the folder; by default in the "assets/_learning-plots" folder, 1 level up from the working directory
#' @param fileName expects "somefilename" for ggsave output image file
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

learningEpaulette <-
  function(WD = "?",
           learningplot_correction = NULL,
           heightScalar = 0.5,
           epauletteHeight = 0.2,
           randomSeed = 101,
           saveFile = TRUE,
           destFolder,
           fileName = "GP-Learning-Epaulette",
           font_size = 8,
           width = 11,
           height = 1.6,
           dpi = 200,
           showPlot = TRUE,
           ...) {
    WD <- parse_wd(WD)
    if (missing(destFolder)) {
      destFolder <- fs::path("assets", "_learning-plots")
    }

    # #test for NULL values being supplied; replace with defaults
    # a<-match.call(expand.dots=TRUE)[-1] %>% as.list()
    #

    WD_git <- get_wd_git(WD = WD)
    if(is.null(learningplot_correction)){
      learningplot_correction=get_fm("LearningPlotCorrection",WD_git=WD_git)
    }

    #if WD supplied that is not getwd(), append it to destFolder
    if (!identical(WD, getwd())) {
      destFolder <- fs::path(WD, destFolder)
    }


    # Standards exist?
    standardsFile <- fs::path(WD_git, "saves", "standards.RDS")
    standardsFound <- file.exists(standardsFile)

    ###########
    # Do all this only if compiled standards found ----------------------------

    if (!standardsFound) {
      warning("Compiled Standards not found at: ", standardsFile)
    } else{
      # Define important variables ----------------------------------------------
      # Load compiled standards
      importedData <- readRDS(standardsFile)
      targetSubj <- importedData$targetSubj


      # Interpret imported data -------------------------------------------------


      #Calculate corrected proportions if requested
      proportions0 = importedData$a_combined  %>% dplyr::group_by(.data$subject) %>% dplyr::filter(.data$n >
                                                                                        0)
      if (learningplot_correction) {
        proportions = proportions0 %>% dplyr::summarise(
          proportion = round(sum(.data$n_prop_adj, na.rm =
                                   T), 2),
          .groups =
            "drop",
          n_stnds = sum(.data$n)
        )
      } else{
        #don't use adjusted proportions if not requested
        proportions = proportions0 %>% dplyr::summarise(
          proportion = round(sum(.data$n_prop, na.rm =
                                   T), 2),
          .groups =
            "drop",
          n_stnds = sum(.data$n)
        )
      }

      #Make sure proportions = 100%

      tot_prop <- sum(proportions$proportion)
      if (tot_prop != 1) {
        remaining <- 1 - tot_prop
        where_add <- proportions$proportion %>% which.max()
        message(
          "In output for learningEpaulette(), adding ",
          round(remaining, 2),
          proportions$subject[where_add],
          " so proportions add to 100%",

          " to ",
        proportions$proportion[where_add] <-
          proportions$proportion[where_add] + remaining
        )
      }

      # Set up vector to recode subject levels according to key (for learning chart subject abbrevs.)

      # names(convert_labels) <- ordered_subjects
      #
      # convert_labels2 <- ordered_subj
      # names(convert_labels2) <- ordered_subjects

      xlabels <- proportions  %>%
        dplyr::left_join(.,importedData$chart_labels,by=c(subject="full_subj")) %>%
        dplyr::mutate(
          value = scales::percent(.data$proportion),
          x.prop = proportion,
          x = (cumsum(.data$proportion) -
                 .data$proportion / 2),
          # #not sure the next one is necessary...should remove if not
          # subj = dplyr::recode(.data$subject_orig, !!!convert_labels2),
          lab = paste0(t(.data$value), t(.data$abbrev_subj)),
          hjust = 0.5,
          fontface = "plain",
          stroke = 1,
          strokeCol = gpColors(.data$gp_pal_subj),
          lightCol = colorspace::lighten(strokeCol, 0.8),
          size = 9,
          y = 0.6
        )


      #thickness of epaulette bar
      thickness = 0.2

      # #I think learning chart can handle sustainability above...this epaulette code CANNOT...
      #
      # epaulette_names <-
      #   ordered_subj_chart[match(proportions$subject, ordered_subjects)]
      # clrs <-
      #   subj_tib %>% dplyr::filter(.data$Subjects %in% proportions$subject) %>% dplyr::pull("color")

      rectangles <-
        dplyr::tibble(
          proportion = xlabels$proportion,
          n_stnds = xlabels$n_stnds,
          xmin = c(0, cumsum(xlabels$proportion)[-length(xlabels$proportion)]),
          xmax = cumsum(xlabels$proportion),
          ymin = 1 - thickness,
          ymax = 1,
          subject = xlabels$abbrev_subj,
          color = xlabels$strokeCol
        ) %>% dplyr::filter(.data$proportion > 0)
      rectangles$subject <-
        factor(rectangles$subject,
               ordered = T,
               levels = rectangles$subject)
      rectangles$border <- "transparent"

      #Ensure that xmax[4] is 1, so there's never a gap at the right
      rectangles$xmax[nrow(rectangles)] <- 1


      #boldenize & embiggenate if targetSubj indicated
      if (!is.null(targetSubj)) {
        (targetRows <-
           which(!is.na(charmatch(
             tolower(xlabels$subject), tolower(targetSubj)
           ))))
        xlabels$stroke[targetRows] <- 2
        xlabels$strokeCol[targetRows] <- gpColors("galactic black")
        xlabels$fontface[targetRows] <- "bold"
        xlabels$size[targetRows] <- 11
        rectangles$border[targetRows] <- gpColors("galactic black")
      }



      # #install compact font
      # sysfonts::font_add_google(name="sans",regular.wt=400,bold.wt=600)
      # showtext::showtext_auto()
      ## PLOT Epaulette

      epaulette <-
        ggplot2::ggplot(rectangles) +
        ggplot2::geom_rect(
          ggplot2::aes_string(
            xmin = "xmin",
            xmax = "xmax",
            ymin = "ymin",
            ymax = "ymax",
            fill = "subject"
          ),
          size = 1.2,
          show.legend = F
        ) +
        ggplot2::scale_colour_manual(values = as.vector(rectangles$color),
                                     aesthetics = c("color", "fill")) +
        #Add Target border(s) if necessary
        ggplot2::geom_rect(
          ggplot2::aes_string(
            xmin = "xmin",
            xmax = "xmax",
            ymin = "ymin",
            ymax = "ymax"
          ),
          fill = "transparent",
          colour = rectangles$border,
          size = 2.3,
          show.legend = F
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1),
                                    expand = ggplot2::expansion(c(0, 0), 0)) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()
        )


      subject_labels <-
        subset(xlabels, xlabels$x.prop > 0) %>% ggplot2::ggplot() +
        galacticEdTools::theme_galactic(font = "sans") +
        ggrepel::geom_text_repel(
          ggplot2::aes_string(
            x = "x",
            y = .2,
            label = "lab",
            fontface = "fontface",
            segment.size = "stroke"
          ),
          seed = randomSeed,
          family = "sans",
          size = font_size,
          show.legend = FALSE,
          direction = "both",
          col = gpColors("galactic black"),
          force = 6,
          point.padding = 0.25
        ) +
        #scale y to pad text according to user input
        ggplot2::scale_y_continuous(limits = c(0, 0.2),
                                    expand = ggplot2::expansion(c(0, 0), 0)) +
        ggplot2::scale_x_continuous(limits = c(0, 1),
                                    expand = ggplot2::expansion(c(0, 0), 0)) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()
        )

      ###
      # HORIZONTAL EPAULETTE + LABELS
      # calculate relative label height; we need to scale the relative proportion of label with the overall image height
      ep_abs_height <-
        epauletteHeight * height #Target epaulette height in inches (must stay constant)
      scaled_height <- (height * heightScalar) #New height in inches
      relEpauletteHeight <- (ep_abs_height) / scaled_height
      relLabHeight <- 1 - relEpauletteHeight

      G <-
        epaulette / subject_labels  + patchwork::plot_layout(
          ncol = 1,
          heights = c(relEpauletteHeight, relLabHeight),
          widths = c(1, 1)
        ) &
        ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(0, 0, 0, 0), "points")
        )




      epaulette_vert <-  ggplot2::ggplot(rectangles) +
        ggplot2::geom_rect(
          ggplot2::aes_string(
            xmin = "xmin",
            xmax = "xmax",
            ymin = "ymin",
            ymax = "ymax",
            fill = "subject"
          ),
          size = 1.2,
          show.legend = F
        ) +
        ggplot2::scale_colour_manual(values = as.vector(rectangles$color),
                                     aesthetics = c("color", "fill")) +
        #Add Target border(s) if necessary
        ggplot2::geom_rect(
          ggplot2::aes_string(
            xmin = "xmin",
            xmax = "xmax",
            ymin = "ymin",
            ymax = "ymax"
          ),
          fill = "transparent",
          colour = rectangles$border,
          size = 2.3,
          show.legend = F
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1),
                                    expand = ggplot2::expansion(c(0.001, 0.001))) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0.1, 0.1))) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()
        ) +
        ggplot2::coord_flip()#+ggplot2::theme_bw()

      xlabels$vertLabStart <- cumsum(xlabels$x.prop)

      subject_labels_vert <-
        subset(xlabels, xlabels$x.prop > 0) %>% ggplot2::ggplot() +
        galacticEdTools::theme_galactic(font = "sans") +
        ggplot2::geom_text(
          ggplot2::aes_string(
            x = .1,
            y = "vertLabStart-.01",
            label = "abbrev_subj",
            fontface = "fontface"
          ),
          hjust = 0,
          vjust = 1.2,
          size = font_size * 0.85,
          family = "sans",
          show.legend = FALSE,
          col = gpColors("galactic black")
        ) +
        ggplot2::geom_segment(ggplot2::aes_string(
          x = .001,
          xend = 2,
          y = "vertLabStart",
          yend = "vertLabStart"
        )) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(c(0, 0))) +
        ggplot2::scale_y_continuous(limits = c(0, 1),
                                    expand = ggplot2::expansion(c(0.001, 0.001))) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 0, 0))

      ###
      # VERTICAL EPAULETTE + LABELS

      G_vert <-
        epaulette_vert + subject_labels_vert + patchwork::plot_layout(ncol = 2, widths =
                                                                        c(0.2, 0.8)) &
        ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()
        )
      #make a vertical version of this

      if (showPlot) {
        plot(G)
      }


      #create folder if necessary
      dir.create(destFolder,
                 showWarnings = FALSE,
                 recursive = TRUE)


      givenExt = ifelse(grepl(".", fileName, fixed = TRUE),
                        gsub(".*\\.(.{3,4}$)", "\\1", fileName),
                        NA) #extract file extension if provided
      fileOut <-
        gsub("(^.*)\\..*$", "\\1", basename(fileName)) #strip extension and full path from provided fileName

      fileOut_vert <- paste0(fileOut, "_vert")

      fileOutExt <-
        ifelse(is.na(givenExt), "png", givenExt) #provide png extension if not provided
      output <- fs::path(destFolder, fileOut, ext = fileOutExt)
      output_vert <-
        fs::path(destFolder,  fileOut_vert, ext = fileOutExt)


      #save the file
      ggplot2::ggsave(
        filename = basename(output),
        plot = G,
        path = fs::path_dir(output),
        width = width,
        height = height * heightScalar,
        dpi = dpi,
        bg = "transparent"
      )
      # ,
      #                 ...)
      #save vertical version
      ggplot2::ggsave(
        filename = basename(output_vert),
        plot = G_vert,
        path = fs::path_dir(output_vert),
        width = height * 1,
        height = width * .5,
        dpi = dpi,
        bg = "transparent"
      )
      #,
      #               ...)

      #output object if they want to modify further
      message("GP Learning Epaulette saved\n@ ", output)
      prop <- rectangles$proportion
      names(prop) <- rectangles$subject
      message("Running upload_assets() to upload updated epaulettes & add to front-matter")
      upload_assets(WD = WD)

      return(invisible(list(
        horiz = G,
        vert = G_vert,
        proportions = prop
      )))
    }
  }
