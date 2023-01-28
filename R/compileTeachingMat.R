#' compileTeachingMat
#'
#' Compile Teaching Materials data from teaching-materials.xlsx
#' @param LessonEnvir what versions of this lesson are available? options: "Classroom" and "Remote"
#' @param linksFile file location of the lesson teaching-materials XLSX worksheet.
#' @param procedureFile file location of the lesson procedure XLSX worksheet
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedTeachingMaterials.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param structureForWeb default=TRUE; Do you want to preface JSON output with component & nest output in Data element?
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/processedResources.json
#' @importFrom rlang .data
#' @export
#'
compileTeachingMat <- function(LessonEnvir= c("Classroom"),
                               linksFile = "meta/teaching-materials.xlsx",
                               procedureFile = "meta/procedure_GSheetsOnly.xlsx",
                               destFolder = "meta/JSON/" ,
                               outputFileName = "teaching-materials.json",
                               WD = getwd(),
                               structureForWeb= TRUE) {

#make YouTube embed links from other links
YTembed<-function(link){
  gsub("^.*(?:v=|youtu.be\\/)([^&]*).*","https://www.youtube.com/embed/\\1",link)
}

  .=NULL #to avoid errors with dplyr syntax

   #if WD supplied, append it to destFolder
   if(!identical(WD, getwd())) {
     linksFile<- fs::path(WD,linksFile)
     procedureFile<-fs::path(WD,procedureFile)
     destFolder <- fs::path(WD, destFolder)
   }

  #read in front-matter.yml
  current_data<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))
  #read in links
  rsrcSummary<-openxlsx::read.xlsx(linksFile,sheet="rsrcSumm",startRow=2)%>% dplyr::tibble()
  linksD<-openxlsx::read.xlsx(linksFile,sheet="dL",startRow=2)%>% dplyr::tibble()
  linksC<-openxlsx::read.xlsx(linksFile,sheet="c_pres",startRow=2)%>% dplyr::tibble()
  linksCH<-openxlsx::read.xlsx(linksFile,sheet="c_handouts",startRow=2)%>% dplyr::tibble()
  linksR<-openxlsx::read.xlsx(linksFile,sheet="r_pres",startRow=2)%>% dplyr::tibble()
  linksRH<-openxlsx::read.xlsx(linksFile,sheet="r_handouts",startRow=2)%>% dplyr::tibble()
  linksMedia<-openxlsx::read.xlsx(linksFile,sheet="multimedia",startRow=2)%>% dplyr::tibble()

  linksAssess<-openxlsx::read.xlsx(linksFile,sheet="assess",startRow=2)%>% dplyr::tibble()
  #read in procedure Part titles, etc
  procTitles<-openxlsx::read.xlsx(procedureFile,sheet="NamesAndNotes")%>% dplyr::tibble()

    #read in main procedure
  #import and make sure numbered columns are integers
  proc<-openxlsx::read.xlsx(procedureFile,sheet="Procedure") %>%
          dplyr::filter(.data$Step!=0) %>%
          rmNArows() %>%
          dplyr::tibble() %>%
          dplyr::mutate(Part=as.integer(.data$Part),
                        Chunk=as.integer(.data$Chunk),
                        ChunkDur=as.integer(.data$ChunkDur),
                        Step=as.integer(.data$Step),
                        PartN=as.integer(.data$PartN),
                        PartDur=as.integer(.data$PartDur))

  ####
  #Figure out lesson duration string
  partDurations<-proc$PartDur[which(proc$PartDur!="")] %>% as.numeric()
  lessonDur <- if(length(partDurations)==1){paste0(partDurations," min") #if just 1 part listed, do X min
    }else{
      #if more than 1 part, but they're all the same, combine them
      if(length(unique(partDurations))==1){
      paste0(length(partDurations)," x ",partDurations[1]," min")
        }else{
          #otherwise state each length separately
          sapply(1:length(partDurations),function(x) {
            paste0("Part ", x,": ",partDurations[x]," min")}) %>% paste0( collapse=", ")
      }
    }

  lessonDur



  #define helper function
  catchLinkNA<-function(linkText,url){
    if(is.na(url)){list(linkText=paste0("ERROR: '",linkText,"' link missing"),url=url)}else{
      list(linkText=linkText,url=url)
    }
  }



# Get grade level variation notes from Procedure.xlsx ---------------------
    gradeVariantNotes <-
      if (is.na(procTitles$PartGradeVarNotes[1])) {

      } else{
        if (length(which(stats::complete.cases(procTitles$PartGradeVarNotes))) ==
            1) {
          list(part = NA,
               partGradeVarNotes = procTitles$PartGradeVarNotes[1])
        } else{
          lapply(1:nrow(procTitles), function(i) {
            list(part = procTitles$Part[i],
                 partGradeVarNotes = procTitles$PartGradeVarNotes[i])
          })
        }
      }


########################################################################
# CLASSROOM ---------------------------------------------------------------

  # Get resource summary from teaching-materials.xlsx
  if (!"Classroom" %in% LessonEnvir) {
    resourcesC <- {
    }
  } else{

    s_C <- subset(rsrcSummary, rsrcSummary$envir == "classroom")
    rsrcSumm_C <- lapply(unique(s_C$itemCat), function(x) {
      d <- subset(s_C, s_C$itemCat == x)
      d$nItem <- sapply(d$nItem, function(x)
        ifelse(is.na(x), 1, x))
      catCount <- sum(as.numeric(d$nItem))
      itemBundle = list(
        nItems = catCount,
        itemCat = d$itemCat[1],
        itemsGroup = lapply(1:length(d$item), function(i) {
          list(item = d$item[i],
               itemExplanation = d$itemExplanation[i])
        })
      )
      itemBundle
    })

    # resources -----------------------------------------------------

    coveredGrades <-
      unique(c(linksC$grades, linksCH$grades))[which(!is.na(unique(c(
        linksC$grades, linksCH$grades
      ))))]
    #Remove any letters
    coveredGrades<-gsub("[a-zA-Z]","",coveredGrades)


    #Build classroom resources list
    resourcesC <- lapply(coveredGrades, function(currGradeBand) {
      gradePrefix = paste0(substr(current_data$GradesOrYears,1,1), currGradeBand)
      currDownloadClass <-
        linksD %>% dplyr::filter(.data$envir == "classroom")
      currDownloadAll <-
        currDownloadClass %>% dplyr::filter(.data$grades == currGradeBand &
                                              .data$part == "all") %>% dplyr::select(.data$gDriveLink) %>% dplyr::slice(1) %>% unlist() %>% as.vector()

      #aggregate data for all parts
      parts <- lapply(1:length(procTitles$PartTitle), function(part_i) {
        part <- procTitles$Part[part_i]
        title <- procTitles$PartTitle[part_i]
        preface <- procTitles$PartPreface[part_i]
        currPresentations <-
          subset(linksC,
                 linksC$part == part_i & linksC$grades == currGradeBand)
        currPresentations2 <- if (nrow(currPresentations) == 0) {
        } else{
          lapply(1:nrow(currPresentations), function(i) {
            list(
              itemTitle = "Presentation",
              itemCat = "presentation",
              links = list(
                catchLinkNA(linkText = "Present Now",
                            url = currPresentations$gPresentLink[i]),
                catchLinkNA(linkText = "Copy to My Google Drive",
                            url = currPresentations$gShareLink[i])
              )
            )
          })
        }
        currHandouts <-
          subset(linksCH,
                 linksCH$part == part_i & linksCH$grades == currGradeBand)
        currHandouts2 <- if (nrow(currHandouts) == 0) {
        } else{
          lapply(1:nrow(currHandouts), function(i) {
            list(
              itemTitle = currHandouts$title[i],
              itemCat = currHandouts$type[i],
              links = list(
                catchLinkNA(linkText = "PDF",
                            url = currHandouts$pdfLink[i]),
                catchLinkNA(linkText = "Copy to My Google Drive",
                            url = currHandouts$gShareLink[i])
              )
            )
          })
        }
        itemList <- c(currPresentations2, currHandouts2)
        # #return assets for part_i
        list(
          part = part,
          title = title,
          preface = preface,
          itemList = itemList
        )

        # currDownloadAll.part_i<-currDownloadClass %>% dplyr::filter(.data$grades==currGradeBand&.data$part==part_i) %>% dplyr::select(.data$gDriveLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()


      })#end parts lapply

      #return list for the classroom lapply
      list(
        grades = paste0(current_data$GradesOrYears," ", currGradeBand),
        gradePrefix = gradePrefix,
        links = list(
          linkText = ifelse(
            is.na(currDownloadAll),
            paste0(
              "Error: 'Download ",
              gradePrefix,
              " Materials for All Parts' Link Missing"
            ),
            paste0("Download ", gradePrefix, " Materials for All Parts")
          ),
          url = currDownloadAll
        ),
        parts = parts
      )


    })#end classroom lapply
  }

########################################################################
# REMOTE ---------------------------------------------------------------

  # Get resource summary from teaching-materials.xlsx
  if (!"Remote" %in% LessonEnvir) {
    resourcesR <- {
    }
  } else{
    s_R <- subset(rsrcSummary, rsrcSummary$envir == "remote")
    rsrcSumm_R <- lapply(unique(s_R$itemCat), function(x) {
      d <- subset(s_R, s_R$itemCat == x)
      d$nItem <- sapply(d$nItem, function(x)
        ifelse(is.na(x), 1, x))
      catCount <- sum(as.numeric(d$nItem))
      itemBundle = list(
        nItems = catCount,
        itemCat = d$itemCat[1],
        itemsGroup = lapply(1:length(d$item), function(i) {
          list(item = d$item[i],
               itemExplanation = d$itemExplanation[i])
        })
      )
      itemBundle
    })
    # #Get grade level variation notes from Procedure.xlsx
    # gradeVariantNotes<-if(is.na(procTitles$PartGradeVarNotes[1])){NA}else{
    #   if(length(which(stats::complete.cases(procTitles$PartGradeVarNotes)))==1){list(part=NA,partGradeVarNotes=procTitles$PartGradeVarNotes[1])}else{
    #     lapply(1:nrow(procTitles),function(i){list(part=procTitles$Part[i],partGradeVarNotes=procTitles$PartGradeVarNotes[i])})
    #   }
    # }

    # resources -----------------------------------------------------
    coveredGrades.R <-
      unique(c(linksR$grades, linksRH$grades))[which(!is.na(unique(c(
        linksR$grades, linksRH$grades
      ))))]
    #Build classroom resources list
    resourcesR <- lapply(coveredGrades.R, function(currGradeBand.R) {
      #not currently supporting batch downloads for remote lesson

      # currDownloadRemote<-linksD %>% dplyr::filter(.data$envir=="remote")
      # currDownloadAll<-currDownloadRemote %>% dplyr::filter(.data$grades==currGradeBand.R&.data$part=="all") %>% dplyr::select(.data$gDriveLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()

      #aggregate data for all parts
      parts <- lapply(1:length(procTitles$Part), function(part_i) {
        part <- procTitles$Part[part_i]
        title <- procTitles$PartTitle[part_i]
        preface <- procTitles$PartPreface[part_i]
        currPresentations <-
          subset(linksR,
                 linksR$part == part_i & linksR$grades == currGradeBand.R)
        currPresentations2 <- if (nrow(currPresentations) == 0) {
        } else{
          lapply(1:nrow(currPresentations), function(i) {
            list(
              itemTitle = "Presentation",
              itemCat = "presentation",
              links = list(
                catchLinkNA(linkText = "Preview Now with Nearpod",
                            url = currPresentations$nPresentLink[i]),
                catchLinkNA(linkText = "Add to My Nearpod Library",
                            url = currPresentations$nShareLink[i]),
                catchLinkNA("Edit in My Google Drive",
                            url = currPresentations$gShareLink[i])
              )
            )
          })
        }
        currHandouts <-
          subset(linksRH,
                 linksRH$part == part_i &
                   (
                     linksRH$grades == currGradeBand.R | is.na(linksRH$grades)
                   ))
        currHandouts2 <- if (nrow(currHandouts) == 0) {
        } else{
          lapply(1:nrow(currHandouts), function(i) {
            list(
              itemTitle = currHandouts$title[i],
              itemCat = currHandouts$type[i],
              links = list(
                linkText = ifelse(
                  is.na(currHandouts$distrLink[i]),
                  "ERROR: Distribution Link Missing",
                  paste0(
                    "Download as .",
                    tools::file_ext(currHandouts$distrLink[i])
                  )
                ),
                url = currHandouts$distrLink[i]
              )#,
              # list(
              #   linkText="Copy to My Google Drive",
              #   url=currHandouts$templateLink[i])

            )
          })
        }
        itemList <- c(currPresentations2, currHandouts2)

        #return assets for part_i
        # currDownloadAll.part_i<-currDownloadClass %>% dplyr::filter(.data$grades==currGradeBand.R&.data$part==part_i) %>% dplyr::select(.data$gDriveLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()

        list(
          part = part,
          title = title,
          preface = preface,
          itemList = itemList#,
          #links=list(linkText=paste0("Download All Part ",part_i,", ",gradePrefix," Materials for All Parts"),
          # url=currDownloadAll.part_i)
        )
      })#end parts lapply

      #return list for the classroom lapply
      gradePrefix = paste0(substr(current_data$GradesOrYears,1,1), currGradeBand.R)
      list(
        grades = paste0(current_data$GradesOrYears," ", currGradeBand.R),
        gradePrefix = gradePrefix,
        # links=list(
        #        linkText=paste0("Download ",gradePrefix," Materials for All Parts"),
        #        url=currDownloadAll
        #        ),
        parts = parts
      )


    })#end remote lapply
  }



# Multimedia --------------------------------------------------------------
# Outputs to separate multimedia JSON
 #if "by" is left blank, add Galactic Polymath by default
  m<-linksMedia
  m$by<-ifelse(is.na(m$by),"Galactic Polymath",m$by)
  #if byLink is blank, but by is galactic polymath, add our Youtube channel
  m$byLink <- ifelse(is.na(m$byLink)&!is.na(m$by),"https://www.youtube.com/channel/UCfyBNvN3CH4uWmwOCQVhmhg/featured",m$byLink)

multimedia<-lapply(1:nrow(m),function(i){
  d<-m[i,]
  list(order=d$order,
    type=d$type,
    title=d$title,
    description=d$description,
    lessonRelevance=d$lessonRelevance,
    by=d$by,
    #if byLink left blank, but
    byLink=d$byLink,
    keywords=d$keywords,
    #Change YouTube links to be embeds & turn {filename.png} links to files found in assets/_other-media-to-publish into catalog.galacticpolymath.com links
    mainLink=YTembed(d$mainLink) %>% expand_md_links(repo=whichRepo(WD=WD)),
    vimeoLink=d$vimeoLink,
    filename=d$filename,
    otherLink=d$otherLink)
})


# Assessments -------------------------------------------------------------
 a_list<-if(is_empty(linksAssess)){}else{
  list(part="last",
       title="Assessments",
       preface="",
       itemList=lapply(1:nrow(linksAssess), function(i) {
         d_i <- linksAssess[i,]
         list(
           itemTitle = ifelse(is_empty(d_i$title),d_i$filename,d_i$title),
           itemCat = NULL,
           links = list(
             list(linkText = "PDF",
                  url = d_i$pdfLink),
             list(linkText = "Copy to My Google Drive",
                  url = d_i$gShareLink)
           )
         )
       })
  )
 }

teachingMat0<-list(lessonDur= lessonDur,
                classroom = if (is.null(resourcesC)) {
                  } else{
                    #Add assessments to the parts list
                    if(!is.null(a_list)){
                      for(i in 1:length(resourcesC)){
                        resourcesC[[i]]$parts <- c(resourcesC[[i]]$parts, list(a_list))
                      }
                    }
                    #return modified list
                    list(
                      resourceSummary = rsrcSumm_C,
                      gradeVariantNotes = gradeVariantNotes,
                      resources = resourcesC
                    )
                  },
                  remote = if (is.null(resourcesR)) {
                  } else{
                    #Add assessments to the parts list
                    if(!is.null(a_list)){
                      for(i in 1:length(resourcesR)){
                        resourcesR[[i]]$parts <- c(resourcesR[[i]]$parts, list(a_list))
                      }
                    }
                    #return modified list
                    list(
                      resourceSummary = rsrcSumm_R,
                      gradeVariantNotes = gradeVariantNotes,
                      resources = resourcesR
                    )
                  })

#discard null sections
teachingMat<-teachingMat0[which(lengths(teachingMat0)>0)]

# Prefix with component and title, and nest output in Data if structuring for web deployment
out<-if(structureForWeb){list(
  `__component` = "teaching-resources.teaching-resources",
  SectionTitle= "Teaching Materials",
  Data=teachingMat
)}else{teachingMat}



# Make json structured output ----------------------------------------------

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(outputFileName))),ext="json")


# Write JSON for teaching materials -----------------------------------
jsonlite::write_json(out,outFile,pretty=TRUE,auto_unbox=TRUE,na="null",null="null")

#write json for multimedia
jsonlite::write_json(multimedia,fs::path(destFolder,"multimedia.json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")



# printToScreenTable<-cbind(ack[,c("Role","Name","Title")],OtherInfo="BlahBlah")

# return compiled output --------------------------------------------------
message(" ",rep("-",30))
message(" Teaching Material Compiled:")
# print(printToScreenTable)
message(" JSON file saved\n @ ",outFile,"\n")
message(" JSON file saved\n @ ",fs::path(destFolder,"multimedia.json"),"\n")
message(" ",rep("-",30))


}
