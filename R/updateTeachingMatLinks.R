#' updateTeachingMatLinks
#'
#' Adds/updates Google Drive share links to the requested tabs in teaching-materials.xlsx. If a file is found on Google Drive, it will add it to the appropriate tab, with a timestamp in updateNotes. If a listed google drive link doesn't match a found, an updateNote will be added "trashed?"
#'
#' Just a note that I'm using 2 libraries to handle XLSX files. Not ideal, but the imported data from openxlsx::read.xlsx is nicer than XLConnect::readWorksheetFromFile, but the editing of data from a complex spreadsheet is MUCH better in XLConnect.
#'
#' @param gh_proj_name The unique project title of this lesson which is prefixed on the lesson folder name and the GitHub project. Not necessarily the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores; If left off, will try to get this info from the GitHubPath if available in the front-matter.yml.
#' @param dataCat which info do you want to merge with your teaching-materials spreadsheet? Options= "download", "classroom" and "remote". Default is all. Abbreviation with first letters acceptable.
#' @param linksFile name of the file we're updating in the meta/ subfolder; default="teaching-materials.xlsx". gdrive *CaseSensitive!
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param sortOutput logical; if T, outputs are sorted by grade, part, filetype, then filename
#' @param returnWorkbook Logical; if T, returns the list which was written to linksFile
#' @export
#'
updateTeachingMatLinks<-function(gh_proj_name,
                                 dataCat = c("download",  "remote", "classroom"),
                                 linksFile = "teaching-materials.xlsx",
                                 WD = getwd(),
                                 sortOutput = T,
                                 returnWorkbook = F
){


  #if WD supplied, append it to destFolder
  linksFile <-  fs::path(WD,"meta", linksFile)

  if(missing(gh_proj_name)){
    current_data<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))
        if(is_empty(current_data$GitHubPath)){stop("Please enter the GitHub Project Name (gh_proj_name)")
        }else{
          gh_proj_name<-current_data$GitHubPath %>% basename %>% tools::file_path_sans_ext()
          }
    #exception if this is the galacticPubs project
    if(gh_proj_name=="galacticPubs"){gh_proj_name<-current_data$ShortTitle}
    }

#define dplyr::coalesce function that doesn't crash with 2 NAs!
  discardNA<-function(x,y){
    if(is.null(x)|is.null(y)){list(x,y)[[which(c(!is.null(x),!is.null(y)))]]}else{
    sapply(1:length(x),function(i){
      if(is.na(x[i])&is.na(y[i])){NA}else{ifelse(!is.na(x[i]),x[i],y[i])}
    })
  }}

# try to avoid OutOfMemoryError w/ options from https://stackoverflow.com/questions/7963393/out-of-memory-error-java-when-using-r-and-xlconnect-package
    options(java.parameters = "-Xmx1024m")


  ### CONSTANTS ###
  #useful later for figuring out filetypes of dribble listings
  mimeKey<-googledrive::drive_mime_type(googledrive::expose())
  #Create key for connecting the dataCat file categories to the tabs on the .xlsx file.
    tmKey<-data.frame(dataCat=c("download","classroom","classroom","remote","remote"),subCat=c("","presentations","handouts","presentations","handouts"),tab=c("dL","c_pres","c_handouts","r_pres","r_handouts"))
    #if shorthand was used in dataCat (e.g. 1st letter), make correction
    dataCat.orig<-dataCat
    dataCat<-unique(tmKey$dataCat)[pmatch(dataCat,unique(tmKey$dataCat))]
    #filter key to only the selected dataCats
    tmKey.selected<-tmKey[tmKey$dataCat %in% dataCat,]
  #If sorting output, what's the order of column sorting?
    sortStringL<-list(dL=c("envir","grades","part"),
                      c_pres=c("stage","grades","part","type"),
                      c_handouts=c("stage","grades","part","type","SvT"),
                      r_pres=c("stage","grades","part","type"),
                      r_handouts=c("stage","grades","part","type","SvT")
                      )
  #################


# Query Google Drive to find locations ------------------------------------

    currLessonDir<-drive_find_path(paste0("Edu/lessons/",gh_proj_name))
    currLessonDirID<-currLessonDir$id

    #Test if valid "gh_proj_name" was provided
    if(nrow(currLessonDir)==0){stop(paste0("No lessons matching \"",gh_proj_name,"\" found."))}else{
      message("\n>> ",nrow(currLessonDir)," lesson(s) found: ",paste(currLessonDir$name),"\n")
    }

    assembledMatDirID<-googledrive::drive_find(q=paste0("name= 'assembled-lesson-materials' and mimeType='application/vnd.google-apps.folder' and '",currLessonDirID,"' in parents"))$id
    assembledMatDribble<-googledrive::drive_find(q=paste0("mimeType='application/vnd.google-apps.folder' and '",assembledMatDirID,"' in parents"))


###########################################################################
# gDrive Data Extraction Step for all dataCats ----------------------------
    # Lapply "loop" through all the dataCat elements (downloads, quickPrep, classroom, remote)
    gData0 <- pbapply::pblapply(1:length(dataCat),FUN=function(k){
      dataCat_k<-dataCat[k]

      # get Download links for dL tab
      if(dataCat_k=="download"){
          excelTab<-tmKey$tab[match(paste(dataCat_k,""),paste(tmKey$dataCat,tmKey$subCat))]

          # lapply "loop" across classroom and remote teaching environments that exist in the folder
          existingEnvirs<-c("classroom","remote")[c("classroom","remote") %in% assembledMatDribble$name]
          dLdata<-lapply(existingEnvirs, function(envir) {
            # Get Folder link for entire environment (all grades)
            envirIndex <- which(assembledMatDribble$name == envir)
            envirDirLink <-
              assembledMatDribble[envirIndex, "drive_resource"][[1]][[1]]$webViewLink
            # Get folder links for all grade subdirectories
            gradeDirDribble <-
              googledrive::drive_find(
                q = paste0(
                  "mimeType='application/vnd.google-apps.folder' and '",
                  assembledMatDribble$id[envirIndex],
                  "' in parents"
                )
              )
            #just get the numbers at the end of the folder name, allowing for
            #y, g, years, grades, etc.
            grades <- gsub("^.*_[a-zA-Z ]*([\\d-]*)", "\\1", gradeDirDribble$name)

            #lapply for each grade
            out.grades <- lapply(1:nrow(gradeDirDribble), function(i) {
              gradeDirLink_i <-
                gradeDirDribble[i, "drive_resource"][[1]][[1]]$webViewLink
              #output each grade's sub-dataframe
              data.frame(
                envir = envir,
                grades = grades[i],
                part = "all",
                path = paste0("/", fs::path(envir, gradeDirDribble$name[i]), "/"),
                filetype = "folder",
                gDriveLink = gradeDirLink_i,
                gID = gradeDirDribble$id[i],
                excelTab = excelTab
              )
            })
            #make a root data frame for the link to all parts, all grades
            out.root <-
              data.frame(
                envir = envir,
                grades = "all",
                part = "all",
                path = paste0("/", envir, "/"),
                filetype = "folder",
                gDriveLink = envirDirLink,
                gID = assembledMatDribble$id[envirIndex],
                excelTab = excelTab
              )
            #combine everything
            dplyr::bind_rows(out.root, do.call(dplyr::bind_rows, out.grades))
          })#End dLdata lapply
          #output Download Link matrix
          do.call(dplyr::bind_rows,dLdata)

          }else{

        ########
        #---> From here, applies to all data cats, except downloads

        #google drive ID for the current data cat folder
        dirID<-assembledMatDribble$id[which(substr(assembledMatDribble$name,1,3)==substr(dataCat_k,1,3))]
        #if the directory is missing for this dataCat, return ()
        if(length(dirID)==0){return(NA)}
        dirDribble<-googledrive::drive_find(q=paste0("'",dirID,"' in parents"))

          gradeDirs<-dirDribble[sapply(dirDribble$drive_resource,function(x) x$mimeType=="application/vnd.google-apps.folder"),]

          #Test if there are no grade directories for this data category
          if(nrow(gradeDirs)==0){return(NA)}

          grades0<-gsub("^.*_[a-zA-Z ]*([\\d-]*)","\\1",gradeDirs$name)
          grades<-grades0[which(grades0!="")]

          # lapply loop for grades
          out.grades<-lapply(1:nrow(gradeDirs),function(i){
            gradeDirID<-gradeDirs$id[i]
            gradeDirDribble_i<-googledrive::drive_find(q=paste0("'",gradeDirID,"' in parents"))

            #lapply "loop" for materials categories (presentations & handouts)
            existingCategories<-c("presentations","handouts")[c("presentations","handouts") %in% gradeDirDribble_i$name]
            out.category<-pbapply::pblapply(existingCategories,function(category){
              excelTab<-tmKey$tab[match(paste(dataCat_k,category),paste(tmKey$dataCat,tmKey$subCat))]
              currCatFiles<-googledrive::drive_find(q=paste0("'",gradeDirDribble_i$id[which(gradeDirDribble_i$name==category)],"' in parents"))
              currCatFiles$mimeTypes<-sapply(currCatFiles$drive_resource,function(x){x$mimeType})
              #remove .gitkeep and other files from currCatFiles
              currCatFiles<-subset(currCatFiles,currCatFiles$mimeTypes!="application/octet-stream")

              #Check for Null current category file list
              if(nrow(currCatFiles)==0){
              return(NA)
              }else{
                currCatFiles$filetype<-mimeKey$human_type[match(currCatFiles$mimeTypes,mimeKey$mime_type)]

                #extract part numbers from _P1_ in file name
                (part_extr<-gsub("^.*[^\\d](_[Pp][^_ \\d]*)(\\d*)[ &_$]*.*$","\\2",currCatFiles$name,fixed=F,perl=T))
                #only return part number if it's numeric; else return ""
                currCatFiles$part<-sapply(part_extr,function(x) {
                  if(is.na(as.numeric(x))){
                    message("Part number not extracted from file: \n -'",x,"'")
                    ""
                    }else{x}})
                #does same thing as googledrive::drive_link(), but eh, why not
                currCatFiles$link<-sapply(currCatFiles$drive_resource,function(x) x$webViewLink)
                baseLink<-gsub("(.*\\/)[edit|view].*$","\\1",currCatFiles$link,perl=T)

                currCatFiles$gShareLink<-sapply(1:nrow(currCatFiles),function(i){
                  row_i<-currCatFiles[i,]
                  # PDF share links can't handle template/preview suffix
                  ifelse(row_i$filetype=="pdf",baseLink[i], paste0(baseLink[i],"template/preview"))
                })

                  if(category=="handouts"){
                    #extract SvT (student vs teacher) version from filename
                    currCatFiles$SvT<-ifelse(grepl(".*(TEACHER|STUDENT).*",toupper(currCatFiles$name),perl=T),
                            gsub(".*(TEACHER|STUDENT).*","\\1",toupper(currCatFiles$name),perl=T),NA)

                    #PDF export syntax is different for google presentations compared to google docs :/
                    pdfExportString<-ifelse((currCatFiles$filetype=="ppt"|currCatFiles$filetype=="pptx"|currCatFiles$filetype=="presentation"),"export/pdf","export?format=pdf")
                    # for shared resources that are already PDFs, avoid making an export pdf string...
                    currCatFiles$pdfLink<-ifelse(currCatFiles$filetype=="pdf",baseLink,paste0(baseLink,pdfExportString))

                    #for all handouts, add a pdfLink...For remote lessons, we don't (yet) have access to distrLink for cloudinary.
                    # This needs to be added manually
                    data.frame(
                      dataCat = dataCat_k,
                      grades = grades[i],
                      part = currCatFiles$part,
                      SvT = currCatFiles$SvT,
                      filename = currCatFiles$name,
                      filetype = currCatFiles$filetype,
                      gDriveLink = currCatFiles$link,
                      gID = currCatFiles$id,
                      gShareLink = currCatFiles$gShareLink,
                      pdfLink = currCatFiles$pdfLink,
                      excelTab = excelTab
                    )
                  }else{
                    #for presentations, there's no student/teacher versioning
                    currCatFiles$SvT=NA
                    #different share links for the 2 environments
                    if(dataCat_k=="classroom"){
                      currCatFiles$gPresentLink<-paste0(baseLink,"present")
                      data.frame(
                        dataCat = dataCat_k,
                        grades = grades[i],
                        part = currCatFiles$part,
                        SvT = currCatFiles$SvT,
                        filename = currCatFiles$name,
                        filetype = currCatFiles$filetype,
                        gDriveLink = currCatFiles$link,
                        gID = currCatFiles$id,
                        gShareLink = currCatFiles$gShareLink,
                        gPresentLink = currCatFiles$gPresentLink,
                        excelTab = excelTab
                      )
                    }else{
                      #for remote environment, we don't have access to nearpod share links to create nShareLink
                      data.frame(
                        dataCat = dataCat_k,
                        grades = grades[i],
                        part = currCatFiles$part,
                        SvT = currCatFiles$SvT,
                        filename = currCatFiles$name,
                        filetype = currCatFiles$filetype,
                        gDriveLink = currCatFiles$link,
                        gID = currCatFiles$id,
                        gShareLink = currCatFiles$gShareLink,
                        excelTab = excelTab
                      )
                    }
                  }# end handout vs. presentation handling
              }#end check for null currCatFile
            })#end out.category lapply

            out.category
          })
          names(out.grades)<-paste0("g",grades)
          out.grades
          }#end "remote/classroom" else{}
        # }#end "not download" else{}

    })#end gData def
    names(gData0)<-dataCat

###########################################################################

    message("Gdrive file info imported")


# now, reorganize gData by XLtab ------------------------------------------
# Melt and reforge as a big ol' tibble
gData<-reshape2::melt(gData0) %>% dplyr::tibble() %>% suppressMessages()


     #Read in tabs from the teaching-materials.xlsx for selected data types


    # redefine tmKey.selected to remove empty dataCat(egorie)s----------------------
    badCats<-sapply(gData0,function(x) identical(x,NA))
    badCatNames<-names(badCats)[which(badCats==TRUE)]
    tmKey.selected2<-tmKey.selected %>% dplyr::filter(!.data$dataCat%in%badCatNames)

    #Read in tabs and remove rows that are totally NA
    tmImported<-lapply(1:length(tmKey.selected2$tab),function(i) {
      d<-openxlsx::read.xlsx(linksFile,sheet=tmKey.selected2$tab[i],startRow=2)
      rmNArows(d)
    })

    #add google drive ID for merging to imported data
    tmImported2<-lapply(1:length(tmImported),function(i) {
      #regex accounts for diff. link structures for downloads links (/folders/) vs file sharing (/d/)
      tmImported[[i]]$gID <- gsub("^.*\\/(?:d|folders)\\/([^\\/\n]*)\\/?.*$","\\1",tmImported[[i]]$gDriveLink)
      tmImported[[i]]
      })
    names(tmImported2)<-tmKey.selected2$tab

###########################################
# merge data and write to appropriate tab ---------------------------------

    ### open the whole workbook to be edited

    tmXLSX<-XLConnect::loadWorkbook(linksFile)


# Merge missing gDrive data into existing teaching-materials.xlsx  --------
    # lapply loop for each excelTab in the imported list
    tmImported.merged<-lapply(1:length(tmImported2),function(i){


      excelTab_i<-names(tmImported2)[i]
      dataCat_i<-tmKey$dataCat[which(tmKey$tab==excelTab_i)]

      gData_i<-gData %>% dplyr::filter(.data$excelTab==excelTab_i)%>% dplyr::tibble() %>% dplyr::mutate(updateNotes=NA)

         #The X filter is in case there's an untitled column with some stuff in it that accidentally gets imported
      excelData_i_0<-tmImported2[[i]] %>% dplyr::tibble() %>% dplyr::select(!dplyr::starts_with("X"))

      #Overwrite all data on dL tab
      if(excelTab_i=="dL") {
        mergedData_i <- gData_i[, names(excelData_i_0)]

      #In all other tabs, preserve title field
      } else{
        #overwrite intersecting names
        #which columns overlap (besides filename)?
        insct <-intersect(names(excelData_i_0),
                          names(gData_i))[-which(intersect(names(excelData_i_0),
                                                           names(gData_i)) =="filename")]

        #filter out excel data with filenames that aren't found on the web
        excelData_i <-excelData_i_0 %>% dplyr::filter(.data$filename %in% gData_i$filename)

        #Now merge, which should preserve titles
        mergedData_i <-  hard_left_join(excelData_i,gData_i,by="filename",as_character=TRUE)

      }



      #Sort output if requested, otherwise output whatcha got
      if(sortOutput){
        sortnames<-as.vector(sortStringL[which(names(sortStringL)==excelTab_i)])
        indx<-match(unlist(sortnames),names(mergedData_i))
        out0 <- mergedData_i %>% dplyr::arrange(dplyr::across(indx))
      }else{out0<-mergedData_i}
      #remove NA rows
      out<-rmNArows(out0)

      #If title missing, try to guess from filename
      #
      if("title"%in%names(out)){
      out$title<-sapply(1:nrow(out),function(i){
        row_i<-out[i,]
        if(is.na(row_i$title)){
            # if wksht or worksheet in title, return "Worksheet"
            descriptor_wksht<-ifelse(grepl(".*(wo?r?kshe?e?t).*", row_i$filename, ignore.case = TRUE),
                                     "Worksheet",
                                     NA)
            descriptor_handout<-ifelse(grepl(".*(handout).*", row_i$filename, ignore.case = TRUE),
                                     "Worksheet",
                                     NA)
            #parts, accommodating (1&2) multipart descriptors
            partpat<-"^.*[^\\d](_[Pp][^_ \\d]*)(\\d*[^_]*).*$"
            descriptor_parts<-ifelse(grepl(partpat,row_i$filename,perl=TRUE),
                                     gsub(partpat,"\\2",row_i$filename,fixed=F,perl=T),
                                     NA)

            descriptor<-c(row_i$SvT,descriptor_wksht,descriptor_handout,ifelse(!is.na(descriptor_parts),paste0("(P",descriptor_parts,")"),NA)) %>% unique_sans_na()

          title_guess<-paste(ifelse(!is.na(descriptor),descriptor,""),collapse=" ")
          title_guess
        }else{
          row_i$title
        }
      })
      }


      #overwrite old data with final (this is not writing to the hard drive until the saveWorkbook stage)
      XLConnect::writeWorksheet(tmXLSX,sheet=excelTab_i,data=out,startCol=1,startRow=3,header=F)

      #output final tab dataset
      out

    })#end tmImported.merged lapply


    names(tmImported.merged)<-names(tmImported2)
    XLConnect::saveWorkbook(tmXLSX,linksFile)

    message(linksFile," Updated and Saved")

    if(returnWorkbook){message(">> Link Updating Complete")
      return(tmImported.merged)
      }else{message(">> Link Updating Complete")}

}
