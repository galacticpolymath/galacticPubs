#' updateTeachingMatLinks
#'
#' Adds/updates Google Drive share links to the requested tabs in teaching-materials.xlsx. If a file is found on Google Drive, it will add it to the appropriate tab, with a timestamp in updateNotes. If a listed google drive link doesn't match a found, an updateNote will be added "trashed?"
#'
#' Just a note that I'm using 2 libraries to handle XLSX files. Not ideal, but the imported data from openxlsx::read.xlsx is nicer than XLConnect::readWorksheetFromFile, but the editing of data from a complex spreadsheet is MUCH better in XLConnect.
#'
#' @param shortTitle The unique short title of this lesson which is prefixed on the lesson folder name in the shared
#' @param dataCat which info do you want to merge with your teaching-materials spreadsheet? Options= "download", "classroom" and "remote". Default is all. Abbreviation with first letters acceptable.
#' @param linksFile name of the file we're updating in the meta/ subfolder; default="teaching-materials.xlsx". gdrive *CaseSensitive!
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param sortOutput logical; if T, outputs are sorted by grade, part, filetype, then filename
#' @param returnWorkbook Logical; if T, returns the list which was written to linksFile
#' @export
#'
updateTeachingMatLinks<-function(shortTitle,
                                 dataCat = c("download",  "remote", "classroom"),
                                 linksFile = "teaching-materials.xlsx",
                                 WD = getwd(),
                                 sortOutput = T,
                                 returnWorkbook = F
){


  #if WD supplied, append it to destFolder
  linksFile <-  fs::path(WD,"meta", linksFile)

  if(missing(shortTitle)){
    current_data<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))
    shortTitle<-current_data$ShortTitle
    if(is_empty(shortTitle)){stop("Please enter a lesson shortTitle")}
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
    EduDirID<-googledrive::drive_find(q=paste0("name='Edu' and mimeType= 'application/vnd.google-apps.folder' and 'root' in parents"))$id
    LessonsDirID<-googledrive::drive_find(q=paste0("name='Lessons' and mimeType= 'application/vnd.google-apps.folder' and '",EduDirID,"' in parents"))$id
    currLessonDir<-googledrive::drive_find(q=paste0("mimeType='application/vnd.google-apps.folder' and '",LessonsDirID,"' in parents"),pattern=shortTitle)
    currLessonDirID<-currLessonDir$id

    #Test if valid "shortTitle" was provided
    if(length(currLessonDir)==0){stop(paste0("No lessons matching \"",shortTitle,"\" found."))}else{
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
                currCatFiles$part<-ifelse(grepl("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*",currCatFiles$name,perl=T),
                          gsub("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*","\\1",currCatFiles$name,perl=T),NA)
                currCatFiles$link<-sapply(currCatFiles$drive_resource,function(x) x$webViewLink)
                baseLink<-gsub("(.*\\/)[edit|view].*$","\\1",currCatFiles$link,perl=T)
                currCatFiles$gShareLink<-paste0(baseLink,"template/preview")
                  if(category=="handouts"){
                    #extract SvT (student vs teacher) version from filename
                    currCatFiles$SvT<-ifelse(grepl(".*(TEACHER|STUDENT).*",toupper(currCatFiles$name),perl=T),
                            gsub(".*(TEACHER|STUDENT).*","\\1",toupper(currCatFiles$name),perl=T),NA)
                    #PDF export syntax is different for google presentations compared to google docs :/
                    pdfExportString<-ifelse((currCatFiles$filetype=="ppt"|currCatFiles$filetype=="pptx"|currCatFiles$filetype=="presentation"),"export/pdf","export?format=pdf")
                    # for shared resources that are already PDFs, avoid making an export pdf string...
                    currCatFiles$pdfLink<-ifelse(currCatFiles$filetype=="pdf",baseLink,paste0(baseLink,pdfExportString))
                    #
                    currCatFiles$gShareLink=ifelse(currCatFiles$filetype=="pdf",NA,baseLink)
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


      excelData_i<-tmImported2[[i]] %>% dplyr::tibble()

      #Overwrite all data on dL tab
      if(excelTab_i=="dL") {
        mergedData_i <- gData_i[, names(excelData_i)]

      #In all other tabs, preserve title field
      } else{
        #overwrite intersecting names
        #which columns overlap (besides filename)?
        insct <-intersect(names(excelData_i),
                          names(gData_i))[-which(intersect(names(excelData_i),
                                                           names(gData_i)) =="filename")]
        browser()
        #filter out excel data with filenames that aren't found on the web
        excelData_i <-excelData_i %>% dplyr::filter(.data$filename %in% gData_i$filename) %>% dplyr::mutate(dplyr::across(.fns=as.character))

        #Set all these columns in excelData_i to NULL
        excelData_i[, insct] <- NULL

        #Now merge, which should preserve titles
        mergedData_i <-
          dplyr::full_join(excelData_i,
                           gData_i,
                           by = "filename")
      }



      #Sort output if requested, otherwise output whatcha got
      if(sortOutput){
        sortnames<-as.vector(sortStringL[which(names(sortStringL)==excelTab_i)])
        indx<-match(unlist(sortnames),names(mergedData_i))
        out0 <- mergedData_i %>% dplyr::arrange(dplyr::across(indx))
      }else{out0<-mergedData_i}
      #remove NA rows
      out<-rmNArows(out0)

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
