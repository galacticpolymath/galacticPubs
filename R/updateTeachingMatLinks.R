#' updateTeachingMatLinks
#'
#' Adds/updates Google Drive share links to the requested tabs in teaching-materials.xlsx. If a file is found on Google Drive, it will add it to the appropriate tab, with a timestamp in updateNotes. If a listed google drive link doesn't match a found, an updateNote will be added "trashed?"
#'
#' Just a note that I'm using 2 libraries to handle XLSX files. Not ideal, but the imported data from openxlsx::read.xlsx is nicer than XLConnect::readWorksheetFromFile, but the editing of data from a complex spreadsheet is MUCH better in XLConnect.
#'
#' @param shortTitle The unique short title of this lesson which is prefixed on the lesson folder name in the shared
#' @param linksFile file location of the lesson teaching-resource-links XLSX worksheet. gdrive; *CaseSensitive!
#' @param dataCat which info do you want to merge with your teaching-materials spreadsheet? Options= "quickPrep_feedback", "download", "classroom" and "remote". Default is all. Abbreviation with first letters acceptable.
#' @param returnWorkbook Logical; if T, returns the list which was written to linksFile
#' @export
#'
updateTeachingMatLinks<-function(shortTitle,linksFile="meta/teaching-materials.xlsx",dataCat=c("download","quickPrep_feedback","remote","classroom"),returnWorkbook=F){


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
    tmKey<-data.frame(dataCat=c("quickPrep_feedback","download","classroom","classroom","remote","remote"),subCat=c("","","presentations","handouts","presentations","handouts"),tab=c("quickPrep_Fb","dL","c_pres","c_handouts","r_pres","r_handouts"))
    #if shorthand was used in dataCat (e.g. 1st letter), make correction
    dataCat.orig<-dataCat
    dataCat<-unique(tmKey$dataCat)[pmatch(dataCat,unique(tmKey$dataCat))]
    #filter key to only the selected dataCats
    tmKey.selected<-tmKey[tmKey$dataCat %in% dataCat,]
  #needed to
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
          dLdata<-lapply(existingEnvirs,function(envir){
          # Get Folder link for entire environment (all grades)
          envirIndex<-which(assembledMatDribble$name==envir)
          envirDirLink<-assembledMatDribble[envirIndex,"drive_resource"][[1]][[1]]$webViewLink
          # Get folder links for all grade subdirectories
          gradeDirDribble<-googledrive::drive_find(q=paste0("mimeType='application/vnd.google-apps.folder' and '",assembledMatDribble$id[envirIndex],"' in parents"))
          grades<-gsub("^.*_(.*)","\\1",gradeDirDribble$name)

          #lapply for each grade
          out.grades<-lapply(1:nrow(gradeDirDribble),function(i){
            gradeDirLink_i<-gradeDirDribble[i,"drive_resource"][[1]][[1]]$webViewLink
            #output each grade's sub-dataframe
            data.frame(envir=envir,grades=grades[i],part="all",path=paste0("/",fs::path(envir,gradeDirDribble$name[i]),"/"),filetype="folder", gDriveLink=gradeDirLink_i,gID=gradeDirDribble$id[i],excelTab=excelTab)
          })
          #make a root data frame for the link to all parts, all grades
          out.root<-data.frame(envir=envir,grades="all",part="all",path=paste0("/",envir,"/"),filetype="folder",gDriveLink=envirDirLink,gID=assembledMatDribble$id[envirIndex],excelTab=excelTab)
          #combine everything
          rbind(out.root,do.call(rbind,out.grades))
            })#End dLdata lapply
          #output Download Link matrix
          do.call(rbind,dLdata)

          }else{
        #google drive ID for the current data cat folder
        dirID<-assembledMatDribble$id[which(substr(assembledMatDribble$name,1,3)==substr(dataCat_k,1,3))]
        dirDribble<-googledrive::drive_find(q=paste0("'",dirID,"' in parents"))

        #handle quickprep "data category"
        if(dataCat_k=="quickPrep_feedback"){
            excelTab<-tmKey$tab[match(paste(dataCat_k,""),paste(tmKey$dataCat,tmKey$subCat))]
            #If the filename has "-part 1-" in it, extract part, otherwise put an NA (i.e. for forms)
            qp_part<-ifelse(grepl("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*",dirDribble$name,perl=T),
                        gsub("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*","\\1",dirDribble$name,perl=T),NA)
            qp_links<-sapply(dirDribble$drive_resource,function(x) {x$webViewLink})
            qp_mimeTypes<-sapply(dirDribble$drive_resource,function(x){x$mimeType})
            qp_filetypes<-mimeKey$human_type[match(qp_mimeTypes,mimeKey$mime_type)]
            qp_baseLink<-gsub("(.*\\/)[edit|view].*$","\\1",qp_links,perl=T)

            qp_gPresentLink=paste0(qp_baseLink,"present")
            # Make PresentLink NA for non-presentation files
            qp_gPresentLink[which(googledrive::drive_reveal(dirDribble,"mime_type")$mime_type!="application/vnd.google-apps.presentation")]<-NA

            #output Quickprep & Feedback Table
            data.frame(dataCat=dataCat_k,grades=NA,part=qp_part,filename=dirDribble$name,filetype=qp_filetypes,
                       gDriveLink=qp_links,gPresentLink=qp_gPresentLink,gID=dirDribble$id,excelTab=excelTab)

        #handle all remote and classroom environments differently
        }else{
          gradeDirs<-dirDribble[sapply(dirDribble$drive_resource,function(x) x$mimeType=="application/vnd.google-apps.folder"),]
          grades0<-gsub("^.*_(.*)","\\1",gradeDirs$name)
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
              #extract part numbers from _P1_ in file name
              part<-ifelse(grepl("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*",currCatFiles$name,perl=T),
                        gsub("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*","\\1",currCatFiles$name,perl=T),NA)
              link<-sapply(currCatFiles$drive_resource,function(x) x$webViewLink)
              baseLink<-gsub("(.*\\/)[edit|view].*$","\\1",link,perl=T)
              currCatFiles_mimeTypes<-sapply(currCatFiles$drive_resource,function(x){x$mimeType})
              filetype<-mimeKey$human_type[match(currCatFiles_mimeTypes,mimeKey$mime_type)]
              #extract SvT (student vs teacher) version from filename
              gShareLink<-paste0(baseLink,"template/preview")
                if(category=="handouts"){
                  SvT<-ifelse(grepl(".*(TEACHER|STUDENT).*",currCatFiles$name,perl=T),
                          gsub(".*(TEACHER|STUDENT).*","\\1",currCatFiles$name,perl=T),NA)
                  pdfLink<-gShareLink<-paste0(baseLink,"export?format=pdf")
                  #for all handouts, add a pdfLink...For remote lessons, we don't (yet) have access to distrLink for cloudinary.
                  # This needs to be added manually
                  data.frame(dataCat=dataCat_k,grades=grades[i],part=part,SvT=SvT,filename=currCatFiles$name,filetype=filetype,gDriveLink=link,gID=currCatFiles$id,gShareLink=gShareLink,pdfLink=pdfLink,excelTab=excelTab)
                }else{
                  #for presentations, there's no student/teacher versioning
                  SvT=NA
                  #different share links for the 2 environments
                  if(dataCat_k=="classroom"){
                    gPresentLink<-paste0(baseLink,"present")
                    data.frame(dataCat=dataCat_k,grades=grades[i],part=part,SvT=SvT,filename=currCatFiles$name,filetype=filetype,gDriveLink=link,gID=currCatFiles$id,gShareLink=gShareLink,gPresentLink=gPresentLink,excelTab=excelTab)
                  }else{
                    #for remote environment, we don't have access to nearpod share links to create nShareLink
                    data.frame(dataCat=dataCat_k,grades=grades[i],part=part,SvT=SvT,filename=currCatFiles$name,filetype=filetype,gDriveLink=link,gID=currCatFiles$id,gShareLink=gShareLink,excelTab=excelTab)
                  }
                }# end handout vs. presentation handling
            })#end out.category lapply
            names(out.category)<-existingCategories
            out.category
          })
          names(out.grades)<-paste0("g",grades)
          out.grades
          }#end "remote/classroom" else{}
        }#end "not download" else{}

    })#end gData def
    names(gData0)<-dataCat

###########################################################################

    message("Gdrive file info imported")


# now, reorganize gData by XLtab ------------------------------------------
gData<-reshape2::melt(gData0) %>% dplyr::tibble() %>% suppressMessages()



     #Read in tabs from the teaching-materials.xlsx for selected data types
    #Remove rows that are totally NA
    tmImported<-lapply(1:length(tmKey.selected$tab),function(i) {
      d<-openxlsx::read.xlsx(linksFile,sheet=tmKey.selected$tab[i],startRow=2)
      rmNArows(d)
    })

    #add google drive ID for merging to imported data
    tmImported2<-lapply(1:length(tmImported),function(i) {
      tmImported[[i]]$gID <- gsub("^.*\\/d\\/([^\\/]*)\\/.*$","\\1",tmImported[[i]]$gDriveLink)
      tmImported[[i]]
      })
    names(tmImported2)<-tmKey.selected$tab

###########################################
# merge data and write to appropriate tab ---------------------------------

    ### open the whole workbook to be edited

    tmXLSX<-XLConnect::loadWorkbook(linksFile)


# Merge missing gDrive data into existing teaching-materials.xlsx  --------
    # lapply loop for each excelTab in the imported list
    tmImported.merged<-lapply(1:length(tmImported2),function(i){

      excelData_i<-tmImported2[[i]] %>% dplyr::tibble()
      excelTab_i<-names(tmImported2)[i]

      dataCat_i<-tmKey$dataCat[which(tmKey$tab==excelTab_i)]


      gData_i<-gData %>% dplyr::filter(.data$excelTab==excelTab_i)%>% dplyr::tibble() %>% dplyr::mutate(updateNotes=NA)

      #add missing data to Spreadsheet dataframe from gDrive meta info
      excel_Data_i.aug <- dplyr::left_join(excelData_i,gData_i,by="gID") #%>% print(Inf)

      #which columns overlap (besides gID)? which will need to be coalesced
      insct<-intersect(names(excelData_i),names(gData_i))[-which(intersect(names(excelData_i),names(gData_i))=="gID")]


        if(nrow(excel_Data_i.aug)>0){

          #merge .x and .y into 1 coalesced column for variables that overlap
          #(added as new colName (without .x/.y); those cols still there)

          for (colName in insct){
          fullColNames<-paste0("excel_Data_i.aug$",colName,c(".x",".y"))
          replacement<-discardNA(x=eval(parse(text=fullColNames[1])),y=eval(parse(text=fullColNames[2])))
            #create new merged column
          excel_Data_i.aug[,colName]<-replacement
            }

          #Find out which rows in excel data are not actually on the drive (maybe trashed or moved)
          trashed<-dplyr::anti_join(excelData_i,gData_i,by="gID")
          #add updateNotes for trashed
          excel_Data_i.aug$updateNotes[excel_Data_i.aug$gID%in%trashed$gID]<-"trashed?"
        }


      #which rows exist on gDrive, but not the Excel sheet, and should be added?
      missing<-dplyr::anti_join(gData_i,excelData_i,by="gID")


      #if there are missing data, prep a compatible dataframe
        if(nrow(missing)>=1){
        #make missing a compatible df to bind to final df
        missing.df<-matrix(nrow=nrow(missing),ncol=ncol(excelData_i)) %>% as.data.frame()
        names(missing.df)<-names(excelData_i)
        missing.df[,insct]<-missing[,insct]
        missing.df$updateNotes<-date()
          #if the excel data isn't empty, bind the old excel data with the new missing data from gdrive info
          if(nrow(excel_Data_i.aug)>0){
            final<-rbind(excel_Data_i.aug[names(excelData_i)],missing.df)
          #if excel data is empty, the only data to output is the missing data
          }else{
             final<-missing.df
          }
        #otherwise, the output data is just the augmented excel data (with bits filled in from gdrive info)
        }else{
          final<-excel_Data_i.aug[,names(excelData_i)]
        }



      #overwrite old data with final
      XLConnect::writeWorksheet(tmXLSX,sheet=excelTab_i,data=final,startCol=1,startRow=3,header=F)

      #output final tab dataset
      final

    })#end tmImported.merged lapply


    names(tmImported.merged)<-names(tmImported2)
    XLConnect::saveWorkbook(tmXLSX,linksFile)

    message(linksFile," Updated and Saved")

    if(returnWorkbook){tmImported.merged}else{message(">> Link Updating Complete")}

}
