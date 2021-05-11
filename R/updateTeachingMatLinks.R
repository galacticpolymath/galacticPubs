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
updateTeachingMatLinks<-function(shortTitle,linksFile="meta/teaching-materials.xlsx",dataCat=c("quickPrep_feedback","download","remote","classroom"),returnWorkbook=F){


#define dplyr::coalesce function that doesn't crash with 2 NAs!
  discardNA<-function(x,y){
    if(is.null(x)|is.null(y)){list(x,y)[[which(c(!is.null(x),!is.null(y)))]]}else{
    sapply(1:length(x),function(i){
      if(is.na(x[i])&is.na(y[i])){NA}else{ifelse(!is.na(x[i]),x[i],y[i])}
    })
  }}


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



  #Extract filenames, links, etc. for all
    lessonFolder<-googledrive::drive_ls("~/Edu/Lessons/",pattern=shortTitle)
    lessonFolderPath<-paste0("~/Edu/Lessons/",lessonFolder$name)
    materialsPaths<-fs::path(lessonFolderPath,"assembled-lesson-materials/",dataCat)

###########################################################################
# gDrive Data Extraction Step for all dataCats ----------------------------
    # Lapply "loop" through all the dataCat elements (downloads, quickPrep, classroom, remote)
    out.dataCat <- pbapply::pblapply(1:length(dataCat),FUN=function(k){
      dataCat_k<-dataCat[k]

      # get Download links for dL tab
      if(dataCat_k=="download"){
          excelTab<-tmKey$tab[match(paste(dataCat_k,""),paste(tmKey$dataCat,tmKey$subCat))]
          assembledPath<-fs::path(lessonFolderPath,"assembled-lesson-materials/")
          assembledDribble<-googledrive::drive_ls(assembledPath)

          # lapply "loop" across classroom and remote teachign environments
          dLdata<-lapply(c("classroom","remote"),function(envir){
          # Get Folder link for entire environment (all grades)
          envirFolderName <- paste0("/",envir,"/")
          envirFolderLink<-googledrive::drive_link(assembledDribble[which(assembledDribble$name==envir),])
          envir
          # Get folder links for all grade subdirectories
          gradeFolderDribble<-googledrive::drive_ls(assembledDribble[which(assembledDribble$name==envir),])%>% dplyr::filter(grepl(paste0(envir,"_"),.data$name))
          grades<-gsub("^.*_(.*)","\\1",gradeFolderDribble$name)
          out.grades<-lapply(1:nrow(gradeFolderDribble),function(i){
            gradeFolderLink_i<-googledrive::drive_link(gradeFolderDribble[i,])
            #output each grades sub-dataframe
            data.frame(envir=envir,grades=grades[i],part="all",filename=paste0("/",fs::path(envir,gradeFolderDribble$name[i]),"/"),filetype="folder", gDriveLink=gradeFolderLink_i,excelTab=excelTab)
          })
          #make a root data frame for the link to all parts, all grades
          out.root<-data.frame(envir=envir,grades="all",part="all",filename=envirFolderName,filetype="folder",gDriveLink=envirFolderLink,excelTab=excelTab)
          #combine everything
          rbind(out.root,do.call(rbind,out.grades))
            })#End dLdata lapply
          #output Download Link matrix
          do.call(rbind,dLdata)

          }else{
        #For all dataCats that are not "download"
        path<-materialsPaths[k]
        pathDribble<-googledrive::drive_ls(path)

        #handle quickprep "data category"
        if(dataCat_k=="quickPrep_feedback"){
            excelTab<-tmKey$tab[match(paste(dataCat_k,""),paste(tmKey$dataCat,tmKey$subCat))]
            #If the filename has "-part 1-" in it, extract part, otherwise put an NA (i.e. for forms)
            qp_part<-ifelse(grepl("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*",pathDribble$name,perl=T),
                        gsub("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*","\\1",pathDribble$name,perl=T),NA)
            qp_links<-googledrive::drive_link(pathDribble)
            qp_mimeTypes<-sapply(pathDribble$drive_resource,function(x){x$mimeType})
            qp_filetypes<-mimeKey$human_type[match(qp_mimeTypes,mimeKey$mime_type)]
            #output Quickprep & Feedback Table
            data.frame(dataCat=dataCat_k,grades=NA,part=qp_part,filename=pathDribble$name,filetype=qp_filetypes,gDriveLink=qp_links,excelTab=excelTab)

        #handle all remote and classroom environments differently
        }else{
          gradeFolders<-pathDribble %>% dplyr::filter(grepl(paste0(dataCat_k,"_"),.data$name))
          # dataCat<-sub("(^.*)_.*","\\1",gradeFolders$name[1])
          grades<-gsub("^.*_(.*)","\\1",gradeFolders$name)
          out.grades<-lapply(1:nrow(gradeFolders),function(i){
            gradeFolder_i_path<-fs::path(path,gradeFolders$name[i])
            gradeFolder_i<-googledrive::drive_ls(gradeFolder_i_path)

            #lapply "loop" for materials types (presentations & handouts)
            out.category<-pbapply::pblapply(c("presentations","handouts"),function(category){
              excelTab<-tmKey$tab[match(paste(dataCat_k,category),paste(tmKey$dataCat,tmKey$subCat))]
              currCatFiles<-googledrive::drive_ls(fs::path(gradeFolder_i_path,gradeFolder_i$name[which(gradeFolder_i$name==category)]))
              #extract part numbers from _P1_ in file name
              part<-ifelse(grepl("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*",currCatFiles$name,perl=T),
                        gsub("^.*[-_].*[P|p][^\\d]*(\\d*).*[_-].*","\\1",currCatFiles$name,perl=T),NA)
              links<-googledrive::drive_link(currCatFiles)
              currCatFiles_mimeTypes<-sapply(currCatFiles$drive_resource,function(x){x$mimeType})
              filetypes<-mimeKey$human_type[match(currCatFiles_mimeTypes,mimeKey$mime_type)]
              #extract SvT (student vs teacher) version from filename
              if(category=="handouts"){
                SvT<-ifelse(grepl(".*(TEACHER|STUDENT).*",currCatFiles$name,perl=T),
                        gsub(".*(TEACHER|STUDENT).*","\\1",currCatFiles$name,perl=T),NA)
                data.frame(dataCat=dataCat_k,grades=grades[i],part=part,SvT=tolower(SvT),filename=currCatFiles$name,filetype=filetypes,gDriveLink=links,excelTab=excelTab)
              }else{
              data.frame(dataCat=dataCat_k,grades=grades[i],part=part,SvT=NA,filename=currCatFiles$name,filetype=filetypes,gDriveLink=links,excelTab=excelTab)
              }
            })
            do.call(rbind,out.category)
          })
          do.call(rbind,out.grades)
          }#end "remote/classroom" else{}
        }#end "not download" else{}

    })#end out.dataCat def
###########################################################################

    message("Gdrive file info imported")

    #extract google id from ../d/IDNUMBER/... & add to each dataframe
    gData<-lapply(out.dataCat,function(L){
      L$gID<-gsub("^.*\\/d\\/([^\\/]*)\\/.*$","\\1",L$gDriveLink)
      L
      })
    names(gData)<-dataCat


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
    # try to avoid OutOfMemoryError w/ options from https://stackoverflow.com/questions/7963393/out-of-memory-error-java-when-using-r-and-xlconnect-package
    options(java.parameters = "-Xmx1024m")
    tmXLSX<-XLConnect::loadWorkbook(linksFile)


# Merge missing gDrive data into existing teaching-materials.xlsx  --------
    # lapply loop for each excelTab in the imported list
    tmImported.merged<-lapply(1:length(tmImported2),function(i){
      excelData_i<-tmImported2[[i]] %>% dplyr::tibble()
      excelTab_i<-names(tmImported2)[i]
      class(excelData_i$filename)<-"character"
      dataCat_i<-tmKey$dataCat[which(tmKey$tab==excelTab_i)]
      gData_i<-gData[[ dataCat_i ]] %>% dplyr::filter(.data$excelTab==excelTab_i)%>% dplyr::tibble()

      #add missing data to Spreadsheet dataframe from gDrive meta info
      excel_Data_i.aug <- dplyr::left_join(excelData_i,gData_i,by="gID") #%>% print(Inf)
        if(nrow(excel_Data_i.aug)>0){
          #which columns overlap (besides gID)? which will need to be coalesced
          insct<-intersect(names(excelData_i),names(gData_i))[-which(intersect(names(excelData_i),names(gData_i))=="gID")]

          #merge .x and .y into 1 coalesced column for variables that overlap
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

      excelCols<-names(excelData_i)[1:which(names(excelData_i)=="updateNotes")]
      #if there are missing data, prep a compatible dataframe
        if(nrow(missing)>=1){
        #make missing a compatible df to bind to final df
        missing.df<-matrix(nrow=nrow(missing),ncol=length(excelCols)) %>% as.data.frame()
        names(missing.df)<-excelCols
        missing.df[,intersect(excelCols,names(missing))]<-missing[,intersect(excelCols,names(missing))]
        missing.df$updateNotes<-date()
          #if the excel data isn't empty, bind the old excel data with the new missing data from gdrive info
          if(nrow(excel_Data_i.aug)>0){
            final<-rbind(excel_Data_i.aug[,excelCols],missing.df)
          #if excel data is empty, the only data to output is the missing data
          }else{
             final<-missing.df
          }
        #otherwise, the output data is just the augmented excel data (with bits filled in from gdrive info)
        }else{
          final<-excel_Data_i.aug[,excelCols]
        }

      # browser()


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
