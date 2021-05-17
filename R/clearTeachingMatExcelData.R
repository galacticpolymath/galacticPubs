#' clearTeachingMatExcelData
#'
#' Caution! Deletes data for the selected sheet(s) in the teaching-materials.xlsx workbook
#'
#' @param tabsToClear a character vector; which tab(s) to delete everything but the first 2 header rows; Options=c("quickPrep_Fb","dL","c_pres","c_handouts","r_pres","r_handouts"). Default is "all".
#' @export
#'
#'
clearTeachingMatExcelData <- function(tabsToClear="all"){
  tabOpts=c("quickPrep_Fb","dL","c_pres","c_handouts","r_pres","r_handouts")
  if(tabsToClear=="all"){tabs<-tabOpts}else{
    tabs<-tabOpts[pmatch(tabsToClear,tabOpts)]
  }

  #load workbook
  #R1C1:R1000C20
  deleteRange<-c(3,1,1000,20)
  fileLoc<-"meta/teaching-materials.xlsx"
  backupLoc<-"meta/teaching-materials_backup.xlsx"

  XLC=NULL#to avoid error check build warning in setStyleAction
  teachingMat<- XLConnect::loadWorkbook(fileLoc)

  #save without overwriting formatting
  XLConnect::setStyleAction(teachingMat,XLC$"STYLE_ACTION.NONE")
  #save a backup
  XLConnect::saveWorkbook(teachingMat,backupLoc)
  sapply(tabs,function(xlTab){
  XLConnect::clearRange(teachingMat, sheet=xlTab,coords=deleteRange)
  message(paste0("  tab '",xlTab,"'  cleared"))
}) %>% invisible()
  XLConnect::saveWorkbook(teachingMat,fileLoc)
  message("\n  Backup Created at:\n  ",normalizePath(backupLoc),"\n")
  message("\n  Data Cleared From:\n  ",normalizePath(fileLoc),"\n")

}
