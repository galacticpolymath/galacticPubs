#' make_printable
#'
#' Knit a concise, formatted PDF version of the lesson plan
#'
#' @param WD is working directory of the project; default= getwd()
#' @param dest_folder where do you want template and knitted files to go? default= "assets/lesson-plan-markdown"
#' @param rebuild do you want to delete files in the dest_folder and start over? default=NULL
#' @param showframe do you want boxes to be drawn to show margins? default= FALSE
#' @param open do you want to open rendered PDF file? default=TRUE
#' @export

make_printable=function(WD=getwd(),dest_folder,rebuild=NULL, showframe=FALSE, open=TRUE){
  if(missing(dest_folder)){
    dest_folder <- fs::path(WD,"assets","lesson-plan-markdown")
  }else{dest_folder<-fs::path(WD,dest_folder)}
  clear<-ifelse(is.null(rebuild),FALSE,rebuild)

# 1) Prep template files in assets folder -------------------------------------------
 toCopy<-list.files( system.file("markdown_template", package ="galacticPubs"),full.names = TRUE,include.dirs = FALSE,recursive=TRUE)
 if(!dir.exists(dest_folder)){dir.create(dest_folder);message("\nDirectory created:\n@ ",dest_folder,"\n")}

icons_folder<-fs::path(dest_folder,"static_icons")
if(!dir.exists(icons_folder)){dir.create(icons_folder);message("\nDirectory created:\n@ ",icons_folder,"\n")}

  img_folder<-fs::path(dest_folder,"dynamic_images")
 if(!dir.exists(img_folder)){dir.create(img_folder);message("\nDirectory created:\n@ ",img_folder,"\n")}

  #copy lesson images to img_folder subfolder, deleting everything if rebuild is triggered
  stageAssets(WD=WD,clear=clear,dest_folder=img_folder)

  #copy latest template files into the directory, clearing it if rebuild has been triggered
  toCopy_base_files<-toCopy[!grepl("static_icons",toCopy)]
  toCopy_icons<-toCopy[grepl("static_icons",toCopy)]
  copy_updated_files(toCopy_base_files,dest_folder=dest_folder,clear=clear)
  copy_updated_files(toCopy_icons,dest_folder=icons_folder,clear=clear)


# 2) Overwrite header.yml from current_data ----------------------------------
 if(!file.exists(fs::path(WD,"meta","front-matter.yml"))){stop("front-matter.yml not found")}
 fm<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))
 #Read in latest header template to overwrite
 header0<-header<-safe_read_yaml(system.file("markdown_template","00-header.yml", package ="galacticPubs"))
 #Merge galacticPubs front-matter fields into 00-header.yml subset of fields
matching_fields<-names(header0)[names(header0)%in%names(fm)]
header[matching_fields]<-fm[matching_fields]

#Clean up/redefine some things
 if(!is_empty(header$LessonBanner)){
  header$LessonBanner<-fs::path("dynamic_images",basename(header$LessonBanner))
 }
 header$SponsorLogo<-basename(header$SponsorLogo[1:3])
 #replace missing logos (up to 3) with spacer PNGs
 header$SponsorLogo <- sapply(header$SponsorLogo,function(x) {ifelse(is.na(x),fs::path("static_icons","transp_logo_spacer.png"),fs::path("dynamic_images",x))},USE.NAMES = FALSE)

 # Combine Driving Qs, etc. into 1 markdown blob; then convert that blob to latex
 header$OverviewText<-lumpItems(
            c("DrivingQ", "EssentialQ", "LearningObj", "MiscMD"),
            item.labs = c(
              "Driving Question(s):",
              "Essential Question(s):",
              "Learning Objective(s):",
              ""
            ),
            list.obj=fm,
            new.name = "Text"
          )$Text %>%  commonmark::markdown_latex()

 # showframe logic for troubleshooting typesetting
 if(showframe){header$`header-includes`<-"\\usepackage[letterpaper,total={6.5in, 9in},showframe]{geometry}"}


 #Write updated header with lesson-specific meta data
 yaml::write_yaml(header,fs::path(dest_folder,"00-header.yml"))


# 3) Generate updated figures where necessary -----------------------------
  # Make & Save QR Code
 pdf( fs::path(img_folder,"lesson_QR.pdf"))
 plot(qrcode::qr_code(fm$URL))
 dev.off()


# 4) Make separate Rmd sections -------------------------------------------



# 5) Merge RMd files ------------------------------------------------------
join_rmd(fs::path("assets","lesson-plan-markdown"),WD=WD)



# 6) Render PDF -----------------------------------------------------------

rmarkdown::render(fs::path(dest_folder,"lesson_plan.Rmd"))


if(open){fs::file_show(fs::path(dest_folder,"lesson_plan.pdf"))}


 }
