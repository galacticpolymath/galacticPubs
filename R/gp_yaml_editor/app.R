#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Helper function for markdown text
md_txt <- function(label,txt){
    if(label==""){
    shiny::markdown(txt)
    }else{
      #remove spaces from end of label and add a colon
    shiny::markdown(paste0(c(paste0('#### ',gsub("[ ]*$","", label),':'),txt)))
    }
}

# Helper function for lumping separate markdown/YAML entries (which are separated for end user continuity)
# into a single list item for the JSON output for the web
lumpItems<-function(items,item.labs,list.obj,new.name){

     applicable00<- match(items,names(list.obj))
     #remove empty items (not just NA)
     applicable0<-as.vector(na.omit(ifelse(list.obj[applicable00]=="",NA,applicable00)))
     applicable <- names(list.obj)[applicable0]
     applicableLabs<-item.labs[as.vector(na.omit(match(items,applicable)))]
     lumped<-sapply(1:length(applicable),function(i){
              # add H4 to label (only if there is a label provided)
              paste0(ifelse(applicableLabs[i]=="","",
                            paste0("#### ",applicableLabs[i],"\n")),
                     list.obj[applicable[i]])
              }) %>%  paste(list.obj[applicable],collapse="\n")
     #remove lumped list items
     first.applicable<-sort(applicable0)[1]
     #rearrange to insert the lumped section
     out<-list.obj
     out[first.applicable]<-lumped #replace first applicable value with new item
     names(out)[first.applicable]<-new.name #rename inserted item according to user defined var new.name
     #remove remaining lumped columns (by name to avoid index issues)
     OUT<-out[-(sort(applicable0)[-1])]
     OUT
    }#end lumpItems()


library(shiny);library(shinythemes)
#import when editor is run from galacticPubs package
default_y_args<-c("Title","author","date","updated","SponsoredBy","Subtitle","EstLessonTime","ForGrades","TargetSubject","Text","Tags")

#find path to meta/ folder, which depends on whether running it in development enviro or from galacticPubs
#then import existing yaml or NULL
potential_paths <- c(paste0(getwd(), "/meta/"),
                    "../../meta/")
#if meta_path_test[2] TRUE, running in development enviro (wd is the app subfolder)
meta_path_test<-sapply(potential_paths,function(x) dir.exists(x))
meta_path <- potential_paths[which(meta_path_test==TRUE)]
yaml_path<-paste0(meta_path,"front-matter.yml")
yaml_test<-file.exists(yaml_path)
WD<-ifelse(meta_path_test[2],"../../","") #WD is working directory
if(yaml_test==FALSE){
    warning(paste("Failed to import `meta/front-matter.yml`\n  *You're starting from scratch.*"))
    y<-yaml::read_yaml(paste0(meta_path,"front-matter_TEMPLATE.yml")) #sapply(default_y_args,function(x) x="",simplify=F)
}else{

    y<-yaml::read_yaml(yaml_path, eval.expr =TRUE)
}
img_loc<-paste0(getwd(),"/www/",collapse="/")
#create image preview directory
dir.create(img_loc,showWarnings =FALSE)

print(y)

# UI SECTION --------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("yeti"),
    title = "GP Lesson Editor",
    position="fixed-top",
# Save Button--------------------------------------------------
    header = div(class="header_save",
    # Define custom CSS styles
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    div(class="header_button_container",
        #save time stamp to left of button
        span(class="yaml_update", htmlOutput("confirm_yaml_update")),
        #save button
        actionButton('save', div(class="header_button_container",
            img(src = 'gpicon.ico'),
            p(strong("Save"))
        )))
    ),#end header

# TAB 1: EDIT -------------------------------------------------------------
    tabPanel(
        "Edit",

        # Boxes need to be put in a row (or column)
        # div(img(src = "GPlogo.png", width = 400), style = "padding:10px"),
        # div(id="box",class="info",
        #   p("Edit lesson title, overview, tags, etc. for lessons",style="font-weight:500;color:#3e0055;")
        #   ),
        h3('Step 1: Enter "Front Matter" Overview Info'),
        p(class="help-text",
            "Most text fields accept",
            a("markdown formatting", href = "https://www.markdownguide.org/basic-syntax/")
        ),
        textInput(
            inputId = "Title",
            label = "Title",
            value = y$Title
        ),
        textInput(
            inputId = "Subtitle",
            label = "Subtitle",
            value = y$Subtitle
        ),
        textInput(
            inputId = "ShortTitle",
            label = "ShortTitle",
            value = y$ShortTitle
        ),
        dateInput(
            inputId = "PublicationDate",
            label = "Publication Date",
            value = y$PublicationDate
        ),
        textInput("LessonBanner","Lesson Banner",
                  value=ifelse(
                          y$LessonBanner=="",
                          list.files(paste0(WD,"assets/banners_etc",collapse="/"),
                                             pattern="^.*[lesson|Lesson].*[b|B]anner.*\\.[png|PNG|jpeg|jpg]",
                                        full.names=T),

                        y$LessonBanner
                      )),
        textAreaInput("SponsoredBy","Sponsored By: (Add multiple entries with `- `, i.e. hyphen+space)",y$SponsoredBy),

        textAreaInput("SponsorLogo",label="Sponsor Logo(s)— (add images to assets/orig-client-media_NoEdit; reorder as needed for multiple logos)",
                  value=ifelse(
                      y$SponsorLogo=="",
                      yaml::as.yaml(list.files(fs::path(WD,"/assets/orig-client-media_NoEdit/"),full.names=T,
                                         pattern="^.*[Ll]ogo.*\\.[png|PNG|jpeg|jpg]")),
                      y$SponsorLogo)
                    ),
        textAreaInput("LearningEpaulette","Learning Epaulette",
                  value=ifelse(
                          y$LearningEpaulette=="",
                          yaml::as.yaml(list.files(fs::path(WD,"/assets/learning-plots/"),
                                             pattern="^.*[Ee]paulet.*\\.[png|PNG|jpeg|jpg]",
                                        full.names=T)),
                        y$LearningEpaulette
                      )),
        textAreaInput("LearningChart","Learning Chart (Shows much lower on Preview page, with Standards)",
                  value=ifelse(
                          y$LearningEpaulette=="",
                           yaml::as.yaml(list.files(fs::path(WD,"/assets/learning-plots/"),
                                             pattern="^.*[cC]hart.*\\.[png|PNG|jpeg|jpg]",
                                        full.names=T)),

                        y$LearningChart
                      )),
        textInput(inputId = "ForGrades",
                  label = "For Grades",
                  value = y$ForGrades),
        textInput(inputId = "TargetSubject",
                  label = "Target Subject",
                  value = y$TargetSubject),
        textInput(
            "EstLessonTime",
            "Estimated Lesson Time",
            value = y$EstLessonTime,
            placeholder = "format= '3 x 45 min'"
            ),
        #text block
        htmlOutput("overview_text_block"),

        selectizeInput("Tags",label="Tags:",choices=y$Tags,selected=y$Tags,options=list(create=TRUE),multiple=TRUE),
        h3("Step 2:"),
        p(
            "Choose which GP webquest you want to grade or upload your own custom key"
        ),
        fluidRow(style = "padding-left:5em", {

        }),
        # h3("Step 3:"),
        # verbatimTextOutput("console"),
        h3("Step 3:"),

        hr()
    ),
    #End Setup Panel

# TAB 2: PREVIEW ----------------------------------------------------------

    tabPanel("Preview",
             htmlOutput("preview"))


)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SERVER LOGIC ------------------------------------------------------------
server <- function(input, output) {

    # output$x<-renderPrint(y)


# Save YAML & JSON when button clicked -------------------------------------------
    doIT<-observe({
    #read in existing front-matter.yml if it exists (just to be sure we're up to date)
    if(file.exists(yaml_path)){y<-yaml::read_yaml(yaml_path, eval.expr =TRUE)}

    # operational input variables we don't want to output
    op_var<-c("save")
    #make nonreactive list of everything except our "Operational" input items
    Y <- reactiveValuesToList(input)[!names(input) %in% op_var]
    #import YAML template to get a canonical order for output
    #template file is in 'extdata/'  ('inst/extdata' if you're developing package)
    template_fields<-names(yaml::read_yaml(system.file("extdata","front-matter_TEMPLATE.yml",package="galacticPubs")))
      # #debugging example
      # template_fields<-template_fields[-c(2,9)]

    ## Test template versions for matching fields (ignoring NAs for fields in template, but not in 'input')
    Y_order_indx0<-as.vector(na.omit(match(template_fields,names(Y))))
    #if the template doesn't have values for a given input, give a warning
    if(sum(is.na(Y_order_indx0))>0){warning("Your template ver: ",y$TemplateVer,
                                           " is missing the field(s):\n\t- ",paste0(names(Y)[which(is.na(Y_order_indx0))],collapse="\n\t- "),
                                           "\nUpdate galacticPubs to upgrade your template to ensure fields are in the right order.")}

    #Put any missing fields that are in 'input', but not the template yml, at the end
    Y_order_indx<-Y_order_indx0
    for(i in 1:length(Y_order_indx)){if(is.na(Y_order_indx[i])){Y_order_indx[i]<-max(Y_order_indx,na.rm=T)+1}}
    #Now we have a robust index vector to reorder Y before outputting to YAML
    Y2<-Y[Y_order_indx]
    #Finally, preserve any manually added fields on YML before overwriting
    y_not_in_Y2<-which(is.na(match(names(y),names(Y2))))
    Y3<-Y2 #initialize w/ old vector (for loop is destructive)
    for(i in y_not_in_Y2){
      #Add y value at beginning or insert it among Y3 fields, 1 by 1
      if(i==1){Y3<-c(y[i],Y3)
      }else{Y3<-c(Y3[1:(i-1)],y[i],
                  if((i-1)==length(Y3)){}else{Y3[i:length(Y3)]})}
    }

    yaml::write_yaml(lapply(Y3,function(x)as.character(x)), paste0(meta_path,"front-matter.yml"))

    ##Create list for JSON output (a little different, bc we want to combine some YAML sections to simplify web output of similar text types)
    # first combine some parts to have desired flexible JSON output

    Y3_lumped<-lumpItems(items=c("DrivingQ","EssentialQ","LearningObj","MiscMD"),
                  item.labs=c("Driving Question","Essential Question(s)","Learning Objective(s)",""),
                  list.obj=Y3,
                  new.name="Text")
    # browser()

    jsonlite::write_json(Y3_lumped,paste0(meta_path,"JSON/front-matter.json"))
    output$confirm_yaml_update <-
        renderText(paste0(
            "front-matter.yml&json updated:<br>",
            format(Sys.time(), "%Y-%b-%d %r")
        ))
    }) %>% bindEvent(input$save)

  output$overview_text_block<-renderUI({
    div(class="text-block",p(class="text-block-title",strong("These sections combined in JSON output as 'Text'")),
          textAreaInput("DrivingQ","Driving question(s):",y$DrivingQ),
          textAreaInput("EssentialQ",
                        a("Essential question(s):",
                          href="https://www.authenticeducation.org/ae_bigideas/article.lasso?artid=53"),
                        y$EssentialQ),
          textAreaInput("LearningObj","Learning Objective(s):",y$LearningObj),
          textAreaInput("MiscMD","Additional text. (Create header with '#### Hook:' & start '- First point' on new line",y$MiscMD)
        )
  })

  #####################################
  # Output the preview of the lesson plan
  output$preview<-renderUI({
    #copy images over to www folder for previewing
    fs::file_copy(input$LessonBanner,img_loc,overwrite=TRUE)
    # browser()
    items2copy<-c("SponsorLogo","LearningEpaulette","LearningChart")
    items2copy_filenames<-lapply(items2copy,function(x) {yaml::yaml.load(input[[x]])})
    names(items2copy_filenames)<-items2copy
    #Test if all the files to copy exist; otherwise through a useful error
    lapply(1:length(items2copy_filenames),function(i){
      filez<-items2copy_filenames[[i]]
      errs<-!file.exists(filez)
      if(sum(errs)>0){
        stop("The following files for field '",names(items2copy_filenames)[[i]],
             "' do not exist:\n\t- ",
             paste(items2copy_filenames[[i]][errs],collapse="\n\t- ")
        )
      }else{
        fs::file_copy(filez,img_loc,overwrite=TRUE)
      }
    })

    sponsoredByTxt<-yaml::yaml.load(input$SponsoredBy)
      # print(h2(shiny::markdown(paste0(c('Driving Question(s):',input$DrivingQ)))))
     list(
        div(style = "margin-top: 60px;"),
        h2(input$Title),
        h5(input$Subtitle),
        img(class="lesson-banner",src=basename(input$LessonBanner)),
        div(class="sponsor-section",
            h4("Sponsored by:"),
        lapply(1:max(length(sponsoredByTxt),length(yaml::yaml.load(input$SponsorLogo))),function(i){
            div(class="sponsor",
            span(class="sponsor-text",shiny::markdown(sponsoredByTxt[i])),
            div(class="sponsor-logo-container",
            img(class="sponsor-logo",src=basename(yaml::yaml.load(input$SponsorLogo)[i]))
            ))})
        ),
        div(class="section",h1("1. Overview")),
        div(class="stats",
          img(class="learning-epaulette",src=basename(input$LearningEpaulette)),
          div(class="triad border-right",
            md_txt('Target subject',input$TargetSubject)
          ),
          div(class="triad border-right",
            md_txt('For grades',input$ForGrades)
            ),
          div(class="triad",
            md_txt("Est. Lesson Time", input$EstLessonTime))
            ),

        md_txt('Driving Question(s)',input$DrivingQ),
        md_txt('Essential Question(s)',input$EssentialQ),
        md_txt('Learning Objective(s)',input$LearningObj),
        md_txt('',input$MiscMD),
        # browser(),
        div(class="keyword-cloud",lapply(input$Tags,function(x){span(class="keyword",x)}))
    )
  })


}

# Run the application
shinyApp(ui = ui, server = server)
