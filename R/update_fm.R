#' update_fm (Update Front Matter)
#'
#' Checks for new galacticPubs front-matter_TEMPLATE.yml. If found, it will add new fields,  return the updated object, and write to drive (meta/front-matter.yml) if requested.
#'
#' If meta/front-matter.yml not found, it is created from the template. Will also combine language and country info to create locale, and add GPCatalogPath if those fields are blank.
#'
#' @param WD Working drive; default=getwd()
#' @param save_output do you want to save the updated front-matter to WD/meta/front-matter.yml? Default=TRUE
#' @param reorder do you want to reorder the resulting list, based on template order? default=TRUE
#' @param change_this A list of values to change in the front matter. Default=NULL. Example: list(RebuildAllMaterials=TRUE,Language="Italian) will trigger a full lesson rebuild when batchCompile() is run and change the Language and locale.
#' @return silently returns updated front-matter.yml object as a list
#' @export
#'

update_fm <- function(WD=getwd(),save_output=TRUE,reorder=TRUE,change_this=NULL){

  galacticPubs_template<-safe_read_yaml(system.file("extdata",
                                                    "front-matter_TEMPLATE.yml",
                                                    package ="galacticPubs"))

  yaml_path<-fs::path(WD,"meta","front-matter.yml")

  #create empty front-matter.yml from template if file not found
  if(!file.exists(yaml_path)){
    yaml::write_yaml(galacticPubs_template,yaml_path)
    message("\n*** meta/front-matter.yml not found at ",WD,"\n @ New file created from galacticPubs template ver.",galacticPubs_template$TemplateVer)
    new_yaml<-galacticPubs_template

  #Otherwise, update existing front-matter.yml
  }else{
  old_yaml<- safe_read_yaml(yaml_path)
  new_yaml <- add_missing_fields(old_yaml,galacticPubs_template,reorder=reorder)

  # Make manual changes if requested ----------------------------------------
  if(!is.null(change_this)){
    for(i in 1:length(change_this)){
      element_i<-names(change_this)[i]
      new_yaml[[element_i]]<-change_this[[i]]
    }
  }


  # If front-matter exists,  do certain routine processes -------------------
   #Add/Update the locale and lang fields with a nonexported internal function parse_locale()
   # overwrites existing lang and locale fields and returns the modified current_data list
    new_yaml <- new_yaml %>% galacticPubs:::parse_locale()

  #Add path to this lesson for once it's published to gp-catalog (if it doesn't exist)
    if(new_yaml$GPCatalogPath==""){
      repo<-whichRepo()
      new_yaml$GPCatalogPath<-catalogURL("LESSON.json",repo)
    }

  #test if it's a new version
  version_bumped<-old_yaml$TemplateVer!=galacticPubs_template$TemplateVer
  if(!version_bumped){
    # message("\nfront-matter.yml template v.",old_yaml$TemplateVer," is up-to-date with galacticPubs v.",as.character(utils::packageVersion("galacticPubs")))

  #otherwise change TemplateVer and let user know it's been upgraded
  }else{
    #reassign new templatever
    new_yaml$TemplateVer <- galacticPubs_template$TemplateVer
    message("\nfront-matter.yml template will be upgraded upon save: ",old_yaml$TemplateVer,"->",new_yaml$TemplateVer)
  }

  #Change LastUpdated field
  new_yaml$LastUpdated<-Sys.time() %>% as.character()


    #save updated file if requested
    if(save_output){
      yaml::write_yaml(new_yaml,yaml_path)
      message("\n@ Updated meta/front-matter.yml saved to disk.")
    }

  }

invisible(new_yaml)
}
