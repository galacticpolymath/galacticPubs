

#' ggGalactic
#'
#' Galactic Polymath stylings for ggplot2; deprecated and moved to galacticEdTools package; use galacticEdTools::theme_galactic() instead
#'
#' @param grid.thickness.maj How heavy do you want grid lines to be? (in case printer makes things lighter); default=.8
#' @param grid.thickness.min How heavy do you want grid lines to be? (in case printer makes things lighter); default=.6
#' @param grid.col What color do you want the grid to be? Default: same as font (#363636)
#' @param border.thickness How heavy do you want the plot border to be?
#' @param border.col  Color of plot border. Default: same as font (#363636)
#' @param font Google font to use, "Montserrat" by default; see options with sysfonts::font_families_google()
#' @param regular.wt font weight for regular font style
#' @param bold.wt font weight for bold text
#' @param font.cex a simple multiplier for scaling all text
#' @param axis.lab.col color of axis labels (and title)
#' @param axis.text.col color of axis text (numbers, dates, etc)
#' @param plot.margin easy access to ggplot margins
#' @export


ggGalactic<-function(grid.thickness.maj=.7,grid.thickness.min=.4,grid.col="#C3C3C3",border.thickness=1.8,border.col="#6D6D6D",font="Montserrat",regular.wt=400,bold.wt=700,font.cex=1,axis.lab.col="#363636",axis.text.col="#6D6D6D",plot.margin=ggplot2::margin(t=10,r=10,b=10,l=10)){
  gpPal=NULL
  utils::data(gpPal,package="galacticPubs")
  showtext::showtext_auto()
  fam=font
    #Only try to download font if online and not already available
  if(is.na(match(font,sysfonts::font_families()))){
    isOnline=RCurl::url.exists("https://www.google.com")
    if(isOnline){
        tryCatch(
        sysfonts::font_add_google(name=font,family=fam,regular.wt=regular.wt,bold.wt=bold.wt),
        error=function(e) cat("\nFont: '",font,"' unavailable."))
    }else{
        cat("\nYou don't seem to be online. Can't download your requested font.")
    }
  }


ggplot2::theme_linedraw()+ #base theme to modify
  ggplot2::theme(
    text=ggplot2::element_text(family=font),
    panel.border=ggplot2::element_rect(size=border.thickness,colour=border.col),
    panel.grid.major=ggplot2::element_line(size=grid.thickness.maj,colour = grid.col),
    panel.grid.minor=ggplot2::element_line(size=grid.thickness.min,colour = grid.col),
    plot.margin=plot.margin,
    plot.title=ggplot2::element_text(family=font,size=30*font.cex,face="bold",color=axis.lab.col),
    plot.subtitle=ggplot2::element_text(family=font,size=22*font.cex,color=gpPal[[1]]$hex[5]),
    axis.title=ggplot2::element_text(family=font,size=28*font.cex,face="plain",color=axis.lab.col),
    axis.text=ggplot2::element_text(family=font,size=18*font.cex,color=axis.text.col),
    axis.ticks=ggplot2::element_line(color=grid.col,size=grid.thickness.maj),
    # axis.ticks.length=ggplot2::unit(axis.tick.length,"pt"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=ggplot2::element_text(family=font,color=axis.text.col,size=18*font.cex),
    legend.title=ggplot2::element_text(family=font,color=axis.lab.col,face="bold",size=18*font.cex),
    legend.position = "right", legend.text.align = 0, legend.background =ggplot2::element_blank()
  )
}

gpLogo<-function(ggObj,xNPC=.9,yNPC=.9,which="horiz_logoWords_GradWhite",size=.1,cloudinaryString=NULL){
  logoFile=switch(which,
    grad_logo_gradTrans="https://res.cloudinary.com/galactic-polymath/image/upload/v1593304396/logos/GP_logo_grad_transBG_300_tbn4ei.png",
    grad_logo_gradWhite="https://res.cloudinary.com/galactic-polymath/image/upload/b_white/v1593304396/logos/GP_logo_grad_transBG_300_tbn4ei.png",
    horiz_logoWords_gradTrans="https://res.cloudinary.com/galactic-polymath/image/upload/v1593304395/logos/GP_logo_wordmark_horiz_grad_transBG_300_lqdj7q.png",
    horiz_logoWords_gradWhite="https://res.cloudinary.com/galactic-polymath/image/upload/b_white/v1593304395/logos/GP_logo_wordmark_horiz_grad_transBG_300_lqdj7q.png",
    horiz_logoWords_whiteAblue="https://res.cloudinary.com/galactic-polymath/image/upload/v1593316226/logos/GP_logo_wordmark_horiz_white_aBlueBG_300_qmuas0.png",
    horiz_logoWords_whiteBlack="https://res.cloudinary.com/galactic-polymath/image/upload/v1594949366/logos/GP_logo_wordmark_horiz_white_blackBG_600_buwnlf.png",
    "Error"
  )

  if(logoFile=="Error"){stop("That's not one of the logo file options")}

  #Handle additional cloudinary parameters
  if(!is.null(cloudinaryString)){
    #test if already Cloudinary string in URL
    noCloudString=stringr::str_detect(logoFile,"upload\\/v")

    if(noCloudString){
    #Add strings
    splitStr<-stringr::str_split(logoFile,"upload\\/v",simplify=T)
    newURL<-paste0(splitStr[1],"upload/",cloudinaryString,"/v",splitStr[2])
    }else{
    #Add to existing strings
    extractStr0<-stringr::str_extract(logoFile,"upload\\/.*\\/v")
    extractStr<-gsub("/v","",extractStr0)
    splitStr<-stringr::str_split(logoFile,"upload\\/.*\\/v",simplify=T)
    newURL<-paste0(splitStr[1],extractStr,",",cloudinaryString,"/v",splitStr[2])
    }
  }else{newURL<-logoFile}

  #read in logo
  "https://res.cloudinary.com/galactic-polymath/image/upload/v1593317568/GP_logo_wordmark_horiz_white_blackBG_600_fjj1ii.png"
  logoImg<-png::readPNG(RCurl::getURLContent(newURL))


   ggObj+ggplot2::annotation_custom(grid::rasterGrob(logoImg,x=ggplot2::unit(xNPC,"npc"),y=ggplot2::unit(yNPC,"npc"),height=ggplot2::unit(size,"npc")))+if(xNPC>1|yNPC>1|xNPC<0|yNPC<0){ggplot2::coord_cartesian(clip = "off")}else{}
   cat("galacticPubs::ggGalactic() is deprecated\nYou should use galacticEdTools::theme_galactic()")

}


