

#' ggGalactic
#'
#' Galactic Polymath stylings for ggplot2
#'
#' @param font Google font to use, "Montserrat" by default
#' @param regular.wt font weight for regular font style
#' @param bold.wt font weight for bold text
#' @param font.cex a simple multiplier for scaling all text
#' @param font.col color of all axis label and title text
#' @param plot.margin easy access to ggplot margins
#' @export


ggGalactic<-function(font="Montserrat",regular.wt=400,bold.wt=700,font.cex=1,font.col="#363636",plot.margin=ggplot2::margin(t=10,r=10,b=10,l=10)){
  gpPal=NULL
  utils::data(gpPal)
  showtext::showtext_auto()
  fam=font
  sysfonts::font_add_google(name=font,family=fam,regular.wt=regular.wt,bold.wt=bold.wt)
ggplot2::theme_linedraw()+ggplot2::theme(
    text=ggplot2::element_text(family=font),
    plot.margin=plot.margin,
    plot.title=ggplot2::element_text(family=font,size=30*font.cex,face="bold",color=font.col),
    plot.subtitle=ggplot2::element_text(family=font,size=22*font.cex,color=gpPal[[1]]$hex[5]),
    axis.title=ggplot2::element_text(family=font,size=28*font.cex,face="bold",color=font.col),
    axis.text=ggplot2::element_text(family=font,size=18*font.cex,color=font.col),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=ggplot2::element_text(family=font,color=font.col,size=18*font.cex),
    legend.title=ggplot2::element_text(family=font,color=font.col,face="bold",size=18*font.cex),
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

}


