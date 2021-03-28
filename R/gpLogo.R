#' gpLogo
#'
#' add a Galactic Polymath logo to a ggplot2 object
#'
#' @param ggObj the ggObject you want to add a logo to
#' @param xNPC x coordinate (0,1) for start of logo
#' @param yNPC y coordinate (0,1) for start of logo
#' @param which which logo you want? One of: "grad_logo_gradTrans","grad_logo_gradWhite", "horiz_logoWords_gradTrans", "horiz_logoWords_gradWhite", "horiz_logoWords_whiteAblue","horiz_logoWords_whiteBlack"
#' @param size how big should the logo be as a proportion (0,1); 0.1 by default
#' @param cloudinaryString any cloudinary processing codes to modify the PNG? \href{https://cloudinary.com/documentation/transformation_reference}{Ref Link}
#' @export

gpLogo<-function(ggObj,xNPC=.9,yNPC=.9,which="horiz_logoWords_gradWhite",size=.1,cloudinaryString=NULL){
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
  # "https://res.cloudinary.com/galactic-polymath/image/upload/v1593317568/GP_logo_wordmark_horiz_white_blackBG_600_fjj1ii.png"
  logoImg<-png::readPNG(RCurl::getURLContent(newURL))


   ggObj+ggplot2::annotation_custom(grid::rasterGrob(logoImg,x=grid::unit(xNPC,"npc"),y=grid::unit(yNPC,"npc"),height=grid::unit(size,"npc")))+if(xNPC>1|yNPC>1|xNPC<0|yNPC<0){ggplot2::coord_cartesian(clip = "off")}else{}

}


