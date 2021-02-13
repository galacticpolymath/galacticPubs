

#GP Palette
gpPal=list(main=
  data.frame(name=c("hydrogen blue","lightning purple","flare fucsia","atomic blue","burst purple", "galactic black","sparkle white"),hex=c("#2c83c3","#6c2d82","#ff3dac","#6812d1","#cb1f8e","#363636","#f0f4ff"),rgb=c("rgb(44,131,195,maxColorValue=255)","rgb(108,45,130,maxColorValue=255)","rgb(255,61,172,maxColorValue=255)","rgb(104,18,209,maxColorValue=255)","rgb(203,31,142,maxColorValue=255)","rgb(54,54,54,maxColorValue=255)","rgb(240,244,255,maxColorValue=255)")),
  extended=data.frame(name=c("dark hydrogen blue","hydrogen blue","light hydrogen blue","dark lightning purple","lightning purple","light lightning purple","flare fucsia","light flare fucsia","pale flare fucsia","dark atomic blue","atomic blue","light atomic blue","dark burst purple","burst purple","light burst purple","galactic black","sparkle white"),
                      hex=c("#005792","#2c83c3","#69b2f6","#3E0D55","#6c2d82","#9c5ab2","#ff3dac","#ff5dbe","#ff78de","#291c9e","#6812d1","#9f4bff","#96195F","#cb1f8e","#c7247d","#363636","#f0f4ff"),
                      rgb=c("rgb(0,87,46,maxColorValue=255)","rgb(44,131,195,maxColorValue=255)","rgb(105,178,246,maxColorValue=255)","rgb(62,513,85,maxColorValue=255)","rgb(108,45,130,maxColorValue=255)","rgb(156,90,178,maxColorValue=255)","rgb(255,61,172,maxColorValue=255)","rgb(255,93,190,maxColorValue=255)","rgb(255,120,222,maxColorValue=255)","rgb(41,28,158,maxColorValue=255)","rgb(104,18,209,maxColorValue=255)","rgb(159,75,255,maxColorValue=255)","rgb(150,25,95,maxColorValue=255)","rgb(203,31,142,maxColorValue=255)","rgb(199,36,125,maxColorValue=255)","rgb(54,54,54,maxColorValue=255)","rgb(240,244,255,maxColorValue=255)")),
  subjects=data.frame(
    name=c("math","ela","socstudies","science","extra"),
    hex=c("#db4125","#eca14d","#633a9a","#b798e8","#f4f0d9"),
    rgb=c("rgb(219,65,37,maxColorValue=255))","rgb(236,161,77,maxColorValue=255))","rgb(99,58,154,maxColorValue=255))","rgb(183,152,232,maxColorValue=255)","rgb(244,240,217,maxColorValue=255))"))
  )


#Show the GP palette
# subset is the first n colors to show or x1:x2 or c(1,3,5) for a specific subset
# pal is 1 by default; if 2, shows extended palette
show.gpPal<-function(pal=1,subset=NULL,...){
  P<-gpPal[[pal]]$hex
  if(length(subset!=0)){
    s <- if(length(subset)==1){eval(quote(1:subset))}else{eval(quote(subset))}
    labs<-subset
    }else{
      s=1:length(P)
      labs<-1:length(P)}
  nColors<-length(s)
  Pnames<-gpPal[[pal]]$name
  Ptitle<-names(gpPal)[pal]


  colorspace::swatchplot(P[s],...)
  whereAtStart<-1/nColors/2
  whereAt<-seq(whereAtStart,1-whereAtStart,1/nColors)
  graphics::mtext(labs,side=1,line=0,at=whereAt)
  graphics::mtext(paste0("palette = ",Ptitle),side=3,line=2,0,font=1)
  }

#output a vector of hex codes, given named colors; list=TRUE lists all options
gpColors<-function(colorNames,list=FALSE){
  allPal.0<-do.call(dplyr::bind_rows,gpPal)
  allPal<-subset(allPal.0,!duplicated(allPal.0$name))
  if(list==TRUE){cat("GP PALETTE OPTIONS",paste0(rep("-",20),collapse=""),"\n",paste(allPal[,1],collapse=", "))}
  indx=charmatch(colorNames,allPal$name)
  colVec<-allPal$hex[indx]
  names(colVec)<-allPal$name[indx]
  return(colVec)
}


#These are default shapes
gpShps<-c(21,24,22,23,8,13,9)


show.gpShapes<-function(){
plot(0:length(gpShps),0:length(gpShps),pch=gpShps,cex=2,xlim=c(-1,length(gpShps)+1),ylim=c(-1,length(gpShps)+1))
}

ggGalactic<-function(font="Montserrat",regular.wt=400,bold.wt=700,font.cex=1,plot.margin=ggplot2::margin(t=10,r=10,b=10,l=10)){
  showtext::showtext_auto()
  fam=font
  sysfonts::font_add_google(name=font,family=fam,regular.wt=regular.wt,bold.wt=bold.wt)
ggplot2::theme_linedraw()+ggplot2::theme(
    text=ggplot2::element_text(family=font),
    plot.margin=plot.margin,
    plot.title=ggplot2::element_text(family=font,size=30*font.cex,face="bold",color=gpPal[[1]]$hex[6]),
    plot.subtitle=ggplot2::element_text(family=font,size=22*font.cex,color=gpPal[[1]]$hex[5]),
    axis.title=ggplot2::element_text(family=font,size=28*font.cex,face="bold",color=gpPal[[1]]$hex[6]),
    axis.text=ggplot2::element_text(family=font,size=18*font.cex,color=gpPal[[1]]$hex[6]),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 10, b = 0, l = 0)),
    legend.text=ggplot2::element_text(family=font,color=gpPal[[1]]$hex[6],size=18*font.cex),
    legend.title=ggplot2::element_text(family=font,color=gpPal[[1]]$hex[6],face="bold",size=18*font.cex),
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


