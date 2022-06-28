#' hard_left_join
#'
#' Merge one dataframe into another, replacing data frame 1's values with matching values from data frame 2 without a bunch of .x .y column duplication BS. This is a destructive function, so back up your data and make sure you're getting the expected output.
#'
#' @param df1 the data frame (or tibble) whose structure you want to preserve
#' @param df2 the data frame with new data you want to crush over df1 (destroying matching cells in df1)
#' @param by a single column name you want to use to match df1 and df2 values
#' @param as_character logical; do you want to override classes and make everything characters. This will avoid errors of matching incompatible types, but may have unexpected results for some data sets. Default=F
#' @return A data frame that has the structure of df1, with matching ids and columns replaced with df2 values
#' @export

hard_left_join<-function(df1,df2,by,as_character=FALSE){
  if(missing(by)){stop("Specify 'by' column for matching df1 and df2")}

  if(as_character){
    df1<-df1  %>% apply(MARGIN=2,function(x){as.character(x)})
    df2<-df2  %>% apply(MARGIN=2,function(x){as.character(x)})
  }
  df1<-as.data.frame(df1)
  df2<-as.data.frame(df2)

  #intersecting (non-id) columns
  ixn<-intersect(names(df1),names(df2))
  ixn <- ixn[!ixn%in%by]

  df1_rows_with_matches<-df1[,by] %in% df2[,by] %>% which()

  #matching rows
  df2_matching_rows<-match(df1[,by],df2[,by]) %>% na.omit() %>% as.vector()

  #do the replacement
  df3<-df1

  df3[df1_rows_with_matches,ixn] <- df2[df2_matching_rows,ixn]

  df3 %>% dplyr::as_tibble()

}
