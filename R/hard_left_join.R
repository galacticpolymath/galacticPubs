#' hard_left_join
#'
#' Merge one dataframe into another, replacing data frame 1's values with matching values from data frame 2 without a bunch of .x .y column duplication BS. This is a destructive function, so back up your data and make sure you're getting the expected output.
#'
#' If df2 contains rows not matched in df1, they will get added to the result. Note that even if df2 has NA for a matching id, it will overwrite a value in df1. (That's why it's "hard" left_join).
#'
#' @param df1 the data frame (or tibble) whose structure you want to preserve
#' @param df2 the data frame with new data you want to crush over df1 (destroying matching cells in df1)
#' @param by a single column name you want to use to match df1 and df2 values
#' @param as_character logical; do you want to override classes and make everything characters. This will avoid errors of matching incompatible types, but may have unexpected results for some data sets. Default=F
#' @return A data frame that has the structure of df1, with matching ids and columns replaced with df2 values
#' @export

hard_left_join<-function(df1,df2,by,as_character=FALSE){
  if(missing(by)){stop("Specify 'by' column for matching df1 and df2")}
  df1_0<-df1
  df2_0<-df2
  if(as_character){
    #annoying AF syntax with across and everything
    df1<-df1_0 %>% dplyr::as_tibble(df1) %>% dplyr::mutate(dplyr::across(dplyr::everything(),as.character))
    df2<-df2_0 %>% dplyr::as_tibble(df2) %>% dplyr::mutate(dplyr::across(dplyr::everything(),as.character))
  }
  df1<-dplyr::as_tibble(df1)
  df2<-dplyr::as_tibble(df2)

  #intersecting (non-id) columns
  ixn<-intersect(names(df1),names(df2))
  ixn <- ixn[!ixn%in%by]

  df2_rows_with_matches<-unlist(df2[,by]) %in% unlist(df1[,by]) %>% which()

  #matching rows
  df1_rows_with_matches<-unlist(df1[,by]) %in% unlist(df2[,by]) %>%  which()

  #output should be in the format of df1
  df3<-df1
  #do the replacement
  if(length(df1_rows_with_matches)>0){
  df3[df1_rows_with_matches,ixn] <- df2[df2_rows_with_matches,ixn]
  }
  #Add unmatched rows in df2
  df2_unmatched<-dplyr::anti_join(df2,df3,by=by) %>% dplyr::select(by,ixn)
  final<-df3 %>% dplyr::add_row(df2_unmatched)
  final
}
