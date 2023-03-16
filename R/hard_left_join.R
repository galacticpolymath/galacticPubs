#' hard_left_join
#'
#' Merge one dataframe into another, replacing data frame 1's values with matching values from data frame 2 without a bunch of .x .y column duplication BS. This is a destructive function, so back up your data and make sure you're getting the expected output.
#'
#' If df2 contains rows not matched in df1, they will get added to the result. Note that even if df2 has NA for a matching id, it will overwrite a value in df1. (That's why it's "hard" left_join).
#'
#' @param df1 the data frame (or tibble) whose structure you want to preserve; If it's an empty tibble, df2 columns matching df1 will be output
#' @param df2 the data frame with new data you want to crush over df1 (destroying matching cells in df1)
#' @param by a single column name you want to use to match df1 and df2 values
#' @param as_char logical; do you want to override classes and make everything characters. This will avoid errors of matching incompatible types, but may have unexpected results for some data sets. Default=F
#' @param df1_cols_to_keep specify columns of df1 to keep as-is; default=NULL
#' @return A data frame that has the structure of df1, with matching ids and columns replaced with df2 values
#' @export
#' @examples
#' (df1<-dplyr::tribble(~id,~b,~c,~d,"Jim",1,2,3,"Bob",4,5,6))
#' (df2<-dplyr::tribble(~id,~b,~c,"Jim",1.1,2.1,"Bob",NA,5.2))
#' hard_left_join(df1,df2,by="id")
#' #df2 values overwrite matching columns; df1 columns not found in df2 are ignored
#'
#'
#' hard_left_join(df1,df2,by="id",df1_cols_to_keep="b")
#' #here df1 values for the b column are kept in output

hard_left_join <-
  function(df1,
           df2,
           by,
           as_char = FALSE,
           df1_cols_to_keep = NULL) {
    if (missing(by)) {
      stop("Specify 'by' column for matching df1 and df2")
    }
    #There's a problem reconciling files if all rows of "by" column aren't unique

    if (is_empty(df2)) {
      final <- df2[, names(df1)]
    }else if(is_empty(df1)){
    final<-df1

    }else{
      df1_dups <- duplicated(df1[, by])
      df2_dups <- duplicated(df2[, by])
      if (sum(df1_dups) > 0) {
        stop(paste0(
          "The following duplicated values in df1 must be made unique: \n   -",
          paste0(unlist(df1[df1_dups, by]), sep = "", collapse = "\n   -")
        ))
      }

      if (sum(df2_dups) > 0) {
        stop(paste0(
          "The following duplicated values in df2 must be made unique: \n   -",
          paste0(unlist(df2[df2_dups, by]), sep = "", collapse = "\n   -")
        ))
      }

      df1_0 <- df1
      df2_0 <- df2
      if (as_char) {
        #annoying AF syntax with across and everything
        df1 <-
          df1_0 %>% dplyr::as_tibble(df1) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
        df2 <-
          df2_0 %>% dplyr::as_tibble(df2) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      }
      df1 <- dplyr::as_tibble(df1)
      df2 <- dplyr::as_tibble(df2)

      #intersecting (non-id) columns
      ixn <- intersect(names(df1), names(df2))
      ixn <- ixn[!ixn %in% by]

      df2_index_in_df1 <- match(unlist(df2[, by]), unlist(df1[, by]))

      #output should be in the format of df1
      df3 <- df1
      #do the replacement
      if (length(unique_sans_na(df2_index_in_df1)) > 0) {
        df3[unique_sans_na(df2_index_in_df1), ixn] <-
          df2[!is.na(df2_index_in_df1), ixn]
      }
      #Add unmatched rows in df2
      df2_unmatched <-
        dplyr::anti_join(df2, df3, by = by) %>% dplyr::select(by, ixn)
      semifinal <- df3 %>% dplyr::add_row(df2_unmatched)

      #Preserve df1 values for certain columns if asked
      if (is.null(df1_cols_to_keep)) {
        final <- semifinal
      } else{
        final <- hard_left_join(semifinal, df1[, c(by, df1_cols_to_keep)], by = by)
      }

    }

    final
  }
