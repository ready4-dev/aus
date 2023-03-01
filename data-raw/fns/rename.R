rename_by_index <- function(df, index_1L_int, new_name_1L_chr) {#prog_rename_col_by_position
  new_name_xx <- rlang::enquo(new_name_1L_chr)
  new_name_xx <- rlang::quo_name(new_name_xx)
  renamed_df <- dplyr::select(df, !! new_name_xx := !! rlang::quo(names(df)[[index_1L_int]]),
                dplyr::everything())
  return(renamed_df)
}
