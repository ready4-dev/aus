#' Rename by index
#' @description rename_by_index() is a Rename function that renames elements of an object based on a pre-speccified schema. Specifically, this function implements an algorithm to rename by index. The function returns Renamed (a data.frame).
#' @param df Data.frame (a data.frame)
#' @param index_1L_int Index (an integer vector of length one)
#' @param new_name_1L_chr New name (a character vector of length one)
#' @return Renamed (a data.frame)
#' @rdname rename_by_index
#' @export 
#' @importFrom rlang enquo quo_name quo
#' @importFrom dplyr select everything
#' @keywords internal
rename_by_index <- function (df, index_1L_int, new_name_1L_chr) 
{
    new_name_xx <- rlang::enquo(new_name_1L_chr)
    new_name_xx <- rlang::quo_name(new_name_xx)
    renamed_df <- dplyr::select(df, `:=`(!!new_name_xx, !!rlang::quo(names(df)[[index_1L_int]])), 
        dplyr::everything())
    return(renamed_df)
}
