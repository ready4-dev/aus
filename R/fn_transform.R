#' Transform Socio-Economic Indices for Areas deciles tibble
#' @description transform_seifa_deciles_tb() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform socio-economic indices for areas deciles tibble. Function argument seifa_deciles_tb specifies the object to be updated. Argument var_nms_1_chr provides the object to be updated. The function returns Socio-Economic Indices for Areas deciles (a tibble).
#' @param seifa_deciles_tb Socio-Economic Indices for Areas deciles (a tibble)
#' @param var_nms_1_chr Variable names 1 (a character vector)
#' @param var_nms_2_chr Variable names 2 (a character vector)
#' @param area_unit_1L_chr Area unit (a character vector of length one)
#' @return Socio-Economic Indices for Areas deciles (a tibble)
#' @rdname transform_seifa_deciles_tb
#' @export 
#' @importFrom dplyr select one_of
#' @keywords internal
transform_seifa_deciles_tb <- function (seifa_deciles_tb, var_nms_1_chr, var_nms_2_chr, area_unit_1L_chr) 
{
    names(seifa_deciles_tb)[6:16] <- cbind(var_nms_1_chr, var_nms_2_chr) %>% 
        as.vector()
    if (area_unit_1L_chr == "LGA") {
        names(seifa_deciles_tb)[1:2] <- c("LGA_Code", "LGA_NAME")
    }
    if (area_unit_1L_chr == "SA2") {
        names(seifa_deciles_tb)[1:2] <- c("SA2_Code", "SA2_NAME")
    }
    names(seifa_deciles_tb)[3:5] <- c("Usual.Res.Pop", "Score", 
        "BLANK")
    names(seifa_deciles_tb)[6:8] <- c("Rank.Australia", "Decile.Australia", 
        "Percentile.Australia")
    names(seifa_deciles_tb)[11:13] <- c("Rank.ST", "Decile.ST", 
        "Percentile.ST")
    seifa_deciles_tb <- seifa_deciles_tb %>% dplyr::select(-dplyr::one_of("BLANK", 
        "NA"))
    return(seifa_deciles_tb)
}
