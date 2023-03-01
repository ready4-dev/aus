transform_seifa_deciles_tb <- function(seifa_deciles_tb, #spatial_seifa_deciles
                                       var_nms_1_chr,
                                       var_nms_2_chr,
                                       area_unit_1L_chr){
  names(seifa_deciles_tb)[6:16] <- cbind(var_nms_1_chr,
                                         var_nms_2_chr) %>%
    as.vector()
  if(area_unit_1L_chr=="LGA"){
    names(seifa_deciles_tb)[1:2] <- c("LGA_Code",
                                      "LGA_NAME")
  }
  if(area_unit_1L_chr=="SA2"){
    names(seifa_deciles_tb)[1:2] <- c("SA2_Code",
                                      "SA2_NAME")
  }
  names(seifa_deciles_tb)[3:5] <- c("Usual.Res.Pop",
                                    "Score",
                                    "BLANK")
  names(seifa_deciles_tb)[6:8] <- c("Rank.Australia",
                                    "Decile.Australia",
                                    "Percentile.Australia")
  names(seifa_deciles_tb)[11:13] <- c("Rank.ST",
                                      "Decile.ST",
                                      "Percentile.ST")
  seifa_deciles_tb <- seifa_deciles_tb %>%
    dplyr::select(-dplyr::one_of("BLANK",
                                 "NA"))
  return(seifa_deciles_tb)
}
# transform_seifa_ds <- function(seifa_data,#prepare_seifa_data # Not used??
#                                area_unit_1L_chr,
#                                #att_data_year,
#                                time_1_sfx_1L_chr){
#   #time_1_sfx_1L_chr <- stringr::str_sub(boundary_year,start=3,end=4)
#   if(area_unit_1L_chr=="LGA"){
#     seifa_deciles_tb <- seifa_data %>%
#       dplyr::select(-"State") %>%
#       dplyr::rename(!!rlang::sym(paste0("LGA_CODE",time_1_sfx_1L_chr)) := "LGA_Code",
#                     !!rlang::sym(paste0("LGA_NAME",time_1_sfx_1L_chr)) := "LGA_NAME") %>%
#       dplyr::mutate(!!rlang::sym(paste0("LGA_CODE",time_1_sfx_1L_chr)) := as.character(!!rlang::sym(paste0("LGA_CODE",time_1_sfx_1L_chr))))
#   }
#   if(area_unit_1L_chr=="SA2"){
#     seifa_deciles_tb <- seifa_data %>%
#       dplyr::rename(SA2_MAIN16=SA2_Code,
#                     SA2_NAME16=SA2_NAME)
#     seifa_deciles_tb[[paste0("SA2_MAIN",time_1_sfx_1L_chr)]] <- factor(seifa_deciles_tb[[paste0("SA2_MAIN",time_1_sfx_1L_chr)]])
#   }
#   return(seifa_deciles_tb)
# }
