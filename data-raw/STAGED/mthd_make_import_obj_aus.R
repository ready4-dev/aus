
make_import_object.Aus_16_31_sp_import_lup <- function(x,
                                                       var_val_chr,
                                                       path_1L_chr){
  make_import_obj_for_australia(var_val_chr = var_val_chr,
                                path_1L_chr = path_1L_chr)
}

make_import_obj_for_australia <- function(var_val_chr,
                                          path_1L_chr){ ## CONCERT ALL OF BELOW TO CONTEXT CLASSES AND METHODS
  if(var_val_chr[2] %>%
     stringr::str_sub(1,3) == "ERP" & var_val_chr[1]== "LGA"){
    import_obj <- spatial_pop_attributes_lga(age_by_lga = path_1L_chr,
                                             year_1L_chr = var_val_chr[3])  %>%
      tibble::as.tibble()
  }
  if(var_val_chr[2] %>%
     stringr::str_sub(1,3) == "ERP" & var_val_chr[1]== "SA2"){
    import_obj <- spatial_pop_attributes_sa2(age_by_sa2 = path_1L_chr,
                                             year_1L_chr = var_val_chr[3]) %>%
      dplyr::mutate(SA2_MAIN16 = stringr::str_c(`SA3 code`,
                                                stringr::str_sub(`SA2 code`,-4)))
  }
  if(var_val_chr[2] %>%
     stringr::str_sub(1,3) == "ERP" & var_val_chr[1]== "SA1" & var_val_chr[3] =="2017"){
    import_obj <- readxl::read_excel(path_1L_chr,
                                     sheet="ASGS2016 ERP",
                                     range ="A7:G57530",
                                     col_names = TRUE)
    names(import_obj) <- c("SA1_7DIG16",    "year_2011",   "year_2012pr", "year_2013pr", "year_2014pr", "year_2015pr", "year_2016pr")
    import_obj <- import_obj %>%
      dplyr::mutate(SA1_7DIG16 = as.character(SA1_7DIG16))
  }
  if(var_val_chr[2] == "SEIFA"){
    if(var_val_chr[1]== "SA2" & var_val_chr[3] =="2011")
      range_string <- "A7:P2110"
    if(var_val_chr[1]== "SA2" & var_val_chr[3] =="2016")
      range_string <- "A7:P2190"
    if(var_val_chr[1]== "LGA" & var_val_chr[3] =="2011")
      range_string <- "A7:P570"
    if(var_val_chr[1]== "LGA" & var_val_chr[3] =="2016")
      range_string <- "A7:P550"
    import_obj <- transform_seifa_deciles_tb(seifa_deciles_tb = readxl::read_excel(path = path_1L_chr,
                                                                                   sheet= 3,
                                                                                   range = range_string,
                                                                                   col_names = FALSE),
                                        var_nms_1_chr = readxl::read_excel(path = path_1L_chr,
                                                                         sheet= 3,
                                                                         range = "F6:M6",
                                                                         col_names = FALSE),
                                        var_nms_2_chr = readxl::read_excel(path = path_1L_chr,
                                                                         sheet= 3,
                                                                         range = "N5:P5",
                                                                         col_names = FALSE),
                                        area_unit_1L_chr = var_val_chr[1])
  }
  if(var_val_chr[2] == "PPR" & var_val_chr[4]== "VIC"){
    import_obj <- purrr::map(c("A101:BJ182",
                               "A191:BJ272",
                               "A281:BJ362",
                               "A371:BJ452"),
                             ~  readxl::read_excel(path_1L_chr,
                                                   sheet="ERP_Ages_LGAs",
                                                   range = .x,
                                                   col_names = TRUE) %>%
                               dplyr::rename(LGA_NAME11 = `Local Government Area`) %>%
                               dplyr::filter(LGA_NAME11 != "Victoria")) %>%
      stats::setNames(c("y2016",
                        "y2021",
                        "y2026",
                        "y2031"))
  }
  if(var_val_chr[2] == "PPR" & var_val_chr[4]== "NSW"){
    import_obj <- make_nsw_pop_projs_ls(path_1L_chr = path_1L_chr,
                                        var_value_vec = var_val_chr)
  }
  if(var_val_chr[2] == "PPR" & var_val_chr[4]== "ACT"){
    import_obj <- make_act_pop_projs_ls(path_1L_chr = path_1L_chr,
                                        var_value_vec = var_val_chr)
  }
  if(var_val_chr[2] == "PPR" & var_val_chr[4]== "NT"){
    import_obj <- make_nt_pop_projs_ls(path_1L_chr = path_1L_chr,
                                       var_value_vec = var_val_chr)
  }
  if(var_val_chr[2] == "PPR" & var_val_chr[4]== "QLD"){
    import_obj <- make_qld_pop_projs_ls(path_1L_chr = path_1L_chr,
                                        var_value_vec = var_val_chr)
  }
  if(var_val_chr[2] == "PPR" & var_val_chr[4]== "SA"){
    import_obj <- make_sa_pop_projs_ls(path_1L_chr = path_1L_chr,
                                       var_value_vec = var_val_chr)
  }
  if(var_val_chr[2] == "PPR" & var_val_chr[4]== "TAS"){
    import_obj <- make_tas_pop_projs_ls(path_1L_chr = path_1L_chr,
                                        var_value_vec = var_val_chr)
  }
  if(var_val_chr[2] == "PPR" & var_val_chr[4]== "WA"){
    import_obj <- make_wa_pop_projs_ls(path_1L_chr = path_1L_chr,
                                       var_value_vec = var_val_chr)
  }
  if(var_val_chr[2] == "2011"){ # THIS LOOKS WRONG
    if(var_val_chr[1]== "SA2")
      range_string <- "A8:P443"
    if(var_val_chr[1]== "LGA")
      range_string <- "A8:F605"
    import_obj <- readxl::read_excel(path_1L_chr,
                                     sheet="Table 3",
                                     range = range_string,
                                     col_names = FALSE)
    colnames(import_obj) <- readxl::read_excel(path_1L_chr,
                                               sheet="Table 3",
                                               range ="A6:F6",
                                               col_names = FALSE)
  }
  if(var_val_chr[[2]] %in% c("EPI_PRF_SRC", "EPI_PRV_RTS", "PPR_MAPE", "EPI_PARAMS", "RR_BR_SH"))
    import_obj <- read.csv(path_1L_chr, stringsAsFactors = F) %>% tibble::as_tibble()
  return(import_obj)
}
spatial_pop_attributes_lga <- function(age_by_lga,
                                       year_1L_chr){
  #readxl::excel_sheets(age_by_lga)
  if(year_1L_chr=="2011"){
    age_by_lga_female<-readxl::read_excel(age_by_lga,
                                          sheet="Table 2",
                                          range ="A9:W574")
    age_by_lga_male<-readxl::read_excel(age_by_lga,
                                        sheet="Table 1",
                                        range ="A9:W574")
    colnames(age_by_lga_female)<-
      colnames(age_by_lga_male)<-cbind(readxl::read_excel(age_by_lga,
                                                          sheet="Table 2",
                                                          range ="A8:D8",
                                                          col_names = FALSE),
                                       readxl::read_excel(age_by_lga,
                                                          sheet="Table 2",
                                                          range ="E7:W7",
                                                          col_names = FALSE))
  }
  if(year_1L_chr=="2016"){
    age_by_lga_female <- readxl::read_excel(age_by_lga,
                                            sheet="Table 2",
                                            range ="A10:W555")
    age_by_lga_male <- readxl::read_excel(age_by_lga,
                                          sheet="Table 1",
                                          range ="A10:W555")
    colnames(age_by_lga_female)<-
      colnames(age_by_lga_male)<-
      cbind(readxl::read_excel(age_by_lga,
                               sheet="Table 2",
                               range ="A9:D9",
                               col_names = FALSE),
            readxl::read_excel(age_by_lga,
                               sheet="Table 2",
                               range ="E8:W8",
                               col_names = FALSE))
  }
  age_by_lga_female <- age_by_lga_female %>%
    rename_by_index(.,5,!!rlang::sym(paste0("y",year_1L_chr,".Females.0.4"))) %>%
    rename_by_index(.,6,!!rlang::sym(paste0("y",year_1L_chr,".Females.5.9"))) %>%
    rename_by_index(.,7,!!rlang::sym(paste0("y",year_1L_chr,".Females.10.14"))) %>%
    rename_by_index(.,8,!!rlang::sym(paste0("y",year_1L_chr,".Females.15.19"))) %>%
    rename_by_index(.,9,!!rlang::sym(paste0("y",year_1L_chr,".Females.20.24"))) %>%
    rename_by_index(.,10,!!rlang::sym(paste0("y",year_1L_chr,".Females.25.29"))) %>%
    rename_by_index(.,11,!!rlang::sym(paste0("y",year_1L_chr,".Females.30.34"))) %>%
    rename_by_index(.,12,!!rlang::sym(paste0("y",year_1L_chr,".Females.35.39"))) %>%
    rename_by_index(.,13,!!rlang::sym(paste0("y",year_1L_chr,".Females.40.44"))) %>%
    rename_by_index(.,14,!!rlang::sym(paste0("y",year_1L_chr,".Females.45.49"))) %>%
    rename_by_index(.,15,!!rlang::sym(paste0("y",year_1L_chr,".Females.50.54"))) %>%
    rename_by_index(.,16,!!rlang::sym(paste0("y",year_1L_chr,".Females.55.59"))) %>%
    rename_by_index(.,17,!!rlang::sym(paste0("y",year_1L_chr,".Females.60.64"))) %>%
    rename_by_index(.,18,!!rlang::sym(paste0("y",year_1L_chr,".Females.65.69"))) %>%
    rename_by_index(.,19,!!rlang::sym(paste0("y",year_1L_chr,".Females.70.74"))) %>%
    rename_by_index(.,20,!!rlang::sym(paste0("y",year_1L_chr,".Females.75.79"))) %>%
    rename_by_index(.,21,!!rlang::sym(paste0("y",year_1L_chr,".Females.80.84"))) %>%
    rename_by_index(.,22,!!rlang::sym(paste0("y",year_1L_chr,".Females.85.pl")))
  # total0to14f <- paste0("y",year_1L_chr,".total0to14f")
  # total15to24f <- paste0("y",year_1L_chr,".total15to24f")
  # age_by_lga_female <- age_by_lga_female %>%
  #   dplyr::mutate(
  #     !!total0to14f := !!rlang::sym(paste0("y",year_1L_chr,".Females.0.4")) + !!rlang::sym(paste0("y",year_1L_chr,".Females.5.9")) + !!rlang::sym(paste0("y",year_1L_chr,".Females.10.14")),
  #     !!total15to24f := !!rlang::sym(paste0("y",year_1L_chr,".Females.15.19")) + !!rlang::sym(paste0("y",year_1L_chr,".Females.20.24")))

  age_by_lga_male <- age_by_lga_male %>%
    rename_by_index(.,5,!!rlang::sym(paste0("y",year_1L_chr,".Males.0.4"))) %>%
    rename_by_index(.,6,!!rlang::sym(paste0("y",year_1L_chr,".Males.5.9"))) %>%
    rename_by_index(.,7,!!rlang::sym(paste0("y",year_1L_chr,".Males.10.14"))) %>%
    rename_by_index(.,8,!!rlang::sym(paste0("y",year_1L_chr,".Males.15.19"))) %>%
    rename_by_index(.,9,!!rlang::sym(paste0("y",year_1L_chr,".Males.20.24"))) %>%
    rename_by_index(.,10,!!rlang::sym(paste0("y",year_1L_chr,".Males.25.29"))) %>%
    rename_by_index(.,11,!!rlang::sym(paste0("y",year_1L_chr,".Males.30.34"))) %>%
    rename_by_index(.,12,!!rlang::sym(paste0("y",year_1L_chr,".Males.35.39"))) %>%
    rename_by_index(.,13,!!rlang::sym(paste0("y",year_1L_chr,".Males.40.44"))) %>%
    rename_by_index(.,14,!!rlang::sym(paste0("y",year_1L_chr,".Males.45.49"))) %>%
    rename_by_index(.,15,!!rlang::sym(paste0("y",year_1L_chr,".Males.50.54"))) %>%
    rename_by_index(.,16,!!rlang::sym(paste0("y",year_1L_chr,".Males.55.59"))) %>%
    rename_by_index(.,17,!!rlang::sym(paste0("y",year_1L_chr,".Males.60.64"))) %>%
    rename_by_index(.,18,!!rlang::sym(paste0("y",year_1L_chr,".Males.65.69"))) %>%
    rename_by_index(.,19,!!rlang::sym(paste0("y",year_1L_chr,".Males.70.74"))) %>%
    rename_by_index(.,20,!!rlang::sym(paste0("y",year_1L_chr,".Males.75.79"))) %>%
    rename_by_index(.,21,!!rlang::sym(paste0("y",year_1L_chr,".Males.80.84"))) %>%
    rename_by_index(.,22,!!rlang::sym(paste0("y",year_1L_chr,".Males.85.pl")))
  # total0to14m <- paste0("y",year_1L_chr,".total0to14m")
  # total15to24m <- paste0("y",year_1L_chr,".total15to24m")
  # age_by_lga_male <- age_by_lga_male %>%
  #   dplyr::mutate(
  #     !!total0to14m := !!rlang::sym(paste0("y",year_1L_chr,".Males.0.4")) + !!rlang::sym(paste0("y",year_1L_chr,".Males.5.9")) + !!rlang::sym(paste0("y",year_1L_chr,".Males.10.14")),
  #     !!total15to24m := !!rlang::sym(paste0("y",year_1L_chr,".Males.15.19")) + !!rlang::sym(paste0("y",year_1L_chr,".Males.20.24")))
  merged_pop<-merge(age_by_lga_female,
                          age_by_lga_male#,
                          # by = c("LGA code",
                          #        "LGA name",
                          #        "S/T code",
                          #        "S/T name")
  )
  merged_pop[["LGA name"]]<-gsub("\\`",
                                       "'",
                                       merged_pop[["LGA name"]])
  merged_pop[["LGA name"]]<-gsub("Glamorgan-Spring Bay",
                                       "Glamorgan/Spring Bay",
                                       merged_pop[["LGA name"]])
  merged_pop[["LGA name"]]<-gsub("Kalgoorlie-Boulder",
                                       "Kalgoorlie/Boulder",
                                       merged_pop[["LGA name"]])
  merged_pop[["LGA name"]]<-gsub("Orroroo-Carrieton",
                                       "Orroroo/Carrieton",
                                       merged_pop[["LGA name"]])
  merged_pop[["LGA name"]]<-gsub("Waratah-Wynyard",
                                       "Waratah/Wynyard",
                                       merged_pop[["LGA name"]])
  return(merged_pop)
}
spatial_pop_attributes_sa2 <- function(age_by_sa2,
                                       prior_timepoint = FALSE,
                                       reference_list = list(ST_Code = c(1:8),
                                                             data_range = c("A10:AB585",
                                                                            "A10:AB471",
                                                                            "A10:AB537",
                                                                            "A10:AB181",
                                                                            "A10:AB261",
                                                                            "A10:AB108",
                                                                            "A10:AB77",
                                                                            "A10:AB140")),
                                       year_1L_chr){
  if(!prior_timepoint){
    femaletable <- "Table 5"
    maletable <- "Table 4"
  }else{
    femaletable <- "Table 2"
    maletable <- "Table 1"
  }
  st_index_nbr <- as.numeric(readxl::read_excel(age_by_sa2,
                                                sheet = femaletable,
                                                range ="A10:A10",
                                                col_names = FALSE))
  reference_lookup <- reference_list %>% purrr::pluck("data_range") %>% purrr::pluck(st_index_nbr)
  age.by.sa2.female<-readxl::read_excel(age_by_sa2,
                                        sheet = femaletable,
                                        range = reference_lookup,
                                        col_names = FALSE)
  age.by.sa2.male<-readxl::read_excel(age_by_sa2,
                                      sheet = maletable,
                                      range = reference_lookup,
                                      col_names = FALSE)
  ## Depends on all state/territory files having headers in same cells
  colnames(age.by.sa2.female) <-
    colnames(age.by.sa2.male) <-
    cbind(readxl::read_excel(age_by_sa2,
                             sheet=femaletable,
                             range ="A8:J8",
                             col_names = FALSE),
          readxl::read_excel(age_by_sa2,
                             sheet=femaletable,
                             range ="K7:AB7",
                             col_names = FALSE)
    )
  age.by.sa2.female<-age.by.sa2.female %>%
    rename_by_index(.,11,!!rlang::sym(paste0("y",year_1L_chr,".Females.0.4"))) %>%
    rename_by_index(.,12,!!rlang::sym(paste0("y",year_1L_chr,".Females.5.9"))) %>%
    rename_by_index(.,13,!!rlang::sym(paste0("y",year_1L_chr,".Females.10.14"))) %>%
    rename_by_index(.,14,!!rlang::sym(paste0("y",year_1L_chr,".Females.15.19"))) %>%
    rename_by_index(.,15,!!rlang::sym(paste0("y",year_1L_chr,".Females.20.24"))) %>%
    rename_by_index(.,16,!!rlang::sym(paste0("y",year_1L_chr,".Females.25.29"))) %>%
    rename_by_index(.,17,!!rlang::sym(paste0("y",year_1L_chr,".Females.30.34"))) %>%
    rename_by_index(.,18,!!rlang::sym(paste0("y",year_1L_chr,".Females.35.39"))) %>%
    rename_by_index(.,19,!!rlang::sym(paste0("y",year_1L_chr,".Females.40.44"))) %>%
    rename_by_index(.,20,!!rlang::sym(paste0("y",year_1L_chr,".Females.45.49"))) %>%
    rename_by_index(.,21,!!rlang::sym(paste0("y",year_1L_chr,".Females.50.54"))) %>%
    rename_by_index(.,22,!!rlang::sym(paste0("y",year_1L_chr,".Females.55.59"))) %>%
    rename_by_index(.,23,!!rlang::sym(paste0("y",year_1L_chr,".Females.60.64"))) %>%
    rename_by_index(.,24,!!rlang::sym(paste0("y",year_1L_chr,".Females.65.69"))) %>%
    rename_by_index(.,25,!!rlang::sym(paste0("y",year_1L_chr,".Females.70.74"))) %>%
    rename_by_index(.,26,!!rlang::sym(paste0("y",year_1L_chr,".Females.75.79"))) %>%
    rename_by_index(.,27,!!rlang::sym(paste0("y",year_1L_chr,".Females.80.84"))) %>%
    rename_by_index(.,28,!!rlang::sym(paste0("y",year_1L_chr,".Females.85.pl")))
  age.by.sa2.female <- dplyr::bind_cols(age.by.sa2.female %>%
                                          dplyr::select(-(1:6)),
                                        age.by.sa2.female %>%
                                          dplyr::select(6:1)
  )
  # total0to14f <- paste0("y",year_1L_chr,".total0to14f")
  # total15to24f <- paste0("y",year_1L_chr,".total15to24f")
  # age.by.sa2.female <- age.by.sa2.female %>%
  #   dplyr::mutate(
  #     !!total0to14f := !!rlang::sym(paste0("y",year_1L_chr,".Females.0.4")) + !!rlang::sym(paste0("y",year_1L_chr,".Females.5.9")) + !!rlang::sym(paste0("y",year_1L_chr,".Females.10.14")),
  #     !!total15to24f := !!rlang::sym(paste0("y",year_1L_chr,".Females.15.19")) + !!rlang::sym(paste0("y",year_1L_chr,".Females.20.24")))
  age.by.sa2.male<-age.by.sa2.male %>%
    rename_by_index(.,11,!!rlang::sym(paste0("y",year_1L_chr,".Males.0.4"))) %>%
    rename_by_index(.,12,!!rlang::sym(paste0("y",year_1L_chr,".Males.5.9"))) %>%
    rename_by_index(.,13,!!rlang::sym(paste0("y",year_1L_chr,".Males.10.14"))) %>%
    rename_by_index(.,14,!!rlang::sym(paste0("y",year_1L_chr,".Males.15.19"))) %>%
    rename_by_index(.,15,!!rlang::sym(paste0("y",year_1L_chr,".Males.20.24"))) %>%
    rename_by_index(.,16,!!rlang::sym(paste0("y",year_1L_chr,".Males.25.29"))) %>%
    rename_by_index(.,17,!!rlang::sym(paste0("y",year_1L_chr,".Males.30.34"))) %>%
    rename_by_index(.,18,!!rlang::sym(paste0("y",year_1L_chr,".Males.35.39"))) %>%
    rename_by_index(.,19,!!rlang::sym(paste0("y",year_1L_chr,".Males.40.44"))) %>%
    rename_by_index(.,20,!!rlang::sym(paste0("y",year_1L_chr,".Males.45.49"))) %>%
    rename_by_index(.,21,!!rlang::sym(paste0("y",year_1L_chr,".Males.50.54"))) %>%
    rename_by_index(.,22,!!rlang::sym(paste0("y",year_1L_chr,".Males.55.59"))) %>%
    rename_by_index(.,23,!!rlang::sym(paste0("y",year_1L_chr,".Males.60.64"))) %>%
    rename_by_index(.,24,!!rlang::sym(paste0("y",year_1L_chr,".Males.65.69"))) %>%
    rename_by_index(.,25,!!rlang::sym(paste0("y",year_1L_chr,".Males.70.74"))) %>%
    rename_by_index(.,26,!!rlang::sym(paste0("y",year_1L_chr,".Males.75.79"))) %>%
    rename_by_index(.,27,!!rlang::sym(paste0("y",year_1L_chr,".Males.80.84"))) %>%
    rename_by_index(.,28,!!rlang::sym(paste0("y",year_1L_chr,".Males.85.pl")))
  age.by.sa2.male <- dplyr::bind_cols(age.by.sa2.male %>%
                                        dplyr::select(-(1:6)),
                                      age.by.sa2.male %>%
                                        dplyr::select(6:1)
  )
  # total0to14m <- paste0("y",year_1L_chr,".total0to14m")
  # total15to24m <- paste0("y",year_1L_chr,".total15to24m")
  # age.by.sa2.male <- age.by.sa2.male %>%
  #   dplyr::mutate(
  #     !!total0to14m := !!rlang::sym(paste0("y",year_1L_chr,".Males.0.4")) + !!rlang::sym(paste0("y",year_1L_chr,".Males.5.9")) + !!rlang::sym(paste0("y",year_1L_chr,".Males.10.14")),
  #     !!total15to24m := !!rlang::sym(paste0("y",year_1L_chr,".Males.15.19")) + !!rlang::sym(paste0("y",year_1L_chr,".Males.20.24")))
  merged.pop <- dplyr::inner_join(age.by.sa2.male,
                                        age.by.sa2.female)
  return(merged.pop)
}
