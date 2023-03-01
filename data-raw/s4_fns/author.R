author_AusACT <- function(x, # May need to change calling method (currently manufacture)
                          what_1L_chr = "ppr_2018"){ # make_act_ppr_2016 Method for make_act_ppr_2016 class
  if(what_1L_chr == "ppr_2018"){
    path_1L_chr <- paste0(x@processed_fls_dir_1L_chr,
                          "/",
                          x@a_VicinityLookup@vicinity_raw_r3$name_chr,
                          # ready4::get_from_lup_obj(data_lookup_tb = lookup_tbs_r4@sp_data_pack_lup,
                          #                       match_value_xx = "aus_sa3_ACT_ppr_2018_tb",
                          #                       match_var_nm_1L_chr = "name",
                          #                       target_var_nm_1L_chr = "source_reference_chr",
                          #                       evaluate_1L_lgl = F),
                          ".RDS")
    processed_file_exists_lgl <- file.exists(path_1L_chr)
    y_VicinityLookup <- vicinity::VicinityLocalRaw(merge_itms_chr = x@merge_itms_chr, #vicinity::VicinityLocal
                                                   raw_fls_dir_1L_chr = x@raw_fls_dir_1L_chr,
                                                   pkg_1L_chr = x@pkg_1L_chr,
                                                   overwrite_1L_lgl = x@overwrite_1L_lgl,
                                                   write_1L_lgl = x@write_1L_lgl,
                                                   a_VicinityLookup = x@a_VicinityLookup#lup_tbs_r4
                                                   ) %>%
      author(processed_fls_dir_1L_chr_chr = x@processed_fls_dir_1L_chr,
             crs_nbr_dbl = x@crs_nbr_dbl)

    if(x@overwrite_1L_lgl | !processed_file_exists_lgl){
      new_act_ppr_ls <- add_year_2016_to_ACT_ppr(aus_sa3_ACT_ppr_2018_tb = path_1L_chr %>% readRDS(),
                                             aus_sa2_ACT_erp_age_sex_2016_tb = parse(text = x@merge_itms_chr) %>% eval())
      saveRDS(new_act_ppr_ls,file = path_1L_chr)
    }
    y_VicinityLookup <- renewSlot(y_VicinityLookup,#
                                  "vicinity_processed_r3",
                                   y_VicinityLookup@vicinity_processed_r3 %>%
                                     dplyr::mutate(year_start_chr = purrr::map2_chr(name_chr,
                                                                                    year_start_chr,
                                                                                    ~ ifelse(.x == "aus_sa3_ACT_ppr_2018_tb",
                                                                                             "2016",
                                                                                             .y))))
  }
  return(y_VicinityLookup)

}
author_AusHeadspace <- function(x){ #method for hss class #add_aus_hss_data
  y_VicinityLookup <- x@a_VicinityLookup
  vicinity_raw_r3 <- y_VicinityLookup@vicinity_raw_r3
  ready4use::assert_single_row_tb(vicinity_raw_r3)
  import_type_ls <- procure(vicinity_raw_r3) ## NOT DEFINED - Possibly a manufacture method #TF2A
  ready4use::assert_matches_chr(import_type_ls[[1]],"Aus_16_31_hss")#AusSpR4c::add_aus_hss_data
  dv_import_r3 <- manufacture(vicinity_raw_r3) #TF2A
  hss_lup_r3_pt1 <- dv_import_r3 %>%
    dplyr::mutate(data_repo_ui_chr = NA_character_) %>% ## DELETE THIS LINE IN UPDATE
    procure() ## DEFINED????
  # manufacture(x, #TF2A
  #                           forced_choice_chr = "source_url_chr")
  # hss_lup_r3_pt1 <- data.table::fread(vicinity_raw_r3 %>% dplyr::pull(download_url_chr)) %>%
  #   tibble::as_tibble()
  author(vicinity_raw_r3,
         match_vals_xx = vicinity_raw_r3 %>% dplyr::pull(name_chr),
         path_1L_chr = paste0(x@raw_fls_dir_1L_chr,"/Geometries"),
         overwrite_1L_lgl = x@overwrite_1L_lgl)
  bndys_to_add_chr <- vicinity_raw_r3 %>%
    dplyr::pull(add_boundaries) %>% ######
  purrr::pluck(1)
  #####
  path_to_centres_sf_1L_chr <- vicinity::make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr,
                                                                   name_chr = vicinity_raw_r3$name_chr,
                                                                   data_type_chr = "Geometry")
  path_to_seed_sf_1L_chr <-x@merge_itms_chr[2] %>% stringr::str_sub(start=10,end=-3)
  x <- renewSlot(x,"path_to_seed_sf_1L_chr",path_to_seed_sf_1L_chr)
  y_VicinityLookup <- renew(y_VicinityLookup,
                            path_1L_chr = x@path_to_seed_sf_1L_chr,
                            what_1L_chr = "templates") %>%
    renew(what_1L_chr = "identifiers")
  ####
  inc_cols_vec <- c(names(hss_lup_r3_pt1)[!names(hss_lup_r3_pt1) %in% c("lng_dbl", "lat_dbl")], bndys_to_add_chr)
  if(x@overwrite_1L_lgl | !file.exists(path_to_centres_sf_1L_chr)){
    centres_sf <- purrr::reduce(x@merge_itms_chr,
                                .init = hss_lup_r3_pt1 %>%
                                  sf::st_as_sf(coords = c("long", "lat"),
                                               crs = x@crs_nbr_dbl[1]),
                                ~ vicinity::make_intersecting_geometries(geometry_one_sf = .x,
                                                                         geometry_two_sf = eval(parse(text=.y)),
                                                                         crs_nbr_dbl = x@crs_nbr_dbl,
                                                                         validate_1L_lgl = F))
    # land_sf <- sf::st_union(eval(parse(text=x@merge_itms_chr[2]))) %>% sf::st_sf()
    saveRDS(centres_sf, file = path_to_centres_sf_1L_chr)
    # starter_sf <- vicinity::join_lon_lat_sfs(polys_sf = land_sf %>% sf::st_sf(),
    #                                             points_sf = starter_sf,
    #                                             crs_nbr_dbl = x@crs_nbr_dbl,
    #                                             validate_1L_lgl = F)

  }else{
    centres_sf <- paste0(path_to_centres_sf_1L_chr
                         #  x@processed_fls_dir_1L_chr,
                         # "/",
                         # vicinity::sp_starter_sf_lup(y_VicinityLookup) %>% dplyr::pull(starter_sf),
                         # ".RDS"
    ) %>% readRDS()
  }
  #if(overwrite_1L_lgl | is.null(vicinity::sp_site_coord_lup(y_VicinityLookup))){
  hss_lup_r3_pt2 <- centres_sf %>%
    dplyr::select(inc_cols_vec) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::mutate_at(.vars = bndys_to_add_chr,
                     .funs = as.character())
  hss_lup_r3 <- dplyr::inner_join(hss_lup_r3_pt1,
                                  hss_lup_r3_pt2) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(.vars = inc_cols_vec, .funs = as.character) %>%
    vicinity::vicinity_points()
  y_VicinityLookup <- renewSlot(y_VicinityLookup,#vicinity::`sp_site_coord_lup<-`
                                "vicinity_points_r3",
                                hss_lup_r3)
  #}
  y_VicinityLookup <- y_VicinityLookup %>%
    renew(template_ls = list(centres_sf) %>% stats::setNames(vicinity_raw_r3$name_chr),#vicinity::add_data_pack_lup
          tbl_data_type_1L_chr = "Geometry",
          package_1L_chr = x@pkg_1L_chr,
          what_1L_chr = "processed")
  y_VicinityLookup <- renewSlot(y_VicinityLookup,#vicinity::`sp_data_pack_lup<-`
                                "vicinity_processed_r3",
                                y_VicinityLookup@vicinity_processed_r3 %>%
                                  dplyr::mutate(source_reference_chr = purrr::map2_chr(area_type_chr,
                                                                                       source_reference_chr,
                                                                                       ~ ifelse(.x == "HSS",
                                                                                                paste0(.y,"_sf"),
                                                                                                .y))))
  return(y_VicinityLookup)
}
author_AusOrygen <- function(x){ # add_aus_oyh_data oyh method?
  y_VicinityLookup <- x@a_VicinityLookup
  vicinity_raw_r3 <- y_VicinityLookup@vicinity_raw_r3
  ready4use::assert_single_row_tb(vicinity_raw_r3)
  import_type_ls <- procure(vicinity_raw_r3) # Not defined - perhaps manufacture?? #TF2A
  ready4use::assert_matches_chr(import_type_ls[[1]],
                                "Aus_16_31_oyh"#"AusSpR4c::add_aus_oyh_data"
  )
  starter_sf_name_1L_chr <- paste0(vicinity_raw_r3$name_chr, "_sf")
  path_to_seed_sf_1L_chr <- paste0(x@processed_fls_dir_1L_chr,"/",starter_sf_name_1L_chr,".RDS")
  if(x@overwrite_1L_lgl | !file.exists(path_to_seed_sf_1L_chr)){
    aus_lga_16_sf <- eval(parse(text = x@merge_itms_chr[1])) #merge_sfs_vec
    # From this point forward names are 2016 versions - function will need to be updated for other years.
    oyh_part_whole_lgas_sf <- aus_lga_16_sf  %>%
      dplyr::filter(LGA_NAME16 %in% c("Brimbank (C)",
                                      "Hobsons Bay (C)",
                                      "Hume (C)",
                                      "Maribyrnong (C)",
                                      "Melton (C)",
                                      "Moonee Valley (C)",
                                      "Moreland (C)",
                                      "Wyndham (C)"))
    mel_lga_16_sf <- aus_lga_16_sf %>%
      dplyr::filter(LGA_NAME16 %in% c("Melbourne (C)"))
    aus_sa1_16_sf <- eval(parse(text = x@merge_itms_chr[2]))#merge_sfs_vec
    mel_lga_sa1s_not_in_oyh_vec <- as.character(c(20604111801,20604111802,20604111808,20604111814,20604112501,20604112502,
                                                  20604112503,20604112504,20604112505,20604112506,20604112507,20604112508,
                                                  20604112509,20604112510,20604112511,20604112512,20604112513,20604112514,
                                                  #20604112601, # deprecated in 2016
                                                  20604112602,20604112603,20604112604,20604112605,20604112606,
                                                  #20604112607, # deprecated in 2016
                                                  #20604112608, # deprecated in 2016
                                                  20604112609,
                                                  20604112610, # incorporates 20604112608 in 2016
                                                  20604112611,20604112612,20604112613,20604112614,20604112615,20604112616,
                                                  20604112617,20604112618,20604112619,20604112620,
                                                  #20604112621,
                                                  20604112622,20604112623,20604112624,20604112625,20604112626,20604112627,
                                                  20604112628,
                                                  20604112629, # new 2016 code
                                                  20604112630, # new 2016 code
                                                  20604112631, # new 2016 code
                                                  20604112632, # new 2016 code
                                                  20604112633, # new 2016 code
                                                  20604112634, # new 2016 code
                                                  20604112635, # new 2016 code
                                                  20605113102))
    mel_lga_not_oyh_sf <- aus_sa1_16_sf %>%
      dplyr::filter(SA1_MAIN16 %in% mel_lga_sa1s_not_in_oyh_vec)
    mel_lga_in_oyh_sf <- vicinity::get_set_diff_lng_lat_sf(geometry_sf = mel_lga_16_sf,
                                                           cut_sf = mel_lga_not_oyh_sf,
                                                           crs_nbr_dbl = x@crs_nbr_dbl)
    ###
    # intersects_sf <- sf::st_intersection(oyh_part_whole_lgas_sf,mel_lga_in_oyh_sf)
    # sf::st_difference(starter_sf,intersects_sf) %>% sf::st_union() %>% plot()
    # test <- sf::st_cast(starter_sf, "POLYGON")
    ####
    # sf::st_intersection
    # "Maribyrnong (C)",
    # "Moonee Valley (C)",
    # "Moreland (C)",
    #
    # sf::st_intersects(starter_sf) %>% dplyr::filter(LGA_NAME16 == "Melton (C)"),
    #                   starter_sf %>% dplyr::filter(LGA_NAME16 == "Wyndham (C)"))
    # ,
    # "Wyndham (C)"
    ####
    starter_sf <- rbind(oyh_part_whole_lgas_sf,
                        mel_lga_in_oyh_sf) %>%
      dplyr::mutate(SMH_NAME16 = "Orygen") %>%
      dplyr::group_by(SMH_NAME16) %>%
      dplyr::summarise_at(dplyr::vars(c("STE_CODE16","STE_NAME16")),
                          .funs = dplyr::first) %>%
      dplyr::mutate(AREA_KMSQ = sf::st_area(.) %>%
                      units::set_units(km^2))  %>%
      dplyr::select(SMH_NAME16, STE_CODE16, STE_NAME16, AREA_KMSQ)

    saveRDS(starter_sf, file = path_to_seed_sf_1L_chr)
  }else{
    starter_sf <- "SKIP_IMPORT"
  }
  starter_sf_lup_r3 <- tibble::add_row(y_VicinityLookup@vicinity_templates_r3,#vicinity::sp_starter_sf_lup(
                                       country_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(country_chr),
                                       area_type_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(area_type_chr),
                                       area_bndy_yr_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(area_bndy_yr_chr),
                                       starter_sf_nm_chr = starter_sf_name_1L_chr,
                                       subdivision_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(uid_chr))
  y_VicinityLookup <- renew(y_VicinityLookup, #vicinity::`sp_starter_sf_lup<-`
                            "vicinity_templates_r3",
                            starter_sf_lup_r3) %>%
    renew(what_1L_chr = "identifiers")
  y_VicinityLookup <- y_VicinityLookup %>%
    renew(template_ls = list(starter_sf) %>%
            stats::setNames(vicinity_raw_r3$name_chr),#vicinity::add_data_pack_lup
          tbl_data_type_1L_chr = "Geometry", # "Shape"
          package_1L_chr = x@pkg_1L_chr,
          what_1L_chr = "processed"#, #lup_dir = lup_dir
    )
  return(y_VicinityLookup)
}
author_AusProjections <- function(x#, # xx1 method? # add_ppr_custom_bound_data
                                      # y_VicinityLookup,
                                      # merge_sfs_vec,
                                      # processed_fls_dir_1L_chr,
                                      # raw_fls_dir_1L_chr,
                                      # package_1L_chr,
                                      # overwrite_1L_lgl
){ ## NOTE: OTHER TERRITORIES ARE NOT COVERED BY THIS FUNCTION
  y_VicinityLookup <- x@a_VicinityLookup
  z_vicinity_raw <- y_VicinityLookup@vicinity_raw_r3
  ready4use::assert_single_row_tb(z_vicinity_raw)
  import_type_ls <- procure(z_vicinity_raw) ## NOT DEFINED ####TF2A
  ready4use::assert_matches_chr(import_type_ls[[1]],
                                "Aus_16_31_xx1")
  # x <- x %>%
  #  dplyr::filter(make_script_src == "Aus_16_31_xx1")#AusSpR4c::add_ppr_custom_bound_data
  path_to_seed_sf_1L_chr <- vicinity::make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr,
                                                                   name_chr = z_vicinity_raw$name_chr,
                                                                   data_type_chr = "Geometry")
  x <- renewSlot(x,"path_to_seed_sf_1L_chr",path_to_seed_sf_1L_chr)
  y_VicinityLookup <- renew(y_VicinityLookup,
                            path_1L_chr = x@path_to_seed_sf_1L_chr,
                            what_1L_chr = "templates") %>%
    renew(what_1L_chr = "identifiers")
  starter_sf_name <- vicinity::get_name_from_path_chr(path_to_seed_sf_1L_chr, with_ext_1L_lgl = F)
  #starter_sf_name <- paste0(x$name, "_sf")
  #path_to_seed_sf_1L_chr <- paste0(x@processed_fls_dir_1L_chr,"/",starter_sf_name,".RDS")#processed_fls_dir_1L_chr
  if(x@overwrite_1L_lgl | !file.exists(path_to_seed_sf_1L_chr)){
    aus_ste_nat_bnd_2016_sf <- eval(parse(text = x@merge_itms_chr[1]))#merge_sfs_vec
    aus_lga_nat_bnd_2016_sf <- eval(parse(text = x@merge_itms_chr[2]))#merge_sfs_vec
    aus_lga_nat_bnd_2011_sf <- eval(parse(text = x@merge_itms_chr[3]))#merge_sfs_vec
    aus_sa3_nat_bnd_2016_sf <- eval(parse(text = x@merge_itms_chr[4]))#merge_sfs_vec
    lga_2016_states_vec <- c("New South Wales", "Queensland", "Western Australia")
    lga_2011_states_vec <- c("South Australia", "Tasmania", "Victoria")
    sa3_2016_states_vec <- c("Australian Capital Territory", "Northern Territory")
    states_vec <- aus_ste_nat_bnd_2016_sf %>% dplyr::pull(STE_NAME16) %>% as.character() %>% unique()
    var_nms_chr <- c("STE_NAME16","STE_NAME11","STE_NAME16")
    uids_chr <- c("LGA_NAME16","LGA_NAME11","SA3_NAME16")
    no_stat_ls <- make_no_stat_ls(area_geoms_ls = list(lga_2016_states_vec,lga_2011_states_vec,sa3_2016_states_vec),
                                  area_atts_ls = list(lga_2016_states_vec,lga_2011_states_vec,sa3_2016_states_vec),
                                  statistic_sfs_ls = list(aus_lga_nat_bnd_2016_sf,aus_lga_nat_bnd_2011_sf,aus_sa3_nat_bnd_2016_sf),
                                  var_nms_chr = var_nms_chr,
                                  land_sf = aus_ste_nat_bnd_2016_sf,
                                  land_name_1L_chr = "STE_NAME16",
                                  feature_pfx_1L_chr = "No usual address",
                                  uids_chr = uids_chr)
    ## Below section is based on no_stat_ls only including LGA_NAME11 subdivisions. Needs to be abstracted further if this function is
    ## to be used for other data inputs.
    no_stat_sf <- purrr::map(no_stat_ls,
                             ~ .x %>% dplyr::select(-c("STE_CODE11", "STE_NAME11", "AREA_SQKM", "AREASQKM16","STE_CODE16"#,
                                                       #"STE_NAME16"
                             )) %>%
                               dplyr::mutate(LGA_CODE16 = NA_character_,
                                             LGA_NAME16 = NA_character_,
                                             SA3_CODE16 = NA_character_,
                                             SA3_NAME16 = NA_character_)) %>%
      do.call(what = rbind)
    ##
    full_coverage_uids_vec <- uids_chr[intersect(uids_chr,names(no_stat_sf)) %>%
                                             purrr::map_lgl(~ no_stat_sf %>%
                                                              dplyr::pull(.x) %>%
                                                              is.na() %>% all())]
    starter_sf <- list(lga_2016_sf = aus_lga_nat_bnd_2016_sf %>% dplyr::filter(STE_NAME16 %in% lga_2016_states_vec) %>%
                         dplyr::select(-c("STE_CODE16",
                                          #"STE_NAME16",
                                          "AREASQKM16")) %>%
                         dplyr::mutate(LGA_CODE11 = NA_character_,
                                       LGA_NAME11 = NA_character_,
                                       SA3_CODE16 = NA_character_,
                                       SA3_NAME16 = NA_character_),
                       lga_2011_sf = aus_lga_nat_bnd_2011_sf %>% dplyr::filter(STE_NAME11 %in% lga_2011_states_vec) %>%
                         dplyr::select(-c("STE_CODE11",
                                          #"STE_NAME11",
                                          "AREA_SQKM")) %>%
                         dplyr::mutate(LGA_CODE16 = NA_character_,
                                       LGA_NAME16 = NA_character_,
                                       SA3_CODE16 = NA_character_,
                                       SA3_NAME16 = NA_character_) %>%
                         dplyr::rename(STE_NAME16 = STE_NAME11),
                       sa3_2016_sf = aus_sa3_nat_bnd_2016_sf %>% dplyr::filter(STE_NAME16 %in% sa3_2016_states_vec) %>%
                         dplyr::select(-c("SA4_CODE16", "SA4_NAME16", "GCC_CODE16", "GCC_NAME16", "STE_CODE16",
                                          #"STE_NAME16",
                                          "AREASQKM16")) %>%
                         dplyr::mutate(LGA_CODE11 = NA_character_,
                                       LGA_NAME11 = NA_character_,
                                       LGA_CODE16 = NA_character_,
                                       LGA_NAME16 = NA_character_)) %>%
      # purrr::map2(uids_chr,
      #             ~ {
      #               if(.y %in% full_coverage_uids_vec){
      #                 remove_feature_by_prefix_sf(data_sf = .x,
      #                                             var_name_1L_chr = .y,
      #                                             prefix_str = "Unincorp")
      #               }else{
      #                 .x
      #               }
      #             }) %>%
      append(list(no_stat_sf)) %>%
      do.call(what = rbind)
    #starter_sf_1 <- sf::st_join(aus_ste_nat_bnd_2016_sf %>% dplyr::select(-AREASQKM16), starter_sf, join = sf::st_touches)
    starter_sf <- starter_sf %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(XX1_NAME16 = rowname) %>%
      vicinity::make_valid_new_sf() ## NEW
    ##
    saveRDS(starter_sf, file = path_to_seed_sf_1L_chr)
  }else{
    starter_sf <- "SKIP_IMPORT"
  }
  y_VicinityLookup <- x@a_VicinityLookup
  starter_sf_lup_r3 <- tibble::add_row(y_VicinityLookup@vicinity_templates_r3,
                                       country_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(country_chr),
                                       area_type_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(area_type_chr),
                                       area_bndy_yr_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(area_bndy_yr_chr),
                                       starter_sf = starter_sf_name,
                                       subdivision_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(uid_chr))
  y_VicinityLookup <- renewSlot(y_VicinityLookup,
                                "vicinity_templates_r3",
                                starter_sf_lup_r3) %>%
    renew(what_1L_chr = "identifiers")
  y_VicinityLookup <- y_VicinityLookup %>%
    renew(template_ls = list(starter_sf) %>% # add_data_pack_lup
            stats::setNames(y_VicinityLookup@vicinity_raw_r3 %>%
                              dplyr::pull(name_chr)), ## COULD NEED SF SUFFIX - ONLY CHECK IF BREAKS DOWN.
          tbl_data_type_1L_chr = "Geometry", #"Shape
          package_1L_chr = x@pkg_1L_chr,
          what_1L_chr = "processed")
  return(y_VicinityLookup)
}
