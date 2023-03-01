## 1. MAKE CLASSES
## 1.2 Create make Table
aus_16_31_s4_make_tb <- ready4class::ready4_constructor_tbl() %>%
  tibble::add_case(name_stub = "lookup",
                   class_desc = "Lookup tables for Australian spatial context",
                   prototype = list(c("character")),
                   class_slots = list(c("context")),
                   values = list("Australia"),
                   allowed_values = list(context = "Australia"),
                   parent_class = "VicinityLookup",
                   meaningful_names = list(list(Abbreviations = "sp_abbreviations_lup",
                                                Raw_Data_Source = "sp_import_lup",
                                                Data_Packs = "sp_data_pack_lup",
                                                Data_Resolution = "sp_resolution_lup",
                                                Site_Coordinates = "sp_site_coord_lup",
                                                Starter_Geometries = "sp_starter_sf_lup",
                                                Unique_IDs = "sp_uid_lup",
                                                Context = "context"))) %>%
  tibble::add_case(name_stub = "script_data", #Aus_16_31Arguments??
                   class_desc = "Data for constructing Australian spatial objects from scripts.",
                   prototype = c("VicinityLookup") %>% list(),
                   class_slots = c("lup_tbs_r4") %>% list(),#a_VicinityLookup # needed?
                   parent_class = "ready4Arguments") %>% # VicinityArguments
  tibble::add_case(name_stub = "oyh",
                   class_desc = "Meta data for constructing OYH Specialist Mental Health Catchment geometries.",
                   prototype = c("character") %>% list(),
                   class_slots = c("algorithm_name") %>% list(),
                   values = list("Aus_16_31_oyh"),
                   allowed_values = list(algorithm_name = "Aus_16_31_oyh"),
                   parent_class = "Aus_16_31Arguments") %>%
  tibble::add_case(name_stub = "hss",
                   class_desc = "Meta data for constructing Headspace Centre geometries.",
                   prototype = c("character") %>% list(),
                   class_slots = c("algorithm_name") %>% list(),
                   values = list("Aus_16_31_hss"),
                   allowed_values = list(algorithm_name = "Aus_16_31_hss"),
                   parent_class = "Aus_16_31Arguments") %>%
  tibble::add_case(name_stub = "xx1",
                   class_desc = "Meta data for constructing custom Australian population projections boundary.",
                   prototype = c("character") %>% list(),
                   class_slots = c("algorithm_name") %>% list(),
                   values = list("Aus_16_31_xx1"),
                   allowed_values = list(algorithm_name = "Aus_16_31_xx1"),
                   parent_class = "Aus_16_31Arguments") %>%
  tibble::add_case(name_stub = "add_tas_ppr_data",
                   class_desc = "Meta data for processing Tasmanian population projections.",
                   prototype = c("character") %>% list(),
                   class_slots = c("algorithm_name") %>% list(),
                   values = list("add_tas_ppr_data"),
                   allowed_values = list(algorithm_name = "add_tas_ppr_data"),
                   parent_class = "Aus_16_31Arguments") %>%
  tibble::add_case(name_stub = "make_act_ppr_2016",
                   class_desc = "Meta data for processing ACT population projections.",
                   prototype = c("character") %>% list(),
                   class_slots = c("algorithm_name") %>% list(),
                   values = list("make_act_ppr_2016"),
                   allowed_values = list(algorithm_name = "make_act_ppr_2016"),
                   parent_class = "Aus_16_31Arguments") %>%
  dplyr::mutate(make_s3 = FALSE)  %>%
<<<<<<< HEAD
  ready4class::remake_ls_cols()
=======
  ready4use::renew()
>>>>>>> master
  # dplyr::mutate_at(dplyr::vars(prototype,
  #                              prototype_checker_prefix,
  #                              prototype_namespace,
  #                              class_slots,
  #                              include_classes
  # ),
  # ~ purrr::map(., ~ list(.x)))
