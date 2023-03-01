library(ready4fun)
library(ready4use)
# library(ready4show)
# library(youthvars)
# library(scorz)
# library(specific)
library(sf)
library(vicinity)
ready4fun::write_fn_type_dirs()
#
# MANUAL STEP. Write all your functions to R files in the new "fns" directory.
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Model Australian Spatial Data With Ready4",
                                 pkg_desc_1L_chr = "Tools for modelling Australia specific geometry and spatial attribute data for use with the ready4 youth mental health systems model (https://ready4-dev.github.io/ready4/).
                            This development version of the aus package has been made available as part of the process of testing and documenting the package. It is currently highly unstable and is not yet recommended for use.
                            If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                                 authors_prsn = c(#utils::person(given = "Caroline",family = "Gao",email = "caroline.gao@orygen.org.au", role = c("aut"),comment = c(ORCID = "0000-0002-0987-2759")),
                                   utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton@orygen.org.au", role = c("aut", "cre"),comment = c(ORCID = "0000-0001-7407-9194")),
                                   utils::person("Orygen", role = c("cph", "fnd")),
                                   utils::person("Headspace", role = c( "fnd")),
                                   utils::person("National Health and Medical Research Council", role = c( "fnd"))),
                                 urls_chr = c("https://ready4-dev.github.io/aus/",
                                              "https://github.com/ready4-dev/aus",
                                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = c("rmarkdown"),
                                                                       imports_chr = c("knitrBootstrap")#, depends_chr = c("osrm") # required?
  ),
  build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
  check_type_1L_chr = "ready4",
  copyright_holders_chr = "Orygen",
  custom_dmt_ls = ready4fun::make_custom_dmt_ls(),##
  dev_pkgs_chr = c("ready4","ready4use","vicinity"),
  lifecycle_stage_1L_chr = "experimental",
  path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/s2lsd-logo/default.png",# UPDATE
  piggyback_to_1L_chr = "ready4-dev/ready4",
  ready4_type_1L_chr = "modelling",  
  zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7687126.svg)](https://doi.org/10.5281/zenodo.7687126)"
  )
y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                 name_stub_chr = "Lookup",#"script_data", #Aus_16_31Arguments??
                                                 class_desc_chr = "Lookup tables for Australian geometry and spatial attribute data.",
                                                 pt_ls = list("vicinity_abbreviations") %>% list(),
                                                 slots_ls = list("vicinity_abbreviations_r3") %>% list(),#a_VicinityLookup # needed?
                                                 parent_class_chr = "VicinityLookup"), # VicinityArguments
  # ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
  #                                              name_stub_chr = "_16_31_Arguments",#"script_data", #Aus_16_31Arguments??
  #                                              class_desc_chr = "Data for constructing Australian spatial objects from scripts.",
  #                                              # pt_ls = list("VicinityLookup") %>% list(),
  #                                              # slots_ls = list("lup_tbs_r4") %>% list(),#a_VicinityLookup # needed?
  #                                              parent_class_chr = "VicinityArguments") %>% # VicinityArguments
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                 name_stub_chr = "Orygen",#"oyh",
                                                 class_desc_chr = "Meta data for constructing OYH Specialist Mental Health Catchment geometries.",
                                                 pt_ls = list("VicinityLookup"#,
                                                              # "numeric",
                                                              # "character",
                                                              # "character",
                                                              # "character",
                                                              # "character",
                                                              # "list",
                                                              # "character",
                                                              # "character",
                                                              # "character",
                                                              # "logical",
                                                              #"logical"
                                                              ) %>% list(),
                                                 slots_ls = list("a_VicinityLookup"#,
                                                                 # "crs_nbr_dbl",
                                                                 # "write_type_1L_chr",
                                                                 # "processed_fls_dir_1L_chr",
                                                                 # "imports_chr",
                                                                 # "path_to_seed_sf_1L_chr",
                                                                 # "imports_ls",
                                                                 # "merge_itms_chr",
                                                                 # "raw_fls_dir_1L_chr",
                                                                 # "pkg_1L_chr",
                                                                 # "overwrite_1L_lgl",
                                                                 # "write_1L_lgl"#, "function_nm_1L_chr"
                                                                 ) %>% list(), # "algorithm_name
                                                 #vals_ls = list(function_nm_1L_chr = "'Aus_16_31_oyh'") %>% list(),
                                                 #allowed_vals_ls = list(function_nm_1L_chr = "'Aus_16_31_oyh'") %>% list(),
                                                 parent_class_chr = "VicinityArguments"),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                 name_stub_chr ="Headspace", #"hss",
                                                 class_desc_chr = "Meta data for constructing Headspace Centre geometries.",
                                                 pt_ls = list("VicinityLookup") %>% list(),
                                                 slots_ls = list("a_VicinityLookup") %>% list(),
                                                 #vals_ls = list(function_nm_1L_chr = "'Aus_16_31_hss'"),
                                                 #allowed_vals_ls = list(function_nm_1L_chr = "'Aus_16_31_hss'"),
                                                 parent_class_chr = "VicinityArguments"),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                 name_stub_chr = "Projections",#"xx1",
                                                 class_desc_chr = "Meta data for constructing custom Australian population projections boundary.",
                                                 pt_ls = list("VicinityLookup") %>% list(),
                                                 slots_ls = list("a_VicinityLookup") %>% list(),
                                                 #vals_ls = list(function_nm_1L_chr = "'Aus_16_31_xx1'"),
                                                 #allowed_vals_ls = list(function_nm_1L_chr = "'Aus_16_31_xx1'"),
                                                 parent_class_chr = "VicinityArguments"),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                 name_stub_chr = "Tasmania",#"add_tas_ppr_data",
                                                 class_desc_chr = "Meta data for processing Tasmanian population projections.",
                                                 pt_ls = list("VicinityLookup") %>% list(),
                                                 slots_ls = list("a_VicinityLookup") %>% list(),
                                                 #vals_ls = list(function_nm_1L_chr = "'add_tas_ppr_data'"),
                                                 #allowed_vals_ls = list(function_nm_1L_chr = "'add_tas_ppr_data'"),
                                                 parent_class_chr = "VicinityArguments"),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                 name_stub_chr = "ACT",#"make_act_ppr_2016",
                                                 class_desc_chr = "Meta data for processing ACT population projections.",
                                                 pt_ls = list("VicinityLookup") %>% list(),
                                                 slots_ls = list("a_VicinityLookup") %>% list(),
                                                 #vals_ls = list(function_nm_1L_chr = "'make_act_ppr_2016'") %>% list(),
                                                 #allowed_vals_ls = list(function_nm_1L_chr = "'make_act_ppr_2016'") %>% list(),
                                                 parent_class_chr = "VicinityArguments")
  )

z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y#, pkg_ds_ls_ls = datasets_ls
) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
#usethis::use_package("sf")
ready4::write_extra_pkgs_to_actions()
ready4::write_citation_cff(packageDescription("aus"),
                           citation_chr = readLines("inst/CITATION"))
# usethis::use_dev_package("youthvars",
#                          type = "Imports",#D?
#                          remote = "ready4-dev/youthvars")
# usethis::use_dev_package("scorz",
#                          type = "Imports",
#                          remote = "ready4-dev/scorz")
devtools::build_vignettes()
