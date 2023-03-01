add_tas_ppr_data <- function(x){
  raw_att_dir <- vicinity::make_path_for_raw_outp_dir(raw_fls_dir_1L_chr = x@raw_fls_dir_1L_chr,#raw_fls_dir_1L_chr,
                                                      category_1L_chr = "Attributes")
  y_VicinityLookup <- x@a_VicinityLookup
  if(!dir.exists(raw_att_dir))
    dir.create(raw_att_dir)
  attributes_to_import_vec <- y_VicinityLookup@vicinity_raw_r3 %>%
    dplyr::filter(data_type_chr == "Attribute") %>% dplyr::pull(name_chr)
  author(x = y_VicinityLookup@vicinity_raw_r3,
           match_vals_xx = attributes_to_import_vec,
                      path_1L_chr = raw_att_dir,
                      overwrite_1L_lgl = x@overwrite_1L_lgl)
  #### IMPORTANT: MANUAL STEP REQUIRED HERE: TO BE REPLACED WITH R SCRIPT THAT CALLS MACRO
  #### See:
  #### https://www.listendata.com/2016/07/run-vba-in-r.html
  #### https://stackoverflow.com/questions/19404270/run-vba-script-from-r
  #### File worksheets imported using Macro described here: https://stackoverflow.com/questions/12162477/importing-multiple-csv-to-multiple-worksheet-in-a-single-workbook
  #### (To implement, press Alt and F11, select Insert Module, copy code with updated fpath and press run / f5
  attribute_ls <- ingest(x = y_VicinityLookup@vicinity_raw_r3 %>% #ready4use::import_data
                                             dplyr::mutate(new_nms_for_inc_fls_ls = ifelse(main_feature_chr == "PPR" & region == "TAS",
                                                                                            local_file_src_chr,
                                                                                            new_nms_for_inc_fls_ls),
                                                           inc_file_main_chr = ifelse(main_feature_chr == "PPR" & region == "TAS",
                                                                                  local_file_src_chr,
                                                                                  inc_file_main_chr)),
                                           imports_chr = attributes_to_import_vec,
                                           data_type_1L_chr = "Attribute",
                                           raw_fls_dir_1L_chr = raw_att_dir,
                                           processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr,#processed_fls_dir_1L_chr,
                                           write_1L_lgl = x@overwrite_1L_lgl)  %>%
    stats::setNames(attributes_to_import_vec)
  purrr::walk2(attribute_ls,
               names(attribute_ls),
               ~ vicinity::write_att_tb(att_tb = .x,
                                             obj_name = .y,
                                             processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr,
                                             overwrite_1L_lgl = x@overwrite_1L_lgl))
  y_VicinityLookup <- y_VicinityLookup %>%
    renew(template_ls = attribute_ls,#vicinity::add_data_pack_lup
                                      tbl_data_type_1L_chr = "Attribute",
                                      package_1L_chr = x@pkg_1L_chr,
          what_1L_chr = "processed")
  return(y_VicinityLookup)
}
