update_area_names <- function(area_dss_by_year_ls,
                              area_1L_chr,
                              altv_names_sf,
                              state_nms_chr,
                              state_short_and_long_nms_chr,
                              remove_symbol_1L_lgl = TRUE){
  area_names_chr <- altv_names_sf %>%
    dplyr::pull(!!rlang::sym(area_1L_chr)) # From ste_names
  if(length(area_names_chr) == (as.numeric(as.character(area_names_chr))[!is.na(as.numeric(as.character(area_names_chr)))] %>% length())){
    updated_area_dss_by_year_ls <- area_dss_by_year_ls
  }else{
    altv_names_chr <- altv_names_sf %>% dplyr::filter(!!rlang::sym(state_nms_chr) == state_short_and_long_nms_chr[2]) %>%
      dplyr::pull(!!rlang::sym(area_1L_chr)) %>% unique() %>% as.character() %>% purrr::discard(is.na)
    short_altv_names_chr <- altv_names_chr %>% stringr::str_replace_all(paste0(" \\(",state_short_and_long_nms_chr[1],"\\)"),"") %>%
      stringr::str_replace_all(" Shire", "")
    current_names_chr <- area_dss_by_year_ls %>% purrr::pluck(1) %>%
      dplyr::pull(!!rlang::sym(area_1L_chr))
    if(remove_symbol_1L_lgl){
      short_altv_names_chr <- purrr::map2_chr(altv_names_chr,
                                              short_altv_names_chr %>%
                                                stringr::str_locate_all(" ") %>%
                                                purrr::map_int(~ .x[length(.x)]),
                                              ~ stringr::str_sub(.x, end = .y-1) %>% tolower() %>% tools::toTitleCase())
      current_names_chr <- current_names_chr %>%
        stringr::str_replace_all(paste0(" \\(","DC","\\)"),"")  %>%
        stringr::str_replace_all(paste0(" \\(","AC","\\)"),"") %>%
        stringr::str_replace_all(paste0(" \\(","C","\\)"),"") %>%
        stringr::str_replace_all(paste0(" \\(","T","\\)"),"")%>%
        stringr::str_replace_all(paste0(" \\(","M","\\)"),"") %>%
        stringr::str_replace_all(paste0(" \\(","RC","\\)"),"") %>%
        stringr::str_replace_all(paste0(" \\(","RegC","\\)"),"") %>%
        stringr::str_replace_all(paste0(" \\(","S","\\)"),"") %>%
        stringr::str_replace_all(paste0(" \\(","R","\\)"),"") %>%
        stringr::str_replace_all(paste0(" \\(","Qld","\\)"),"")
    }
    short_altv_names_chr <- short_altv_names_chr %>%
      stringr::str_replace_all(" - ", "-") %>%
      stringr::str_replace("Fyshwick-Pialligo-Hume","Fyshwick-Pialliago-Hume") %>%
      stringr::str_replace("Break O'day","Break Oday") %>%
      stringr::str_replace("Waratah/Wynyard", "Waratah-Wynyard") %>%
      stringr::str_replace("Woden Valley", "Woden") %>%
      stringr::str_replace(" \\(m\\)","") %>%
      stringr::str_replace(" \\(r\\)","") %>%
      stringr::str_replace(" \\(s\\)","") %>%
      stringr::str_replace(" \\(c\\)","") %>%
      stringr::str_replace("/"," / ")
    matched_names_chr <- purrr::map_chr(current_names_chr[current_names_chr != "Unallocated" & !startsWith(current_names_chr,"Unincorporated")] %>%
                                          tolower() %>%
                                          tools::toTitleCase() %>%
                                          stringr::str_replace(" - ", "-") %>%
                                          stringr::str_replace(paste0(" ",state_short_and_long_nms_chr[1] %>%
                                                                        tolower() %>%
                                                                        tools::toTitleCase()), "") %>%
                                          stringr::str_replace_all("'","") %>%
                                          stringr::str_replace_all("/"," / ") %>%
                                          stringr::str_replace_all("  "," ") %>%
                                          stringr::str_replace(" \\(b\\)",""),
                                        ~ altv_names_chr[short_altv_names_chr == .x])
    # setdiff(altv_names_chr,matched_names_chr)
    updated_area_dss_by_year_ls <-  purrr::map(area_dss_by_year_ls,
                                               ~ .x %>%
                                                 dplyr::filter(!!rlang::sym(area_1L_chr) != "Unallocated" & !startsWith(!!rlang::sym(area_1L_chr),"Unincorporated")) %>%
                                                 dplyr::mutate(!!rlang::sym(area_1L_chr) := matched_names_chr) %>%
                                                 rbind(.x %>% dplyr::filter(!!rlang::sym(area_1L_chr) == "Unallocated" | startsWith(!!rlang::sym(area_1L_chr),"Unincorporated"))))
  }
  return(updated_area_dss_by_year_ls)
}
