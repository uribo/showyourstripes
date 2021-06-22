separate_note_cols <- function(cols) {
  x <-
    cols %>%
    purrr::map(
      ~ tibble::tibble(
        value = .x,
        note = stringr::str_extract_all(.x,
                                        paste0(note_markers, collapse = "|"),
                                        simplify = TRUE)
      ) %>%
        dplyr::mutate(value = stringr::str_remove_all(value,
                                                      paste0(note_markers,
                                                             collapse = "|")) %>%
                        dplyr::if_else(. %in% c("", "\u00d7"),
                                       NA_character_,
                                       .)) %>%
        readr::type_convert(col_types = "dc"))
  df1 <-
    seq.int(length(x)) %>%
    purrr::map_dfc(~ parse_unit(purrr::set_names(x[[.x]][, 1],
                                                 names(x)[.x]),
                                rename = TRUE))
  df2 <-
    seq.int(length(x)) %>%
    purrr::map_dfc(~ purrr::set_names(x[[.x]][, 2],
                                      paste0(names(df1)[.x], "_note")))

  df1 %>%
    purrr::set_names(paste0(names(.),
                            "_value")) %>%
    dplyr::bind_cols(df2) %>%
    dplyr::select(tidyselect::contains(names(.) %>%
                                         stringr::str_remove("_(value|note)")))
}

make_temperature_df <- function(station_name, block_no) {
  target <-
    jmastats:::jma_url(item = "annually",
                       block_no = block_no,
                       year = 2020)
  df_raw <-
    xml2::read_html(target$url) %>%
    rvest::html_table(fill = TRUE)
  selected_item <-
    paste0("annually", "_", target$station_type)
  df <-
    df_raw[[4]][-c(1:2), ]
  names(df) <-
    jmastats:::name_sets(selected_item)
  dplyr::bind_cols(df[, "year"],
                   df[, "temperature_average(℃)"] %>%
                     separate_note_cols()) %>%
    tidyr::pack(
      temperature_average = starts_with("temperature_average"),
      .names_sep = "_c_") %>%
    tidyr::unpack(cols = temperature_average) %>%
    dplyr::mutate(value = units::drop_units(value)) %>%
    dplyr::mutate(station_name = station_name)
}

slowly_save_csv <-
  purrr::slowly(~ save_csv(.x, .y),
                rate = purrr::rate_delay(pause = 8))
save_csv <- function(station_name, block_no) {
  make_temperature_df(station_name,
                      block_no) %>%
    readr::write_csv(glue::glue("data/jma_st{station_name}_block_no{block_no}.csv"))
}
