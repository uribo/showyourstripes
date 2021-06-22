library(targets)
library(jmastats)
library(dplyr)

list(
  tar_target(
    tgt_st,
    c(
      #"稚内", "旭川", "網走",
      "札幌",
      #"帯広",
      #"釧路", "室蘭", "函館",
      "青森", "秋田", "盛岡", "山形",
      "仙台", "福島", "新潟", "金沢", "富山", "長野", "宇都宮",
      "福井", "前橋", "熊谷", "水戸", "岐阜", "名古屋",
      "甲府", "銚子", "津", "静岡", "東京", "横浜", "松江",
      "鳥取", "京都", "彦根", "下関", "広島", "岡山", "神戸",
      "大阪", "和歌山", "奈良", "福岡", "佐賀", "大分",
      "長崎", "熊本", "鹿児島", "宮崎", "松山", "高松",
      "高知", "徳島",
      #"名瀬", "石垣島", "宮古島",
      "那覇"
      #,
      #"南大東"
    ) %>%
      ensurer::ensure(length(.) == 47L)
  ),
  tar_target(
    df_stations,
      stations %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(station_name %in% tgt_st,
             !station_no %in% c(23281, 33136, 54191, 74436)) %>%
      dplyr::distinct(station_no, station_name, block_no, pref_code)
  ),
  tar_target(
    note_markers,
    c("--", "\\)", "]", "\u00d7", "///", "#", "\\*", "@")
  ),
  tar_target(
    df_temperature,
    {
      if (length(fs::dir_ls(here::here("data"), regexp = "jma_st.+.csv")) != 47L) {
        source(here::here("R/collect_jma_data.R"))
        purrr::walk2(
          df_stations$station_name,
          df_stations$block_no,
          ~ slowly_save_csv(.x, .y)
        )
      }
      fs::dir_ls(here::here("data"), regexp = "jma_st.+.csv") %>%
        purrr::set_names(stringr::str_remove(stringr::str_extract(basename(.),
                                                                  "block_no[0-9]{1,}"),
                                             "block_no")) %>%
        purrr::map_dfr(readr::read_csv,
                       col_types = c("ddcc"),
                       .id = "block_no") %>%
        dplyr::filter(!is.na(value), is.na(note)) %>%
        dplyr::group_by(block_no, station_name) %>%
        dplyr::mutate(diff = value - mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(block_no)
    }
  )
)

# targets::tar_make()
