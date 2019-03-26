# make Mari transfer files
library(tidyverse)
library(lubridate)

make_mari_header <- function() {
  d <- data.frame(
    V1 = c("Rack", "1", rep("", 4)),
    V2 = c("Src.Barcode", rep("", 5)),
    V3 = c("Src.List Name", rep("", 5)),
    V4 = c("Dest.Barcode", rep("", 5)),
    V5 = c("Dest.List Name", rep("", 5)),
    V6 = rep("", 6),
    V7 = rep("", 6),
    V8 = rep("", 6)
  )
  return(d)
}

fill_header <- data.frame(
  V1 = "Barcode ID",
  V2 = "Rack",
  V3 = "Source",
  V4 = "Rack",
  V5 = "Destination",
  V6 = "Volume",
  V7 = "Tool",
  V8 = "Name",
  stringsAsFactors = FALSE
)

make_transfer <-
  function(experiment_data,
           dilute_by,
           which_plate,
           which_transfer,
           starting_rfu,
           max_volume) {
    
    trans_val <- as.name(dilute_by)
    max_date <- max(experiment_data$date)
    which_plate <- as.character(which_plate)
    which_transfer <- as.character(which_transfer)
    
    mari_master <-
      experiment_data %>%
      filter(plate_name %in% which_plate) %>% 
      filter(transfer %in% which_transfer) %>%
      filter(date == max_date) %>%
      select(date,
             plate_name,
             well,
             !!trans_val,
             strain,
             treatment,
             replicate) %>%
      mutate(dilution_tmp = !!trans_val / starting_rfu) %>%
      mutate(dilution = ifelse(!!trans_val <= 1.25 * starting_rfu, 1.168, dilution_tmp)) %>%
      mutate(media = floor((max_volume / dilution) * (dilution - 1))) %>%
      mutate(culture = ceiling(max_volume - media)) %>%
      mutate(fill_tool = "TS_1000") %>%
      mutate( 
        transfer_tool = case_when(
          culture < 20 ~ "TS_50",
          culture >= 20 & culture < 280 ~ "TS_300",
          culture >= 280 ~ "TS_1000"
        )
      ) %>%
      mutate(name = paste(strain, treatment, replicate, sep = "|")) %>%
      mutate(salt = as.numeric(treatment)) %>%
      mutate(V2 = "1", V4 = "1") %>%
      mutate(well2 = well) %>%
      arrange(plate_name, salt)
    print(mari_master)
    
    # make Mari fill and transfer sheets
    mari_fill <-
      mari_master %>%
      rename(
        V1 = plate_name,
        V3 = salt,
        V5 = well2,
        V6 = media,
        V7 = fill_tool,
        V8 = name
      ) %>%
      # mutate(V0 = V1) %>%
      map_dfc(., as.character) %>%
      select(starts_with("V")) 
    print(mari_fill)
    
    mari_transfer <-
      mari_master %>%
      rename(
        V1 = plate_name,
        V3 = well,
        V5 = well2,
        V6 = culture,
        V7 = transfer_tool,
        V8 = name
      ) %>%
      # mutate(V0 = V1) %>%
      map_dfc(., as.character) %>%
      select(starts_with("V"))
    print(mari_transfer)
    
    mari_header_fill <- bind_rows(make_mari_header(), fill_header, mari_fill)
    mari_header_transfer <- bind_rows(make_mari_header(), fill_header, mari_transfer)
    return(list(mari_header_fill,mari_header_transfer))
  }
