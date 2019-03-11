# summarise _all_ data into one frame to work with the flexdashboard
library(tidyverse)
library(lubridate)

# load experiment meta data
all_plates_meta <- get(load("metadata/all_plates_meta.Rsave"))

# if your data is not in a binary R file, comment the line above and uncomment + edit the path below:
# all_plates_meta <- read_csv("metadata/all_plates_meta.csv"))

# where are the data stored
path_to_experiment <- "measurements"

# get the file names
(
  files <-
    list.files(
      path = path_to_experiment,
      pattern = regex("*raw.txt|*raw.csv"),
      full.names = TRUE,
      recursive = TRUE
    )
)

# make names for all plates
plate_names <- basename(files)

# read the files and name the data frames with info from file names
all_plates <- map(files, read_csv)
names(all_plates) <- plate_names

# get summary stats
(
  all_plates_summarised <-
    bind_rows(all_plates, .id = "file_name") %>%
    group_by(file_name) %>%
    gather(well, Local_read, A1:F8) %>%
    select(file_name, well, Local_read) %>%
    group_by(file_name, well) %>%
    summarise_at(
      "Local_read",
      funs(
        "Mean" = mean,
        "SD" = sd,
        "Median" = median,
        "Sum" = sum
      )
    ) %>%
    # parse the info from file name to get plate name, date and transfer
    separate(
      file_name,
      into = c("plate_name", "transfer", "year", "month", "day"),
      sep = "-",
      extra = "drop"
    ) %>%
    ungroup %>%
    mutate(date = ymd(paste(
      year, month, day, sep = "-"
    ))) %>%
    select(-year,-month,-day) %>%
    mutate(day = as.numeric(date - min(date))) %>%
    mutate(replicate = str_extract(plate_name, regex("\\d{1}")))
)

# join with meta data table
dd <-
  left_join(all_plates_meta,
            all_plates_summarised,
            by = c("plate_name", "well"))

##############################################################
# remove the rows that match strain D.1.31a or K.3.5c and are from transfer 1
# after updating the design, these rows now correspond to D.1.34a and K.3.3d
# these were the strains we replaced
# see metadata/design.r
##############################################################
dd %>% nrow
dd <- dd %>% filter(!(strain %in% c("D.1.34a", "K.3.3d") & transfer == "t1"))
dd %>% nrow

write_csv(dd, path = paste(path_to_experiment, "/", "all_data", ".csv", sep = ""))
save(dd, file = paste(path_to_experiment, "/", "all_data", ".Rsave", sep = ""))
