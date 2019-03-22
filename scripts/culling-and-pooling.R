# weed out the bad growers and combine strains from 2 zones into 1 plate
# groups will be by replicate, so 4 strains from zone A rep 1 go with zone B rep 1
# combinations: A+B, F+I, J+K, P+D
# for subsalsum (S) take the best 4 and split into 2 reps within the same plate

#### INOCULATION
# Do the first inoculation BY HAND by transferring 800 of culture into 200 of media
# This should provide enough cover for 1-2 days of growth before a within-app transfer can be made
####

# helper functions
make_plate <-
  function(plate_name,
           plate_number,
           treatments,
           strains) {
    wells <-
      expand.grid(Row = LETTERS[1:6], Column = 1:8) %>% mutate(Well = paste0(Row, Column))
    trts  <- rep(treatments, 8)
    strs  <- rep(strains, each = 6)
    filled_plate <-
      tibble(
        plate_name,
        plate_number,
        row = wells$Row,
        column = wells$Column,
        well = wells$Well,
        treatment = trts,
        strain = strs,
        replicate = str_extract(plate_name, regex("\\d{1}"))
      )
    return(filled_plate)
  }

# format so that a new plate metadata can be appended to all_plates_meta directly
format_for_append <- . %>%
  mutate(experiment = "8x6") %>%
  mutate(zone = str_extract(strain, regex("^[:alpha:]{1}"))) %>%
  select(experiment,
         plate_number,
         plate_name,
         zone,
         strain,
         treatment,
         row:well)

# 2019-03-19
# combine F and I
F_keepers <- paste("F.", c("1.2a",  "1.5b", "1.30a", "1.37a"), sep="")
I_keepers <- paste("I.", c("3.11a", "3.22b", "3.28a", "5.3a"), sep="")
FI_keepers <- c(F_keepers, I_keepers)

FI1_plate <- make_plate(
  plate_name = "fi1",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = FI_keepers
)

FI1_plate <- FI1_plate %>% format_for_append

FI2_plate <- make_plate(
  plate_name = "fi2",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = FI_keepers
)

FI2_plate <- FI2_plate %>% format_for_append

FI3_plate <- make_plate(
  plate_name = "fi3",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = FI_keepers
)

FI3_plate <- FI3_plate %>% format_for_append

##### IMPORTANT ######
# When a combined plate is made, append its metadata to the main all_plates_meta.csv/Rsave file
# this will overwrite the main metadata file, so run only once per plate
# if the meta for a particular new plate has changed, remove the old plate and append the new
if (FALSE) {
  all_plates_meta <- read_csv("/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.csv")
  all_plates_meta <- bind_rows(all_plates_meta, FI1_plate, FI2_plate, FI3_plate)
  write_csv(all_plates_meta, path = "/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.csv")
  save(all_plates_meta, file = "/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.Rsave")
}

#####

# 2019-03-20
# combine J and K
J_keepers <- paste("J.", c("2.5b", "3.4a", "3.28b", "3.32a"), sep="")
K_keepers <- paste("K.", c("1.7b", "3.3a", "3.4b",  "3.16a"), sep="")
JK_keepers <- c(J_keepers, K_keepers)

JK1_plate <- make_plate(
  plate_name = "jk1",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = JK_keepers
)

JK1_plate <- JK1_plate %>% format_for_append

JK2_plate <- make_plate(
  plate_name = "jk2",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = JK_keepers
)

JK2_plate <- JK2_plate %>% format_for_append

JK3_plate <- make_plate(
  plate_name = "jk3",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = JK_keepers
)

JK3_plate <- JK3_plate %>% format_for_append

# See 'important' note above

if (FALSE) {
  all_plates_meta <- read_csv("/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.csv")
  all_plates_meta <- bind_rows(all_plates_meta, JK1_plate, JK2_plate, JK3_plate)
  write_csv(all_plates_meta, path = "/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.csv")
  save(all_plates_meta, file = "/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.Rsave")
}

#####


# 2019-03-22
# combine P and D
P_keepers <- paste("P.", c("2.5b", "3.4a", "3.28b", "3.32a"), sep="")
D_keepers <- paste("D.", c("1.7b", "3.3a", "3.4b",  "3.16a"), sep="")
PD_keepers <- c(P_keepers, D_keepers)

PD1_plate <- make_plate(
  plate_name = "pd1",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = PD_keepers
)

PD1_plate <- PD1_plate %>% format_for_append

PD2_plate <- make_plate(
  plate_name = "pd2",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = PD_keepers
)

PD2_plate <- PD2_plate %>% format_for_append

PD3_plate <- make_plate(
  plate_name = "pd3",
  plate_number = 30,
  treatments = c(8, 12, 16, 20, 24, 28),
  strains = PD_keepers
)

PD3_plate <- PD3_plate %>% format_for_append

# See 'important' note above

if (FALSE) {
  all_plates_meta <- read_csv("/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.csv")
  all_plates_meta <- bind_rows(all_plates_meta, PD1_plate, PD2_plate, PD3_plate)
  write_csv(all_plates_meta, path = "/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.csv")
  save(all_plates_meta, file = "/data/Dropbox/Skeletonema-marinoi-salinity-reaction-norms-data/metadata/all_plates_meta.Rsave")
}

#####
