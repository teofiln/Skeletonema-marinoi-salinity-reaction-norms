# set up the 8x6 experiment

library(tidyverse)
library(lubridate)

# these are the samples
# chosen based on how they grew in the first two transfers of the test-pop experiment
# except for zone D, where we have too few cultures to choose from, so all decent ones are included here.

# these are loaded into a 48 well plate, and inoculations are done on Mari
# for the first inoculum, 100 uL of cultere added to 900 of media

#######################################
# The design was updated for the first transfer because some cultures did not grow at any salinity
# The new strains are: K.3.3d (for K.5.3c) and D1.34a (for D.1.31a)
# Also several cultures that were healthy in the long-term cultures but did not grow in the experiment so far,
# were replaced with fresh material, or were re-inoculated from another replicate that was growing.
#
# The design table below reflect these changes.
# Data for the strains that were replaced was removed from the experiment
#
#######################################

AA <- paste("A.", c("2.21b", "2.27b", "3.10c", "3.24c", "3.27a", "3.29a", "3.31b", "3.43b"), sep="")
BB <- paste("B.", c("2.3b",  "2.14a", "2.17a", "2.19b", "2.23a", "2.32a", "2.34c", "2.44a"), sep="")
DD <- paste("D.", c("1.15a", "1.27a", "1.29a", "1.34a", "1.32a", "1.46b", "2.14b", "2.22a"), sep="")
FF <- paste("F.", c("1.2a",   "1.5b", "1.15c", "1.21a", "1.26b", "1.30a", "1.33a", "1.37a"), sep="")
II <- paste("I.", c("3.11a",  "3.13b", "3.17b", "3.22b", "3.28a", "3.44b", "5.3a", "5.19b"), sep="")
JJ <- paste("J.", c("2.5b",   "3.1a",  "3.4a", "3.20a", "3.28b", "3.32a", "3.38a", "3.42b"), sep="")
KK <- paste("K.", c("1.3a",   "1.5a",  "1.7b", "1.32b", "3.3a",  "3.4b",  "3.3d",  "3.16a"), sep="")
PP <- paste("P.", c("1.1", "1.4", "1.9", "1.29", "1.41", "1.49", "1.61", "2.6a"), sep="")

# S. subsalsu subsalsum design

# we've got one plate with eight strains in the columns and six treatments in the rows
# same as the 8x6 S. marinoi experiment
# as of now this is not replicated, and we're just looking to see if they'll grow at the same treatments
# we can split them into three replicates once established

# the S. subsalsum cultures:
# these are added manually to the all_meta_data.csv (created at the bottom of this script)
# because the zone information within this plate changes, whereas the rest of the design is one zone--one plate
SS <- c("I.2.16", "I.2.17", "I.2.22", "I.2.27", "J.LO-03-06", "J.LO-03-20", "J.2.13a", "J.2.14a")
  
treatments <- c(8, 12, 16, 20, 24, 28)
replicates <- 1:3

# so a plate will have strains from a zone in columns and treatments in rows
# each replicate will be in a separate plate

#like so
plate1 <- data.frame(treatments, treatments, treatments, treatments, treatments, treatments, treatments, treatments)
colnames(plate1) <- BB
plate1

# these are inoculated by hand, but will later be transfered with mari.

# to work with the app, the data files need to have the following name format:

# expriment-platenumber-transfernumber-date.csv (or txt)
# example: skmar-p1-t1-2018-05-05.csv

# for this experiment, the files from the plate reader are saved as
# zoneLETTER-transferNUMBER-date.csv (or txt)

# so we need a script that will summarise individual position measurements within a well,
# combine (by plate) and save with the format for the app.
# borrow from growth-report.R


# to follow the conventions above, we choose the order for the plates
# to be the same as the order in which we measure them
# note that its alphabetical except for d which is measured last,
# this is because we don't expect d to last long in the experiment

experiment <- "8x6"
zone <- rep(c("a", "b", "f", "i", "j", "k", "p", "d"), each=3)
replicate <- rep(1:3, 8)
plate_name <- paste(zone, replicate, sep="")
plate_number <- 1:24
experiment_meta <- tibble(experiment, zone, replicate, plate_name, plate_number)
write_csv(experiment_meta, path="EXPERIMENTS/8x6/experiment_meta.csv")

# now we can make tables for the plate and well for each combination of strain and treatment (for each plate)
# and join this to the experiment meta

make_plate <-
  function(plate_name,
           plate_number,
           treatments,
           strains) {
    wells <- expand.grid(Row = LETTERS[1:6], Column = 1:8) %>% mutate(Well = paste0(Row, Column))
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
        replicate=str_extract(plate_name, regex("\\d{1}"))
      )
    return(filled_plate)
  }
# make_plate(plate_name = "a1", plate_number = 1, treatments = treatments, strains = AA)

strain_list <- list(AA, AA, AA, 
                    BB, BB, BB, 
                    FF, FF, FF, 
                    II, II, II, 
                    JJ, JJ, JJ, 
                    KK, KK, KK,
                    PP, PP, PP,
                    DD, DD, DD)
cycle_over <- list(nm=as.list(experiment_meta$plate_name), 
                   nu=as.list(experiment_meta$plate_number),
                   st=strain_list)

all_plates_list <- map(1:24, function(a)
  make_plate(
    plate_name = cycle_over$nm[[a]],
    plate_number = cycle_over$nu[[a]],
    treatments = treatments,
    strains = cycle_over$st[[a]]
  ))

all_plates_df <-
  all_plates_list %>% 
  bind_rows() %>% 
  mutate(experiment="8x6") %>% 
  mutate(zone=str_extract(strain, regex("^[:alpha:]{1}"))) %>% 
  select(experiment, plate_number, plate_name, zone, strain, treatment, row:well) 

# don't save so as to not overwrite all_plates_meta.csv
# this was manually updated with meta data for the Skeletonema subsalsum strains
# write_csv(all_plates_df, path="EXPERIMENTS/8x6/all_plates_meta.csv")
# save(all_plates_df, file="EXPERIMENTS/8x6/all_plates_meta.Rsave")

# read the meta csv again and re-save for the app

apm <- readr::read_csv("8x6/all_plates_meta.csv")
save(apm, file="8x6/all_plates_meta.Rsave")




