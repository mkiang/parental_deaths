## 00_combined_sim_files.R ----
## 
## This file will download all the parts of our simulation files from Harvard
## Dataverse and reassemble them into the large simulation files used for our
## paper. If you prefer to download the files manually, visit the Dataverse
## page at: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FZJM4ZX
## but note that the reassembly code expects the data to be in specific 
## locations. See ./outputs/data_public/a_x_t_metainfo.csv for more. 

## Imports ----
library(tidyverse)
library(here)
library(fs)
source(here("codes", "utils.R"))

## CONSTANTS ----
## Delete the temporary files?
DELETE_TEMP_FILES <- TRUE

## Data ----
metainfo <- read_csv(here("outputs", "data_public", "a_x_t_metainfo.csv"))

## Download files ----
for (i in 1:NROW(metainfo)) {
    f_path <- sprintf("%s/%s", metainfo$dirname[i], metainfo$filename[i])
    
    if (!file_exists(f_path)) {
        ## Up the timer since these are fairly large files (~500MB)
        options(timeout = 60*15)
        message("Downloading ", basename(f_path))
        dir_create(dirname(f_path))
        download.file(metainfo$download_url[i], f_path)
        Sys.sleep(5)
    } else {
        message("Skipping  ", basename(f_path))
    }
}

## Reassemble the a_x_t files from the 10 parts in each directory ----
sim_dirs <- unique(metainfo$dirname)

for (i in 1:length(sim_dirs)) {
    new_path <- sprintf("%s/%s.RDS", dirname(sim_dirs[i]),
                        paste0(basename(sim_dirs[i])))
    
    if (!fs::file_exists(new_path)) {
        sim_files <- dir_ls(sim_dirs[i], type = "file", glob = "*.RDS")
        for (j in 1:10) {
            if (j == 1) {
                temp_x <- readRDS(sim_files[j])
            } else {
                temp_x <- abind::abind(temp_x, readRDS(sim_files[j]))
            }
        }
        
        ### Save it ----
        saveRDS_xz(temp_x, new_path)
        
        ### Delete the temporary files ----
        if (DELETE_TEMP_FILES) {
            fs::dir_delete(sim_dirs[i])
        }
    }
}
