## 99_break_up_sims.R ----
## 
## NOTE: This file is completely commented out because you should never really
## need to run this. We run this because of storage size limits on Harvard
## Dataverse and OSF prevent uploading the original files, which are >5GB 
## each and result in >50GB for the whole project. To stay under the limits,
## we break up each file into 10 smaller chunks and reconstitute the original
## files in the 00_combine_sim_files.R script.
## 
## We just provide this code for transparency and completeness. If you do
## need to run it, uncomment the script below. 

# library(here)
# library(fs)
# source(here("codes", "utils.R"))
# 
# ## Get list of simulation files ----
# sim_files <- dir_ls(here("outputs", "data_public"), 
#                     type = "file", 
#                     regexp = "a_x_t_scenario")
# 
# ## Go through each file, split it up, save the splits ----
# for (p in sim_files) {
#     # Load the file
#     temp_x <- readRDS(p)
#     
#     # Split it up into 10 pieces and save each piece
#     for (i in 1:10) {
#         base_name <- substr(basename(p), 1, nchar(basename(p)) - 4)
#         new_path <- here(dirname(p), 
#                          base_name,
#                          paste0(base_name, "_", i, ".RDS"))
#         
#         if (!fs::file_exists(new_path)) {
#             fs::dir_create(dirname(new_path))
#             
#             start_ix <- (i - 1) * 100 + 1
#             end_ix <- i * 100
#             
#             sub_temp <- temp_x[ , , , , , , start_ix:end_ix]
#             
#             saveRDS_xz(sub_temp, new_path, threads = 12)
#             rm(sub_temp)
#             gc()
#         }
#     }
#     
#     rm(temp_x)
#     gc()
# }
