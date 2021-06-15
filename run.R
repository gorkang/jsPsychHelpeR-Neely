
# Install packages ---------------------------------------------------------

# Run the fist time or when you have an error
  
  # targets::tar_renv()
  # source("setup/setup.R")


# Sync data from server ---------------------------------------------------
  
  # EDIT ONLY THE id_protocol variable value
  id_protocol = 3
  
  # If you do not have the .credentials file: rstudioapi::navigateToFile("setup/setup_server_credentials.R")
  list_credentials = source(".vault/.credentials")
  if (!dir.exists(paste0(getwd(), '/data/' , id_protocol, '/'))) dir.create(paste0(getwd(), '/data/' , id_protocol, '/'))
  system(paste0('sshpass -p ', list_credentials$value$password, ' rsync -av --rsh=ssh ', list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, id_protocol, '/.data/ ', getwd(), '/data/' , id_protocol, '/'))

  # RE-synched files (???)
  # 20210615: 10.093 files
  # 3_Goodbye_original_2021-03-12T17:26:51_962251989.csv
  # 20210615: 10.094 files
  # 3_Goodbye_original_2021-03-12T16:54:14_965155738.csv
  

# CHECK and DELETE duplicates ---------------------------------------------

  source(here::here("R/delete_duplicates.R"))
  
  # Check duplicates and their differences
  CHECK = delete_duplicates(folder = "data/3", check = TRUE)
  
  # **DELETE** NEWER DUPLICATES
  CHECK = delete_duplicates(folder = "data/3", check = FALSE)

    
# Run project -------------------------------------------------------------  
  
  targets::tar_destroy() # Destroy cache (_targets folder)
  targets::tar_make() # Run project
  
  
# Visualize targets networks -----------------------------------------------

  # Dependency tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")
  
  # Meta
  targets::tar_meta() %>% summarise(sum(seconds, na.rm = TRUE))
  targets::tar_meta() %>% select(name, seconds) %>% arrange(desc(seconds))
  targets::tar_meta(fields = warnings) %>% View
