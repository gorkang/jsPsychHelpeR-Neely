
###################### TODO ##############################################
# https://github.com/gorkang/jsPsychHelpeR-Neely/issues/1

# WARNING IN ANALYSIS: SHOULD BE psych::alpha(sass, check.keys=TRUE)[1] %>% kable()

##########################################################################


# Initial setup -----------------------------------------------------------

  # Run the fist time or when you have an error
  # targets::tar_renv()
  # source("setup/setup.R")


# Load libraries ---------------------------------------------------------

  lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source)


# Sync data from server ---------------------------------------------------
  
  update_data(id_protocol = 3, sensitive_tasks = "DEMOGR")


# CHECK and DELETE duplicates ---------------------------------------------

  # Check duplicates and their differences
  CHECK = delete_duplicates(folder = "data/3", check = TRUE, keep_which = "newer")
  CHECK_sensitive = delete_duplicates(folder = ".vault/data", check = TRUE, keep_which = "newer")
  
  # **DELETE** NEWER DUPLICATES
  CHECK = delete_duplicates(folder = "data/3", check = FALSE, keep_which = "newer")
  CHECK_sensitive = delete_duplicates(folder = ".vault/data", check = FALSE, keep_which = "newer")
    
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
