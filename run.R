
###################### TODO ##############################################
# https://github.com/gorkang/jsPsychHelpeR-Neely/issues/1

# WARNING IN ANALYSIS: SHOULD BE psych::alpha(sass, check.keys=TRUE)[1] %>% kable()

##########################################################################


# Initial setup -----------------------------------------------------------

  # Run the fist time or when you have an error
  source("setup/setup.R")


# Run project -------------------------------------------------------------  
  
  # Run only if you want to re-do the data preparation and analysis
  # targets::tar_destroy() # Destroy cache (_targets folder)

  # Run project
  targets::tar_make() # Run project
  

# Others ------------------------------------------------------------------

  # Visualize dependency tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")
  
  # See objects created
  targets::tar_objects()
  
  # Load objects
  targets::tar_load("DF_analysis")
  DF_analysis
  