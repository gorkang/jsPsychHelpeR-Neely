# Initial setup -----------------------------------------------------------

  # Run the fist time or when you have an error
  source("setup/setup.R")


# Run project -------------------------------------------------------------  
  
  # Run only if you want to re-do the data preparation and analysis
  # targets::tar_destroy() # Destroy cache (_targets folder)

  # Invalidate a specific target
  # targets::tar_invalidate("TESTS")
  
# Run project
  targets::tar_make() # Run project
  

# Others ------------------------------------------------------------------

  # Visualize dependency tree
  targets::tar_visnetwork(targets_only = TRUE, label = "time")
  
  # See objects created
  targets::tar_objects()
  
  # Load specific object
  targets::tar_load("DF_analysis")
  DF_analysis
  