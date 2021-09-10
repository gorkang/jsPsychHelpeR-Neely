# jsPsychHelpeR with {targets}

# Parameters --------------------------------------------------------------

  pid_target = 3


# Libraries ---------------------------------------------------------------

  library(targets) 
  library(tarchetypes) 


# Set options, load packages -----------------------------------------------
  
  # Source all /R files
  lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
  lapply(list.files("./R_tasks/", full.names = TRUE, pattern = ".R"), source)
  options(pillar.sigfig = 5)
  
  # Packages to load
  main_packages = c("cli", "crayon", "furrr", "patchwork", "renv", "tarchetypes", "targets", "testthat")
  data_preparation_packages = c("clock", "dplyr", "forcats", "here", "janitor", "purrr", "readr", "stringr", "tibble", "tidyr") 
  data_analysis_packages = c("apaTables", "broom", "broom.mixed", "corrplot", "corrr", "corx", "emmeans", "gmodels", "gt", "gtsummary", "irr", "knitr", "lavaan", "lme4", "parameters", "performance", "psych", "sjPlot", "stargazer")
  data_visualization_packages = c("ggalluvial", "ggraph", "ggridges", "igraph")
  non_declared_dependencies = c("qs", "visNetwork", "webshot", "performance", "bs4Dash", "shinybusy", "clustermq", "shinyWidgets")
  extra_packages = c("shrtcts")
  packages_to_load = c(main_packages, data_preparation_packages, data_analysis_packages, data_visualization_packages, non_declared_dependencies, extra_packages)
  
  
  # target options (packages, errors...)
  tar_option_set(packages = packages_to_load, # Load packages for all targets
                 error = "workspace") # Needed to load workspace on error to debug
  


# Define targets -------------------------------------------------------------
  
targets <- list(
  
  ## Read files --------------------------------------------------------------
  
  # RAW data
  tar_target(input_files, list.files(path = paste0("data/", pid_target), pattern = "*.csv", full.names = TRUE), format = "file"), #, format = "file" (IF files in vault/ first run fails)
  
  tar_target(DF_raw, read_data(input_files, anonymize = FALSE)),
  tar_target(tests_DFraw, tests_DF_raw(DF_raw), priority = 1),
  
  
  # Cleaned data
  tar_target(DF_clean, create_clean_data(DF_raw)),
  
  # Diccionary of tasks
  tar_target(DICCIONARY_tasks, create_diccionary_tasks(DF_clean), priority = 1),

  
  ## Prepare tasks -----------------------------------------------------------
  
  # SENSITIVE TASK
  tar_target(df_DEMOGR3, prepare_DEMOGRsensitive(short_name_scale_str = "DEMOGR3")),
  
  # Other TASKS
  tar_target(df_AIM, prepare_AIM(DF_clean, short_name_scale_str = "AIM")),
  tar_target(df_COVIDCONTROL, prepare_COVIDCONTROL(DF_clean, short_name_scale_str = "COVIDCONTROL")),
  tar_target(df_EAR, prepare_EAR(DF_clean, short_name_scale_str = "EAR")),
  tar_target(df_OTRASRELIG, prepare_OTRASRELIG(DF_clean, short_name_scale_str = "OTRASRELIG")),
  tar_target(df_PSS, prepare_PSS(DF_clean, short_name_scale_str = "PSS")),
  tar_target(df_SASS, prepare_SASS(DF_clean, short_name_scale_str = "SASS")),
  tar_target(df_SCSORF, prepare_SCSORF(DF_clean, short_name_scale_str = "SCSORF")),
  
  
  ## Join tasks --------------------------------------------------------------
  
  tar_target(DF_joined, create_joined(
    df_AIM,
    df_COVIDCONTROL,
    df_DEMOGR3,
    df_EAR,
    df_OTRASRELIG,
    df_PSS,
    df_SASS,
    df_SCSORF
  )),
  

  
  ## Analysis ----------------------------------------------------------------- 
  
  # Prepare a DF ready for the analysis
  tar_target(DF_analysis, create_DF_analysis(DF_joined)),
  
  # [TODO] Descriptive Table 1
  # Important: Should we compare DF_analysis with the final data used in each model? 
  tar_render(descriptives_table1, "doc/descriptives_table1.Rmd", deployment = "main",
             output_file = paste0("../outputs/reports/descriptives_table1.html")),

  
  # Plots
  tar_target(plots_descriptive, analysis_descriptive_plots(DF_joined)),
  
  

  # MATCHED DF --------------------------------------------------------------
  
  # Prepare a DF ready for the analysis
  tar_target(DF_matched, create_DF_matched(DF_clean, DF_joined)),

  
  # Analysis Neely
  tar_render(report_Analysis, "doc/Analysis_V5.Rmd", 
             deployment = "main", 
             params = list(depends = list(DF_joined, DF_matched)), # We read the outputs of these targets for the Analysis
             output_file = paste0("../outputs/reports/Analysis_V5.html")),
  
  
  ## Tests -------------------------------------------------------------------
  
  # [REMEMBER]: Have to manually put every target we have a test for here
  tar_target(TESTS, test_testhat(input_files,
                                 DF_raw,
                                 DF_clean,
                                 DICCIONARY_tasks,
                                 DF_joined
                                 )
  ),
  
  # Report ------------------------------------------------------------------

   # Automatic report
  tar_render(report_DF_clean, "doc/report_DF_clean.Rmd", 
             deployment = "main", 
             params = list(last_task = "Goodbye",
                           pid_report = pid_target),
             output_file = paste0("../outputs/reports/report_DF_clean.html")),
  
  # Matches report
  tar_render(report_MATCH, "doc/report_MATCH.Rmd", deployment = "main",
             output_file = paste0("../outputs/reports/report_MATCH.html")),
  
  # Progress report
  tar_render(report_PROGRESS, path = "doc/report_PROGRESS.Rmd", 
             params = list(input_files_vector = input_files, 
                           pid_report = pid_target, 
                           last_task = "COVIDCONTROL", 
                           goal = 800),
             output_file = paste0("../outputs/reports/report_PROGRESS_", pid_target , ".html"))
)


# Declare pipeline --------------------------------------------------------

  targets
