# SENSITIVE DATA

run_sensitive_data <- function(df_SDG, run_online_FORM = FALSE) {

  # [TODO]: Should get df_SDG as input here (as it is integrated in _targets.R)
  if (!exists("run_online_FORM")) run_online_FORM = TRUE

# Parameters --------------------------------------------------------------

  options(pillar.sigfig = 5)


# Set options, load packages -----------------------------------------------
  
  # Source all /R files
  lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
  lapply(list.files(".vault/R", full.names = TRUE, pattern = ".R"), source)
  source("_packages.R")
  

  # Online Google Form: ONLY RUN IF NECESSARY (API LIMITS)
  # Activate API: https://console.cloud.google.com/apis/credentials
  # See .vault/config/gs4.R
  if (run_online_FORM == TRUE) {
    cat(crayon::yellow("Fetching online FORM Google sheet...\n"))
    df_FORM = prepare_FORM(df_SDG, short_name_scale_str = "FORM")  
  } else {
    cat(crayon::yellow("Reading offline FORM Google sheet...\n"))
    df_FORM = read_rds(".vault/data/df_FORM_RAW.rds")
  }
  
  df_AIM = prepare_AIM_gsheet(df_FORM, short_name_scale_str = "AIM")
  

  # _Tests -------------------------------------------------------------------
  
  # [REMEMBER]: Have to manually put every target we have a test for here (except the automatic tests: 'input_files_automatic_tests_str' takes care of that)
  # tar_target(input_files_automatic_tests_str, list.files(path = "_targets/objects/", pattern="df_*", full.names = FALSE, ignore.case = FALSE)),

  
  # Report ------------------------------------------------------------------
  cat(crayon::yellow("Preparando report_candidatos...\n"))
  rmarkdown::render(".vault/doc/report_candidatos.Rmd", "html_document", quiet = TRUE, clean = TRUE, envir = new.env())
  
 return(df_AIM)
  
}