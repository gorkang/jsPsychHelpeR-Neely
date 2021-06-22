testthat::test_that('Check if the snapshots do not change', {
  # https://testthat.r-lib.org/articles/snapshotting.html
  
  # DEBUG

    # CREATE SNAPSHOTS TEST LINES
      # FILES_TO_SNAPSHOT = list.files(path = "_targets/objects", pattern = "df_*", full.names = TRUE, ignore.case = FALSE)
      # FILES_TO_SNAPSHOT %>% walk(~ cat(paste0('testthat::expect_snapshot_file(here::here("', .x, '"))\n')))
    
    # IF THERE IN AN ERROR (eg. in df_AIM):
      # TARGET = read_rds("_targets/objects/df_AIM")
      # SNAPSHOT = read_rds("tests/testthat/_snaps/snapshots/df_AIM")
      # waldo::compare(SNAPSHOT,TARGET)
      # If NEW snapshot is OK: # testthat::snapshot_accept()
  
  
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "snapshots"
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))

  
  # Actual expectation -------------------------------------------------------------

  Sys.setenv(NOT_CRAN = "true")
  
  # testthat edition
  testthat::local_edition(3)

  testthat::expect_snapshot_file(here::here("_targets/objects/df_AIM"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_COVIDCONTROL"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_DEMOGR3"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_EAR"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_OTRASRELIG"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_PSS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SASS"))
  testthat::expect_snapshot_file(here::here("_targets/objects/df_SCSORF"))

})

