create_DF_matched <- function(DF_clean, DF_joined) {
  
  # DEBUG
  # debug_function("create_DF_matched")
  

  # NEW DF -----------------------------------------------------------------

    DF_joined_clean = 
      DF_joined %>%
      #drop_na(COVIDCONTROL_01_DIR) %>% # Did not complete full protocol
      drop_na(AIM_DIRt, EAR_DIRt, PSS_DIRt, SASS_DIRt, SCSORF_DIRt) %>% # pido que solo tengan completos las variables de autoestima, sass y religiosidad, y demográficos (edad, sexo y lo que permite calcular el gse).
      select(id, matches("DEMOGR3"), AIM_DIRt) %>% 
      filter(
        DEMOGR3_01_DIR >= 18 & # Indica tu edad 
          DEMOGR3_03_DIR == 1 & # ¿Usted nació en Chile?
          DEMOGR3_04_DIR == 1 & # ¿Usted vive hace más de 10 años en Chile?
          #DEMOGR3_06_DIR == 0 & # Durante los últimos 12 meses ¿Ha sido diagnosticado/a y/o tratado/a por algún problema Psicológico o Psiquiátrico?
          DEMOGR3_07_DIR == 0 & # ¿Usted tiene actualmente alguno de los siguientes diagnósticos: Esquizofrenia, Depresión Mayor, Epilepsia grave, Traumatismo en…
          DEMOGR3_08_DIR == 0 & # ¿Tiene algún problema visual o auditivo que le impida responder esta encuesta sin ayuda?
          AIM_DIRt %in% c("ab", "c1a", "c1b", "c2") # Middle and high income
      )
    
    # Use age and sex. Join datetime from DF_clean
    DF_matching_new = 
      DF_joined_clean %>% 
      select(id, DEMOGR3_01_DIR, DEMOGR3_02_DIR) %>% 
      drop_na() %>% 
      left_join(DF_clean %>% distinct(id, .keep_all = TRUE) %>%  select(id, datetime), by = "id") %>% 
      arrange(DEMOGR3_01_DIR, DEMOGR3_02_DIR, datetime)
    
    DF_matching_new_COUNT = 
      DF_matching_new %>% 
      count(DEMOGR3_01_DIR, DEMOGR3_02_DIR, name = "count_new")
    
    
  # OLD DF ------------------------------------------------------------------
  
  # PQ hay id's repetidos???? 
  
    # Son 243 participantes de contextos vulnerables, seleccionando solo chilenos/as con al menos 10 años en chile, y al menos 18 años de edad.
    DF_old = 
      read_csv(here::here("data/2017_experimental_group.csv"), 
               col_types = 
                 cols(
                   .default = col_character())) %>% 
      filter(
        grepl("chile", ISD04, ignore.case = TRUE) &
          ISD08 >= 18 &
          as.numeric(stringr::str_extract(ISD05, "[0-9]{1,3}")) >= 10
        ) %>% 
      drop_na(ISD07, ISD08, Religiosidad, Autoestima, SASS_Total) %>% 
      select(id, ISD07, ISD08) %>% 
      rename(id_old = id)
      
    
    DF_matching_old = 
      DF_old %>% 
      # select(id_old, ISD07, ISD08) %>% 
      mutate(DEMOGR3_01_DIR = as.numeric(ISD08),
             DEMOGR3_02_DIR = 
               case_when(
                 ISD07 == "Masculino" ~ 1,
                 ISD07 == "Femenino" ~ 0,
                 TRUE ~ NA_real_)) %>% 
      select(id_old, DEMOGR3_01_DIR, DEMOGR3_02_DIR) %>% 
      drop_na() 
    
    DF_matching_old_COUNT = 
      DF_matching_old %>% 
      count(DEMOGR3_01_DIR, DEMOGR3_02_DIR, name = "count_old")
    
  
  
  # Match -------------------------------------------------------------------
  
  sample_match <- function(DF = DF_matching_new, age, gender, N) {

    # CHECK If no candidates available
    if (DF %>% filter(DEMOGR3_01_DIR == age & DEMOGR3_02_DIR == gender) %>% nrow() == 0){ 
      
      cat(crayon::red("NO candidates available for age = ", age, " and gender = ", gender, "\n"))
      DF_NEW = tibble(id = NA_character_, ROW = 1)
      
    } else {
    
      # Select N rows ordered by datetime of the participants matching age and sex
      DF_NEW = 
        DF %>% 
        filter(DEMOGR3_01_DIR == age & DEMOGR3_02_DIR == gender) %>% 
        arrange(DEMOGR3_01_DIR, DEMOGR3_02_DIR, datetime) %>% 
        slice_head(n = N) %>% 
        select(id) %>% 
        mutate(ROW = 1:n())
    }
    
      # Filter DF_matching_old to only have the relevant age and sex
      DF_OLD = 
        DF_matching_old %>% 
        filter(DEMOGR3_01_DIR == age & DEMOGR3_02_DIR == gender) %>% 
        mutate(ROW = 1:n())
      
      # Join the selected DF_NEW participants
      DF_OLD %>% 
        left_join(DF_NEW, by = "ROW")
  }
  
  DF_new_matched = 
    1:nrow(DF_matching_old_COUNT) %>% 
    map_df(~ sample_match(age = DF_matching_old_COUNT$DEMOGR3_01_DIR[.x], gender = DF_matching_old_COUNT$DEMOGR3_02_DIR[.x], N = DF_matching_old_COUNT$count_old[.x]))

  
  # Save files --------------------------------------------------------------
  save_files(DF_new_matched, short_name_scale = "matched", is_scale = FALSE)
  
  
  # Output of function ---------------------------------------------------------
  
  list_matching =
    list(DF_new_matched = DF_new_matched,
         DF_matching_new_COUNT = DF_matching_new_COUNT,
         DF_matching_old_COUNT = DF_matching_old_COUNT)
  
  return(list_matching)
  
}