## The purpose of this document: contains functions to clean my penguins raw dataset

#-----------------------------------------------------------------------------

#Clean column names, remove empty rows, remove columns called comment and delta
cleaning <- function(data_raw){
  data_raw %>%
    dplyr:: select(-c(Comments, Delta.15.N..o.oo., Delta.13.C..o.oo.)) %>%
    janitor:: clean_names() %>%
    janitor:: remove_empty(c("rows", "cols"))
}

#-----------------------------------------------------------------------------

#Remove observations with missing data.
remove_na <- function(data_clean){
  data_clean %>%
    dplyr:: filter(!is.na(flipper_length_mm)) %>%
    dplyr:: filter(!is.na(body_mass_g)) %>%
    dplyr:: filter(!is.na(sex))
}

#-----------------------------------------------------------------------------