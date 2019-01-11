#' Save Talent Supply validated files
#'
#' This function allows you to save validated Talent Supply files ready to be uploaded to data base.
#' @param country_name Country for which the saves are going to be saved
#' @param year Year for which the saves are going to be saved
#' @keywords save
#' @export
#' @examples
#' save_validated_files("South_Africa",2017)

supply_save<-function(country_name,year){

  ##Load necessary libraries

  library("data.table")

  setwd(paste0('/data/Stata/Talent_supply_update/SUPPLY_TRANSITION/',country_name))


  ##Read the validated files ready to be saved in the final folder

  country_validated<-read.csv(paste0('./Input_files/Supply_count_country_',year,'.csv'))

  state_validated<-read.csv(paste0('./Input_files/Supply_count_state_',year,'.csv'))

  msa_validated<-read.csv(paste0('./Input_files/Supply_count_msa_',year,'.csv'))


  ##Save all files in the correct format ready to load to data base

  output_dir<-(paste0('/data/Stata/Talent_supply_update/Final_files/',country_name))

  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {print("final directory already exists!")}

  setwd(output_dir)

  country_name_upper<-toupper(country_name)

  ##Country level

  fwrite(country_validated,paste0(country_name_upper,'_SUPPLY_',year,'_COUNTRY_LOCAL_CODES.csv'))

  ##State level

  fwrite(state_validated,paste0(country_name_upper,'_SUPPLY_',year,'_STATE_LOCAL_CODES.csv'))

  ##MSA level

  fwrite(msa_validated,paste0(country_name_upper,'_SUPPLY_',year,'_MSA_LOCAL_CODES.csv'))

  ### ONET save files part


  ##Set directory with validated ONET Talent Supply data

  setwd('..')

  setwd('..')

  setwd(paste0('./SUPPLY_TRANSITION/',country_name))

  ##Read the validated files ready to be saved in the final folder

  country_validated<-read.csv(paste0('./Output_files/country_supply_output_',year,'.csv'))

  state_validated<-read.csv(paste0('./Output_files/state_supply_output_',year,'.csv'))

  msa_validated<-read.csv(paste0('./Output_files/msa_supply_output_',year,'.csv'))

  ##Save all files in the correct format ready to load to data base

  output_dir<-(paste0('/data/Stata/Talent_supply_update/Final_files/',country_name))

  setwd(output_dir)

  country_name_upper<-toupper(country_name)

  ##Country level

  fwrite(country_validated,paste0(country_name_upper,'_SUPPLY_',year,'_COUNTRY_ONET.csv'))

  ##State level

  fwrite(state_validated,paste0(country_name_upper,'_SUPPLY_',year,'_STATE_ONET.csv'))

  ##MSA level

  fwrite(msa_validated,paste0(country_name_upper,'_SUPPLY_',year,'_MSA_ONET.csv'))

  print(paste0("Final files ready to load to data base are available in the Final_files/",country_name," file"))

}
