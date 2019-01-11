#' Talent Supply Validation Function
#'
#' This function allows you to validate the talent supply counts by Local and Onet taxonomies.This files must be saved in the data science Talent Supply
#' directory in share drive. The files with local occupation taxonomy supply counts must be saved in the Input_files folder.
#' The files with Onet occupation taxonomy supply counts must be saved in the Output_files folder.
#' A report with local occupation taxonomy is created for each available year in order to facilitate to the Quantitative Analyst the visual analysis
#' of the data sets if necessary.
#' The validation script executes the following validations, in the order specified below:
#' Local codes files validation
#' \enumerate{
#' \item Verify that the number of available local codes in the file at Country level is equal to the number of codes available in the State level file
#' \item Verify that the sum of total supply crossed local codes at State level is equal to Country level
#' \item Verify that the sum of supply of each occupation at Country level is less or equal than its State level counterpart. If test #3 fails, a file with all codes for which this test does not respect the constraint is saved in the error folder
#' \item Verify that the sum by occupation at MSA level is equal or less than its State counterpart. If test #4 fails, a file with all codes for which this test does not respect the constraint is saved in the error folder
#' }
#' Onet codes files validation
#' \enumerate{
#' \item Verify that the number of available Onet codes in the file at Country level is equal to the number of Onet codes available in the State level file
#' \item Verify that the sum of total supply crossed Onet codes at State level is equal to Country level
#' \item Verify that the sum of supply by occupation at Country level is equal than its State level counterpart. If test #3 fails, a file with all codes for which this test does not respect the constraint is saved in the error folder
#' \item Verify that the sum by Onet occupation at MSA level is equal or less than its State counterpart.If test #4 fails, a file with all codes for which this test does not respect the constraint is saved in the error folder
#' }
#' @param year Year for which we are validating the data
#' @param country_name Country for which we are validating the data.E.g, "South_Africa"
#' @param user Data Base user name E.g, "user"
#' @param password Data base password E.g, "password"
#' @param occupation_type_id Country Occupation type id as in data base
#' @param level_occupation Level of occupation that we are validating, this number correspond to the column level as in table norm_prod.occupations
#' @param Are_Onet_files_available "yes" if the files with Onet supply are available and ready to be validated
#' @param host Indicate Hostname server
#' @keywords supply
#' @export
#' @examples
#' supply_validation(2017,"South_Africa","user","password",20,4,"yes","10.0.5.30")


supply_validation<-function(year,country_name,user,password,occupation_type_id,level_occupation,Are_Onet_files_available,host){
  
##Load neccessary libraries

library(RMySQL)

library(dplyr)

library(data.table)

library(openxlsx)

if (Are_Onet_files_available=="no"){
  
  ##Set the directory where the final files are available
  
  setwd(paste0('/data/Stata/Talent_supply_update/SUPPLY_TRANSITION/',country_name))
  
  
  ## Validate files using local codes structure
  
  
  ##Read the files at Country, State and MSA level
  
  country<-read.csv(paste0('./Input_files/Supply_count_country_',year,'.csv'))
  
  state<-read.csv(paste0('./Input_files/Supply_count_state_',year,'.csv'))
  
  msa<-read.csv(paste0('./Input_files/Supply_count_msa_',year,'.csv'))
  
  country_id<-country[2,2]
  
  ### Add msa ids, msa names, state id and names to MSA data frame
  
  mydb = dbConnect(MySQL(), user=user, password=password, host=host)
  
  msa_query = paste0("select msa_id,msa_name,state_id,state_name from norm_prod.place_details_2016
                     where place_type_id=3 and country_id=",country_id)
  
  msa_taxonomy = dbSendQuery(mydb, msa_query)
  
  msa_taxonomy = fetch(msa_taxonomy, n=-1)
  
  msa<-merge(msa,msa_taxonomy,by.x='msa_id',by.y='msa_id',all.x=TRUE)
  
  ##Create repport for verification at state level
  
  state_verification<-state %>%
    group_by(state_id,occupation_code) %>%
    summarise(tot_emp_state=sum(tot_emp))
  
  state_taxonomy_query = paste0("select state_id,state_name from norm_prod.place_details_2016
                                where country_id=",country_id," and place_type_id=5;")
  
  state_taxonomy=dbSendQuery(mydb,state_taxonomy_query)
  
  state_taxonomy= fetch(state_taxonomy, n=-1)
  
  ##Add Occupation name from db taxonomy
  
  occupation_taxonomy= paste0("select code,name from norm_prod.occupations
                              where occupation_type_id=",occupation_type_id," and level=",level_occupation,";")
  
  occupation_taxonomy_query<-dbSendQuery(mydb,occupation_taxonomy)
  
  occupation_taxonomy=fetch(occupation_taxonomy_query, n= -1)
  
  
  ###Merge states and occupation taxonomy with summary of state
  
  state_count_file<-merge(state_verification,state_taxonomy,by.x = "state_id",by.y = "state_id",all.x = TRUE)
  
  state_count_file<-merge(state_count_file,occupation_taxonomy,by.x = "occupation_code",by.y = "code",all.x = TRUE)
  
  ##Make sure that we save the data in data frame
  
  state_count_file<-as.data.frame(state_count_file)
  
  ###Create Summary of total supply by State
  
  state_summary<-state_count_file %>%
    group_by(state_id,state_name) %>%
    summarise(tot_supply_state=sum(tot_emp_state))
  
  ##Make sure that we save the data in data frame
  
  state_summary<-as.data.frame(state_summary)
  
  ###Save report file State
  
  output_dir<-('./report_files')
  
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  workbook_state<-createWorkbook()
  addWorksheet(workbook_state,"Count_state_occupations")
  addWorksheet(workbook_state,"Summary_state")
  
  writeData(workbook_state,1,state_count_file,rowNames = FALSE)
  writeData(workbook_state,2,state_summary,rowNames = FALSE)
  
  saveWorkbook(workbook_state, paste0(output_dir,"/",country_name,"_state_report_",year,".xlsx"), overwrite = TRUE)
  
  
  ##Create repport for verification at msa level
  
  msa_verification<-msa %>%
    group_by(msa_id,msa_name,occupation_code) %>%
    summarise(tot_emp_msa=sum(tot_emp))
  
  
  ##Merge msa summary with occupation taxonomy
  
  msa_count_file<-merge(msa_verification,occupation_taxonomy,by.x = "occupation_code",by.y = "code",all.x = TRUE)
  
  ##Make sure that we save the data in data frame
  
  msa_count_file<-as.data.frame(msa_count_file)
  
  ###Create Summary of total supply by State
  
  msa_summary<-msa_count_file %>%
    group_by(msa_id,msa_name) %>%
    summarise(tot_supply_msa=sum(tot_emp_msa))
  
  ##Make sure that we save the data in data frame
  
  msa_summary<-as.data.frame(msa_summary)
  
  ###Save report file MSA
  
  output_dir<-('./report_files')
  
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  workbook_msa<-createWorkbook()
  addWorksheet(workbook_msa,"Count_msa_occupations")
  addWorksheet(workbook_msa,"Summary_msa")
  
  writeData(workbook_msa,1,msa_count_file,rowNames = FALSE)
  writeData(workbook_msa,2,msa_summary,rowNames = FALSE)
  
  saveWorkbook(workbook_msa, paste0(output_dir,"/",country_name,"_msa_report_",year,".xlsx"), overwrite = TRUE)
  
  
  
  
  ##Start the tests using local occupations file
  ##1.- Verify that the number of available local codes at Country level is equal than the State level
  
  ##Summarize values by local occupation code at state level
  
  total_state_count<-state %>%
    group_by(occupation_code) %>%
    summarise(tot_emp_state=sum(tot_emp))
  
  country_count<-merge(country,total_state_count,by.x='occupation_code',by.y = 'occupation_code',all.x = TRUE)
  
  
  if (nrow(country_count) != nrow(total_state_count)) {
    print('The number of local codes in the Country file and the State file does not match, please verify the State and Country files')
    
  } else {print('Country and State level files local codes taxonomy have been verified, we can proceed to next validation step')}
  
  
  ##Merge country values with State values
  ##2.- Verify that the sum by occupation at Country level is equal than the State level
  
  country_count<-merge(country,total_state_count,by.x='occupation_code',by.y = 'occupation_code')
  
  ##Verify that the sum of total supply crossed local codes at State level is equal to Country level
  
  if (sum(country_count$tot_emp_state) != sum(country_count$tot_emp)){
    
    print('The sum of total supply crossed local codes at State level is not equal to the sum at Country level')
    
    sum_country_local<-sum(country_count$tot_emp)
    
    sum_state_local<-sum(country_count$tot_emp_state)
    
    difference_local<-abs(sum_country_local-sum_state_local)
    
    print(paste0('The total supply in the country file is equal to ',sum_country_local))
    
    print(paste0('The total supply in the state file is equal to ',sum_state_local))
    
    print(paste0('There is a difference of ',difference_local,' individuals in the total supply between Country and State'))
    
    
    
  } else {
    
    print('The sum of total supply crossed local codes at State level is equal to the sum at Country level,we can proceed to next validation step')
    
  }
  
  ##Generate a dummy variable in order to detect local codes with larger supply numbers than the country level
  
  country_count<-country_count %>%
    mutate(dummy=as.numeric(tot_emp_state>tot_emp))
  
  if (sum(country_count$dummy > 0,na.rm = TRUE)){
    
    print('There are some codes for which the sum by occupational local codes at State level is larger than its Country counterpart, a file containing this codes has been saved in the error folder')
    
    country_count_errors<-country_count %>%
      filter(dummy==1)
    
    output_dir<-('./error_folder_local_country')
    
    if (!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    
    write.csv(country_count_errors,paste0(output_dir,'/country_count_errors_by_occupation_',year,'.csv'),row.names = FALSE)
    
  } else {print('The sum of total supply by occupation local codes at State level is equal or lower to its Country counterpart, we can proceed to next validation step ')}
  
  
  ##3.- Verify that the sum by occupation at MSA level is equal or less than the State level
  
  total_msa_count<-msa %>%
    group_by(occupation_code,state_id, state_name) %>%
    summarise(tot_emp_msa=sum(tot_emp))
  
  msa_validation<-merge(total_msa_count,state,by.x = c('occupation_code','state_id'),by.y = c('occupation_code','state_id'),all.x=TRUE)
  
  
  ##Generate a dummy variable in order to detect local codes at msa level with larger-
  ##supply numbers than the state level
  
  msa_validation<-msa_validation %>%
    mutate(dummy=as.numeric(tot_emp_msa > tot_emp))
  
  if (sum(msa_validation$dummy > 0,na.rm = TRUE)){
    
    print('There are some codes for which the sum by occupational local codes at MSA level is larger than its State counterpart, a file containing this codes by State has been saved in the error folder')
    
    msa_validation_errors<-msa_validation %>%
      filter(dummy==1)
    
    output_dir<-('./error_folder_local_state')
    
    if (!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    
    write.csv(msa_validation_errors,paste0(output_dir,'/state_count_errors_by_occupation_',year,'.csv'),row.names = FALSE)
    
  } else {print('The sum of total supply by occupation local codes at MSA level is equal or lower to its State counterpart, we can proceed to next validation step ')}
  
  ##Disconnect from data base
  
  all_cons <- dbListConnections(MySQL())
  
  for(con in all_cons){dbDisconnect(con)}} else {
    
    ##If Onet files are available for validation, this part of the sctipt will be ran
    
    ##Set the directory where the final files are available
    
    setwd(paste0('/data/Stata/Talent_supply_update/SUPPLY_TRANSITION/',country_name))
    
    ## Validate files using local codes structure
    
    ##Read the files at Country, State and MSA level
    
    country<-read.csv(paste0('./Input_files/Supply_count_country_',year,'.csv'))
    
    state<-read.csv(paste0('./Input_files/Supply_count_state_',year,'.csv'))
    
    msa<-read.csv(paste0('./Input_files/Supply_count_msa_',year,'.csv'))
    
    country_id<-country[2,2]
    
    ### Add msa ids, msa names, state id and names to MSA data frame
    
    mydb = dbConnect(MySQL(), user=user, password=password, host=host)
    
    msa_query = paste0("select msa_id,msa_name,state_id,state_name from norm_prod.place_details_2016
                       where place_type_id=3 and country_id=",country_id)
    
    msa_taxonomy = dbSendQuery(mydb, msa_query)
    
    msa_taxonomy = fetch(msa_taxonomy, n=-1)
    
    msa<-merge(msa,msa_taxonomy,by.x='msa_id',by.y='msa_id',all.x=TRUE)
    
    ##Create repport for verification at state level
    
    state_verification<-state %>%
      group_by(state_id,occupation_code) %>%
      summarise(tot_emp_state=sum(tot_emp))
    
    state_taxonomy_query = paste0("select state_id,state_name from norm_prod.place_details_2016
                                  where country_id=",country_id," and place_type_id=5;")
    
    state_taxonomy=dbSendQuery(mydb,state_taxonomy_query)
    
    state_taxonomy= fetch(state_taxonomy, n=-1)
    
    ##Add Occupation name from db taxonomy
    
    occupation_taxonomy= paste0("select code,name from norm_prod.occupations
                                where occupation_type_id=",occupation_type_id," and level=",level_occupation,";")
    
    occupation_taxonomy_query<-dbSendQuery(mydb,occupation_taxonomy)
    
    occupation_taxonomy=fetch(occupation_taxonomy_query, n= -1)
    
    
    ###Merge states and occupation taxonomy with summary of state
    
    state_count_file<-merge(state_verification,state_taxonomy,by.x = "state_id",by.y = "state_id",all.x = TRUE)
    
    state_count_file<-merge(state_count_file,occupation_taxonomy,by.x = "occupation_code",by.y = "code",all.x = TRUE)
    
    ##Make sure that we save the data in data frame
    
    state_count_file<-as.data.frame(state_count_file)
    
    ###Create Summary of total supply by State
    
    state_summary<-state_count_file %>%
      group_by(state_id,state_name) %>%
      summarise(tot_supply_state=sum(tot_emp_state))
    
    ##Make sure that we save the data in data frame
    
    state_summary<-as.data.frame(state_summary)
    
    ###Save report file State
    
    output_dir<-('./report_files')
    
    if (!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    
    workbook_state<-createWorkbook()
    addWorksheet(workbook_state,"Count_state_occupations")
    addWorksheet(workbook_state,"Summary_state")
    
    writeData(workbook_state,1,state_count_file,rowNames = FALSE)
    writeData(workbook_state,2,state_summary,rowNames = FALSE)
    
    saveWorkbook(workbook_state, paste0(output_dir,"/",country_name,"_state_report_",year,".xlsx"), overwrite = TRUE)
    
    ##Create repport for verification at msa level
    
    msa_verification<-msa %>%
      group_by(msa_id,msa_name,occupation_code) %>%
      summarise(tot_emp_msa=sum(tot_emp))
    
    
    ##Merge msa summary with occupation taxonomy
    
    msa_count_file<-merge(msa_verification,occupation_taxonomy,by.x = "occupation_code",by.y = "code",all.x = TRUE)
    
    ##Make sure that we save the data in data frame
    
    msa_count_file<-as.data.frame(msa_count_file)
    
    ###Create Summary of total supply by MSA
    
    msa_summary<-msa_count_file %>%
      group_by(msa_id,msa_name) %>%
      summarise(tot_supply_msa=sum(tot_emp_msa))
    
    ##Make sure that we save the data in data frame
    
    msa_summary<-as.data.frame(msa_summary)
    
    ###Save report file MSA
    
    output_dir<-('./report_files')
    
    if (!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    
    workbook_msa<-createWorkbook()
    addWorksheet(workbook_msa,"Count_msa_occupations")
    addWorksheet(workbook_msa,"Summary_msa")
    
    writeData(workbook_msa,1,msa_count_file,rowNames = FALSE)
    writeData(workbook_msa,2,msa_summary,rowNames = FALSE)
    
    saveWorkbook(workbook_msa, paste0(output_dir,"/",country_name,"_msa_report_",year,".xlsx"), overwrite = TRUE)
    
    
    
    ##Start the tests using local occupations file
    ##1.- Verify that the number of available local codes at Country level is equal than the State level
    
    ##Summarize values by local occupation code at state level
    
    total_state_count<-state %>%
      group_by(occupation_code) %>%
      summarise(tot_emp_state=sum(tot_emp))
    
    country_count<-merge(country,total_state_count,by.x='occupation_code',by.y = 'occupation_code',all.x = TRUE)
    
    
    if (nrow(country_count) != nrow(total_state_count)) {
      print('The number of local codes in the Country file and the State file does not match, please verify the State and Country files')
      
    } else {print('Country and State level files local codes taxonomy have been verified, we can proceed to next validation step')}
    
    
    ##Merge country values with State values
    ##2.- Verify that the sum by occupation at Country level is equal than the State level
    
    country_count<-merge(country,total_state_count,by.x='occupation_code',by.y = 'occupation_code')
    
    ##Verify that the sum of total supply crossed local codes at State level is equal to Country level
    
    if (sum(country_count$tot_emp_state) != sum(country_count$tot_emp)){
      
      print('The sum of total supply crossed local codes at State level is not equal to the sum at Country level')
      
      sum_country_local<-sum(country_count$tot_emp)
      
      sum_state_local<-sum(country_count$tot_emp_state)
      
      difference_local<-abs(sum_country_local-sum_state_local)
      
      print(paste0('The total supply in the country file is equal to ',sum_country_local))
      
      print(paste0('The total supply in the state file is equal to ',sum_state_local))
      
      print(paste0('There is a difference of ',difference_local,' individuals in the total supply between Country and State'))
      
      
      
    } else {
      
      print('The sum of total supply crossed local codes at State level is equal to the sum at Country level,we can proceed to next validation step')
      
    }
    
    ##Generate a dummy variable in order to detect local codes with larger supply numbers than the country level
    
    country_count<-country_count %>%
      mutate(dummy=as.numeric(tot_emp_state>tot_emp))
    
    if (sum(country_count$dummy > 0,na.rm = TRUE)){
      
      print('There are some codes for which the sum by occupational local codes at State level is larger than its Country counterpart, a file containing this codes has been saved in the error folder')
      
      country_count_errors<-country_count %>%
        filter(dummy==1)
      
      output_dir<-('./error_folder_local_country')
      
      if (!dir.exists(output_dir)){
        dir.create(output_dir)
      }
      
      write.csv(country_count_errors,paste0(output_dir,'/country_count_errors_by_occupation_',year,'.csv'),row.names = FALSE)
      
    } else {print('The sum of total supply by occupation local codes at State level is equal or lower to its Country counterpart, we can proceed to next validation step ')}
    
    
    ##3.- Verify that the sum by occupation at MSA level is equal or less than the State level
    
    total_msa_count<-msa %>%
      group_by(occupation_code,state_id, state_name) %>%
      summarise(tot_emp_msa=sum(tot_emp))
    
    msa_validation<-merge(total_msa_count,state,by.x = c('occupation_code','state_id'),by.y = c('occupation_code','state_id'),all.x=TRUE)
    
    
    ##Generate a dummy variable in order to detect local codes at msa level with larger-
    ##supply numbers than the state level
    
    msa_validation<-msa_validation %>%
      mutate(dummy=as.numeric(tot_emp_msa > tot_emp))
    
    if (sum(msa_validation$dummy > 0,na.rm = TRUE)){
      
      print('There are some codes for which the sum by occupational local codes at MSA level is larger than its State counterpart, a file containing this codes by State has been saved in the error folder')
      
      msa_validation_errors<-msa_validation %>%
        filter(dummy==1)
      
      output_dir<-('./error_folder_local_state')
      
      if (!dir.exists(output_dir)){
        dir.create(output_dir)
      }
      
      
      write.csv(msa_validation_errors,paste0(output_dir,'/state_count_errors_by_occupation_',year,'.csv'),row.names = FALSE)
      
      
      
    } else {print('The sum of total supply by occupation local codes at MSA level is equal or lower to its State counterpart, we can proceed to next validation step ')}
    
    
    ##Disconnect from data base
    
    all_cons <- dbListConnections(MySQL())
    
    for(con in all_cons){dbDisconnect(con)}
    
    
    #########################################################################################################################
    ############################### Validate files using onet codes structure ###############################################
    #########################################################################################################################
    
    ##Set directory
    
    setwd(paste0('/data/Stata/Talent_supply_update/SUPPLY_TRANSITION/',country_name))
    
    ##Read the files at Country, State and MSA level
    
    country<-read.csv(paste0('./Output_files/country_supply_output_',year,'.csv'))
    
    state<-read.csv(paste0('./Output_files/state_supply_output_',year,'.csv'))
    
    msa<-read.csv(paste0('./Output_files/msa_supply_output_',year,'.csv'))
    
    ##Extract country_id
    
    country_id<-country[2,2]
    
    ### Add msa ids, msa names, state id and names to MSA data frame
    
    mydb = dbConnect(MySQL(), user= user , password= password, host=host)
    
    msa_query = paste0("select msa_id,msa_name,state_id,state_name from norm_prod.place_details_2016
                       where place_type_id=3 and country_id=",country_id)
    
    msa_taxonomy = dbSendQuery(mydb, msa_query)
    
    msa_taxonomy = fetch(msa_taxonomy, n=-1)
    
    msa<-merge(msa,msa_taxonomy,by.x='msa_id',by.y='msa_id',all.x=TRUE)
    
    
    
    ##Start the tests using Onet occupations file
    ##1.- Verify that the number of available Onet codes at Country level is equal than the State level
    
    ##Summarize values by Onet occupation code at state level
    
    total_state_count<-state %>%
      group_by(occupation_code) %>%
      summarise(tot_emp_state=sum(tot_emp))
    
    country_count<-merge(country,total_state_count,by.x='occupation_code',by.y = 'occupation_code',all.x = TRUE)
    
    
    if (nrow(country_count) != nrow(total_state_count)) {
      print('The number of Onet codes in the Country file and the State file does not match, please verify the State and Country files')
      
    } else {('Country and State level files Onet codes taxonomy have been verified, we can proceed to next validation step') }
    
    
    ##Merge country values with State values
    ##2.- Verify that the sum by occupation at Country level is equal than the State level
    
    country_count<-merge(country,total_state_count,by.x='occupation_code',by.y = 'occupation_code')
    
    
    ##Verify that the sum of total supply crossed Onet codes at State level is equal to Country level
    
    if (sum(country_count$tot_emp_state == sum(country_count$tot_emp))){
      
      print('The sum of total supply crossed Onet codes at State level is not equal to the sum at Country level')
      
    } else {
      
      print('The sum of total supply crossed Onet codes at State level is equal to the sum at Country level,we can proceed to next validation step')
      
    }
    
    
    ##Generate a dummy variable in order to detect Onet codes with larger-
    ##supply numbers than the country level
    
    country_count<-country_count %>%
      mutate(dummy=as.numeric(tot_emp_state>tot_emp))
    
    if (sum(country_count$dummy > 0)){
      
      print('There are some codes for which the sum by occupational Onet codes at State level is larger than its Country counterpart
            , a file containing this codes has been saved in the error folder')
      
      country_count_errors<-country_count %>%
        filter(dummy==1)
      
      output_dir<-('./error_folder_Onet_country')
      
      if (!dir.exists(output_dir)){
        dir.create(output_dir)
      }
      
      setwd(output_dir)
      
      write.csv(country_count_errors,paste0(output_dir,'/country_count_errors_by_occupation_',year,'.csv'),row.names = FALSE)
      
      
      
    } else {print('The sum of total supply by occupation Onet codes at State level is equal or lower to its Country counterpart, we can proceed to next validation step ')}
    
    
    ##3.- Verify that the sum by occupation at MSA level is equal or less than the State level
    ## Variable in the msa file for onet code shoud be occupation_code
    
    
    total_msa_count<-msa %>%
      group_by(occupation_code,state_id, state_name) %>%
      summarise(tot_emp_msa=sum(tot_emp))
    
    msa_validation<-merge(total_msa_count,state,by.x = c('occupation_code','state_id'),by.y = c('occupation_code','state_id'),all.x=TRUE)
    
    
    ##Generate a dummy variable in order to detect Onet codes at msa level with larger-
    ##supply numbers than the state level
    
    msa_validation<-msa_validation %>%
      mutate(dummy=as.numeric(tot_emp_msa - tot_emp>10))
    
    if (sum(msa_validation$dummy > 0)){
      
      print('There are some codes for which the sum by occupational Onet codes at MSA level is larger than its State counterpart
        , a file containing this codes by State has been saved in the error folder')
      
      msa_validation_errors<-msa_validation %>%
        filter(dummy==1)
      
      output_dir<-('./error_folder_Onet_state')
      
      if (!dir.exists(output_dir)){
        dir.create(output_dir)
      }
      
      
      write.csv(msa_validation_errors,paste0(output_dir,'/state_count_errors_by_occupation_',year,'.csv'),row.names = FALSE)
      
      
      
    } else {print('The sum of total supply by occupation Onet codes at MSA level is equal or lower to its State counterpart, we can proceed to next validation step ')}
    
    
    ##Disconnect from data base
    
    all_cons <- dbListConnections(MySQL())
    
    for(con in all_cons){dbDisconnect(con)}}
}


