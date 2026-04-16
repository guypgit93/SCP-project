# Sample Code for your request:  49273f34301f49f08c202efda20e346c
# ******************************************************************************

rm(list=ls())

# Replace "where" with the filepath of the working folder (where any temporary files created by this programme will be stored)   eg:  c:\ukhls\temp
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofManchester/Documents/ECON60052/Midterm")

# Replace "where" with the folderpath where the data has been downloaded and unzipped
# eg: c:/ukhls_data/UKDA-6614-stata/stata/stata13_se/ukhls
ukhls <- "~/Downloads/UKDA-6614-stata/stata/stata14_se/ukhls"

# Replace "where" with the filepath of the folder where you want to store the final dataset produced by this programme.
# eg: c:/ukhls/results
outputpath <- "~/Library/CloudStorage/OneDrive-TheUniversityofManchester/Documents/ECON60052/Midterm"

# The file produced by this programme will be named as below. If you want to change the name do it here.
outputfilename <- "UKHLS_long_ijklmno"

# By default the data will be extracted from the waves whose letter prefixes are written below, and merged. If you want to a different selection of waves, make the change here
allWaves <- c("i j k l m n o")

# These variables from the indall files will be included. These include some key variables as determined by us PLUS any variables requested by you.
indallvars <- c("age_dv country ethn_dv ff_ivlolw gor_dv marstat_dv hhsize hidp intdatm_dv intdaty_dv iviolw mastat_dv nchild_dv pidp pno psnen01_lw psnen01_xw psnen91_lw psnen91_xw psnen99_lw psnen99_xw psneng2_lw psneng2_xw psnenub_lw psnenub_xw psnenui_lw psnenui_xw psnenus_lw psnenus_xw psu racel_dv region sex_dv strata urban_dv")

# These variables from the indresp files will be included. These include some key variables as determined by us PLUS any variables requested by you.
indvars <- c("age_dv benctc benbase1 benbase2 benbase3 benbase4 benbase96 country ethn_dv ff_emplw ff_ivlolw ffbrfedlw fimnnet_dv ftexw gor_dv hhsize hhtype_dv hidp ind5mus_lw ind5mus_xw indbd91_lw indbdub_lw indin01_lw indin01_xw indin91_lw indin91_xw indin99_lw indin99_xw inding2_lw inding2_xw indinub_lw indinub_xw indinui_lw indinui_xw indinus_lw indinus_xw indns91_lw indnsub_lw indpxg2_xw indpxub_lw indpxub_xw indpxui_lw indpxui_xw indpxus_lw indpxus_xw indscg2_xw indscub_lw indscub_xw indscui_lw indscui_xw indscus_lw intdatm_dv intdaty_dv iviolw jbstat mastat_dv marstat_dv mlstat nbornlw nchild_dv pidp pno psu racel_dv region scghq1_dv scghq2_dv sex_dv strata tenure_dv ukborn urban_dv")
# These variables from the child files will be included. These include some key variables as determined by us PLUS any variables requested by you.
chvars <- c("age_dv chddvg2_xw chddvub_lw chddvub_xw chddvui_lw chddvui_xw country gor_dv hhsize hidp intdatm_dv intdaty_dv iviolw marstat_dv pidp pno psnen01_lw psnen91_lw psneng2_lw psneng2_xw psnenub_lw psnenub_xw psnenui_lw psnenui_xw psnenus_lw psnenus_xw psu region sex_dv strata urban_dv")

# These variables from the hhresp files will be included. These include some key variables as determined by us PLUS any variables requested by you.
hhvars <- c("country fihhmnnet1_dv gor_dv hhden01_xw hhden91_xw hhden99_xw hhdeng2_xw hhdenub_xw hhdenui_xw hhdenus_xw hhsize hhtype_dv hidp hsivlw ieqmoecd_dv nkids_dv nkids05 nkids615 nkids015 psu region strata tenure_dv urban_dv xpgaslw xpleclw")

# These variables from the youth files will be included. These include some key variables as determined by us PLUS any variables requested by you.
youthvars <- c("age_dv country ethn_dv gor_dv hidp intdatm_dv intdaty_dv pidp pno psu racel_dv sex_dv strata urban_dv ypsmlw ypwklw ythscg2_xw ythscub_xw ythscui_xw ythscus_xw")

# These variables from the nurse labblood file will be included. These include some key variables as determined by us PLUS any variables requested by you. 
xlabbloodvars  <- c("")

# These variables from the nurse epigenetic clocks file will be included. These include some key variables as determined by us PLUS any variables requested by you. 
xepigenclockvars <- c("")

# These variables from the nurse proteomic file will be included. These include some key variables as determined by us PLUS any variables requested by you. 
xproteovars <- c("")
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# // Anything below this line should not be changed! Any changes to the selection of variables and waves, and location of folders, should be made above. //
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Function to check if a package is installed and install it if necessary
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE, type = "binary")
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE, type = "source", INSTALL_opts = "--no-multiarch")
      library(package, character.only = TRUE)
    }
  }
}
  
  # Check and install necessary packages
  check_and_install("haven")
  check_and_install("dplyr")
  check_and_install("tidyr")
  check_and_install("foreach")

#  // this program returns all variable names with the wave prefix
getVars <- function(arg1, arg2) {
  if (arg2== "") {
    wavemyvars <- arg1
  } else if (arg1 != "" & !is.null(arg2)) {
    # Split the string into separate elements
    wavemyvar <- strsplit(arg1, " ")[[1]]
    # Add the prefix "*_" to each element
    wavemyvar_prefix <- paste0(arg2,"_", wavemyvar)  
    # Concatenate the elements back into a single string
    wavemyvars <- paste(wavemyvar_prefix, collapse = " ")
  }  else {
    wavemyvars <- ""
  }
  return(wavemyvars)
}

# // this program to returns  which variables exist in this wave
getExistingVars <- function(vars, df) {
  existingVars <- c()
  for (var in vars) {
    if (var %in% colnames(df)) {
      existingVars <- c(existingVars, var)
    }
  }
  return(existingVars)
}

# Split the wave string into individual letters
allWaves <- strsplit(allWaves, " ")[[1]]

# Loop through each wave
for (wave in allWaves) {
  # Find the wave number (letter corresponding to the wave)
  waveno <- which(strsplit("abcdefghijklmnopqrstuvwxyz", "")[[1]] == wave)
  
  # find the wave household vars  
  wavehhvars <- getVars(hhvars, wave)
  
  # find the wave individual vars
  waveindvars <- getVars(indvars, wave)
  
  # find the wave all individual vars
  waveindallvars <- getVars(indallvars, wave)
  
  # find the wave child vars
  wavechvars <- getVars(chvars, wave)
  
  # find the wave youth vars
  waveyouthvars <- getVars(youthvars, wave)
  
  # open the the household level file with the required variable
  hhresp_dat <- read_dta(paste0(ukhls, "/", wave, "_hhresp.dta"))
  
  vars_to_check <- c(paste0(wave, "_hidp"), wavehhvars)
  vars_to_check <- unlist(strsplit(vars_to_check, " "))
  
  # get the existing variables using the getExistingVars function
  existingVars <- getExistingVars(vars_to_check, hhresp_dat)
  
  # keep only the existing variables in the data frame
  df_hhresp <- hhresp_dat %>% select(all_of(existingVars))
  
  # check if individual, child, or youth variables are required
  if (!is.null(indvars) || !is.null(chvars) || !is.null(youthvars)) {
    # if any individual variable is required, first merge INDALL keeping the pidp
    indall_data <- right_join(df_hhresp, read_dta(paste0(ukhls, "/", wave, "_indall.dta")), by = paste0(wave, "_hidp"))
    
    # drop rows where pidp is missing (loose households with no individuals)
    indall_data <- indall_data[!is.na(indall_data$pidp), ]
    
    # get the existing variables using the getExistingVars function
    vars_to_check <- c("pidp", paste0(wave, "_hidp"), wavehhvars, waveindallvars)
    vars_to_check <- unlist(strsplit(vars_to_check, " "))
    existingVars <- getExistingVars(vars_to_check, indall_data)
    
    # keep only the existing variables in the data frame
    df_alldata <- indall_data %>% select(all_of(existingVars))
    
    # add any requested individual variables
    if (!is.null(indvars)){
      # Remove the duplicate variable from df_indresp before merging to avoid any suffix being added
      df_indresp <- read_dta(paste0(ukhls, "/", wave, "_indresp.dta"))
      df_indresp <- df_indresp[ , !names(df_indresp) %in% names(df_alldata) | names(df_indresp) == "pidp"]
      
      indvars_dat <- merge(df_alldata, df_indresp, by = "pidp", all = TRUE)
      
      # get the existing variables using the getExistingVars function
      vars_to_check <- c("pidp", paste0(wave, "_hidp"), wavehhvars, waveindvars, waveyouthvars, wavechvars, waveindallvars)
      vars_to_check <- unlist(strsplit(vars_to_check, " "))
      existingVars <- getExistingVars(vars_to_check, indvars_dat)
      
      # keep only the existing variables in the data frame
      df_alldata <- indvars_dat %>% select(all_of(existingVars))
    }
    
    # add any requested youth variables
    if (!is.null(waveyouthvars)){
      # remove the duplicate variable from df_youth before merging to avoid any suffix being added
      df_youth <- read_dta(paste0(ukhls, "/", wave, "_youth.dta"))
      df_youth <- df_youth[ , !names(df_youth) %in% names(df_alldata) | names(df_youth) == "pidp"]
      
      youth_dat <- merge(df_alldata, df_youth, by = "pidp", all = TRUE)
      
      # get the existing variables using the getExistingVars function
      vars_to_check <- c("pidp", paste0( wave, "_hidp"), wavehhvars, waveindvars, waveyouthvars, wavechvars, waveindallvars)
      vars_to_check <- unlist(strsplit(vars_to_check, " "))
      existingVars <- getExistingVars(vars_to_check, youth_dat)
      
      # keep only the existing variables in the data frame
      df_alldata <- youth_dat %>% select(all_of(existingVars))
    }
    
    # add any requested child variables
    if (!is.null(wavechvars)){
      df_child <- read_dta(paste0(ukhls, "/", wave, "_child.dta"))
      df_child <- df_child[ , !names(df_child) %in% names(df_alldata) | names(df_child) == "pidp"]
      child_dat <- merge(df_alldata, df_child, by = "pidp", all = TRUE)
      
      # Get the existing variables using the getExistingVars function
      vars_to_check <- c("pidp", paste0(wave, "_hidp"), wavehhvars, waveindvars, waveyouthvars, wavechvars, waveindallvars)
      existingVars <- getExistingVars(unlist(strsplit(vars_to_check, " ")), child_dat)
      
      # Keep only the existing variables in the data frame
      df_alldata <- child_dat %>% select(all_of(existingVars))
    }
}	
  # Create a wave variable
  df_alldata$wavename <- waveno
  
  # Drop the wave prefix from all variables
  colnames(df_alldata) <- gsub(paste0(wave, "_"), "", colnames(df_alldata))
  
  # Save the file that was created
  saveRDS(df_alldata, file = paste0("temp_", wave, ".rds"))
	
  }

#Merge and produce final files

  df_append <- NULL  # Initialize an empty dataframe to store the result
  for (w in allWaves) {
    df_temp <- readRDS(paste0("temp_", w, ".rds"))
    df_append <- bind_rows(df_append, df_temp)
  }
  
  # Move pidp to the beginning of the file
  df_longdata <- df_append[order(df_append$pidp), ]
  df_longdata <- df_longdata[order(df_longdata$pidp), ]
  
  # Check how many observations are available from each wave
  table(df_longdata$wavename)
  
  # Save the long file
  saveRDS(df_longdata, file = paste0(outputpath, "/", outputfilename, ".rds"))
  
  # Erase temporary files
  for (wave in allWaves) {
    file.remove(paste0("temp_", wave, ".rds"))
  }