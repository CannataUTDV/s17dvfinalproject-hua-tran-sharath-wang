# loading required packages 
require(readr)
require(plyr)
require(dplyr)
<<<<<<< Updated upstream
require(data.world)
require(readr)
require(DT)
require(dplyr)

#DO THIS BEFORE RUNNING THE SCRIPT======================================================
# set your working directory to 01 Data 
#Download PreETL_Death.csv from data.world: https://data.world/ninaxhua/s-17-dv-final-project
#Put PreETL_HospitalData.csv into CSVs folder in the DataVisualizations folder
#=======================================================================================

education = query(
        data.world(),
        dataset="uscensusbureau/acs-2015-5-e-education", type="sql",
        query="select State, AreaName, B15002_001 as total25, B15002_002 as males, B15002_019 as females, B15002_003 as m_no_school, B15002_011 as m_hs, B15002_015 as m_bs, B15002_016 as m_ms, B15002_018 as m_phd, B15002_020 as f_no_school, B15002_028 as f_hs, B15002_032 as f_bs, B15002_033 as f_ms, B15002_035 as f_phd from USA_All_States")

# file path to raw death csv 
filePath = file.path("../../CSVs/PreETL_Death.csv")

# reading csv file as a variable 
rawDeath <- readr::read_csv(filePath)

# removing columns: invoice number, po number, mfg name, catalog code, effective date, expiration date, contract number, and profile name 
death = rawDeath %>% dplyr::select(., -`113_CAUSE_NAME`) %>% plyr::rename(., c("YEAR" = "year", "CAUSE_NAME" = "cause", "STATE" = "AreaName", "DEATHS" = "amt_death"))

# join data: full outer join between two tables
df = full_join(x = education, y = death, by = "AreaName")

# rename columns
df = dplyr::rename(df, edu.males = males, edu.females = females, edu.total = total25)
=======

#DO THIS BEFORE RUNNING THE SCRIPT======================================================
# set your working directory to 01 Data 
#Download HospitalData from data.world: https://data.world/ninaxhua/s-17-edv-project-1
#Rename the .csv as PreETL_HospitalData.csv
#Put PreETL_HospitalData.csv into CSVs folder in the DataVisualizations folder
#=======================================================================================

# retrieve data from data.world 
source("Retrieve_Data.R")

>>>>>>> Stashed changes
# determine column types 
str(df)

# dimensions 
dimensions = c("State", "AreaName", "cause")

# measures   
measures = setdiff(names(df), dimensions)

# remove ASCII characters 
for(n in names(df)) {
<<<<<<< Updated upstream
        df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
=======
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
>>>>>>> Stashed changes
}

# if column is a dimension, turn all NA values to empty strings 
na2emptyString <- function (x) {
<<<<<<< Updated upstream
        x[is.na(x)] <- ""
        return(x)
}
if( length(dimensions) > 0) {
        for(d in dimensions) {
                # Change NA to the empty string.
                df[d] <- data.frame(lapply(df[d], na2emptyString))
                # Get rid of " and ' in dimensions.
                df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
                # Change & to and in dimensions.
                df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
                # Change : to ; in dimensions.
                df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
        }
=======
  x[is.na(x)] <- ""
  return(x)
}
if( length(dimensions) > 0) {
  for(d in dimensions) {
    # Change NA to the empty string.
    df[d] <- data.frame(lapply(df[d], na2emptyString))
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
  }
>>>>>>> Stashed changes
}

# if column is a measure, turn all NA values to 0 
na2zero <- function (x) {
<<<<<<< Updated upstream
        x[is.na(x)] <- 0
        return(x)
=======
  x[is.na(x)] <- 0
  return(x)
>>>>>>> Stashed changes
}

# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions, and change NA to 0.
if( length(measures) > 1) {
<<<<<<< Updated upstream
        for(m in measures) {
                print(m)
                df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
                df[m] <- data.frame(lapply(df[m], na2zero))
                df[m] <- lapply(df[m], function(x) {
                        as.numeric(as.character(x))
                })
                #   df[m] <- data.frame(lapply(df[m], as.numeric(as.character(.)))) # This is needed to turn measures back to numeric because gsub turns them into strings.
        }
}

# write new cleaned csv file 
write.csv(df, gsub("PreETL_", "", filePath), row.names=FALSE, na = "")
=======
  for(m in measures) {
    print(m)
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
   df[m] <- data.frame(lapply(df[m], na2zero))
   df[m] <- lapply(df[m], function(x) {
           as.numeric(as.character(x))
   })
#   df[m] <- data.frame(lapply(df[m], as.numeric(as.character(.)))) # This is needed to turn measures back to numeric because gsub turns them into strings.
  }
}


# write new cleaned csv file 
write.csv(df, gsub("PreETL_", "", filePath), row.names=FALSE, na = "")





>>>>>>> Stashed changes
