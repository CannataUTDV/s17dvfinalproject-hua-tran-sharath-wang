require(data.world)
require(readr)
require(DT)
require(dplyr)

#DO THIS BEFORE RUNNING THE SCRIPT======================================================
# set your working directory to 01 Data 
#Download HospitalData from data.world: https://data.world/ninaxhua/s-17-edv-project-1
#Rename the .csv as PreETL_HospitalData.csv
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
df = rename(df, edu.males = males, edu.females = females, edu.total = total25)
# # reset the column types
# amt_death_f = as.factor(df$amt_death)
# 
# amt_death_n = as.numeric(as.character(amt_death_f))
# 
# 
# df$amt_death = amt_death_n
# View(df)
