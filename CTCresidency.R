



library(tidyverse)
library(janitor)
library(MCOE)
library(here)
library(ggthemes)


con <- mcoe_sql_con()

districts <- c("Greenfield",
               "King City", 
               "Salinas Union",
               "Santa Rita"
)

districts.list <- paste0(districts, collapse = "|")


### Student Race ------


enroll <- tbl(con, "ENROLLMENT") %>% 
    filter(COUNTY == "Monterey",
           #        DistrictCode == "10272",
           YEAR  == max(YEAR)
    ) %>%
    #       head(20) %>%
    collect() %>%
    mutate(ETHNIC = as.character(ETHNIC)) %>%
    left_join_codebook("ENROLLMENT", "ETHNIC")


enroll.mry <- enroll %>%
    filter(str_detect(DISTRICT,districts.list))   %>%
    group_by(DISTRICT, definition) %>%
    summarise(enr = sum(ENR_TOTAL)) %>%
    mutate(perc.enr = 100*enr/sum(enr))



### Effective / Credential -------

teach <- tbl(con, "Teaching") %>% 
    filter(County_Name == "Monterey",
           Aggregate_Level == "D"
           #        DistrictCode == "10272",
          # YEAR  == max(YEAR)
    ) %>%
  #         head(20) %>%
    collect() 


teach.grant <- teach %>%
    filter(str_detect(District_Name,districts.list),
           Teacher_Experience_Level == "ALL",
           Charter_School == "No",
           Teacher_Credential_Level == "ALL",
           Subject_Area == "TA",
           DASS == "All",
           School_Grade_Span == "ALL"
           )




###  Teacher Race -----


staff_demo <- tbl(con, "STAFF_DEMO")  %>%
    filter( CountyName == "Monterey",
            AcademicYear == max(AcademicYear)
    ) %>%
    collect()


staff_demo2 <- staff_demo %>%
    filter(FTE_Teaching > 0,
           str_detect(DistrictName,districts.list)) %>%
    mutate(EthnicGroup = as.character(EthnicGroup)) %>%
    left_join_codebook("STAFF_DEMO", "EthnicGroup") %>%
    group_by(DistrictName, definition) %>%
    count() %>%
    group_by(DistrictName) %>%
    mutate(perc = 100*n/sum(n)) 
