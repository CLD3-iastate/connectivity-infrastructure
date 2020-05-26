library(naniar)

#
# merge acs data (geometry for map) and SCHEV data =======================
#

acs <- readRDS("./Data/acs.Rds")
schev <- readRDS("./Data/schev.rds")


# check if either data set has more observations (counties/cities)

setdiff(acs$COUNTYFP, schev$COUNTYFP)  # none
setdiff(schev$COUNTYFP, acs$COUNTYFP)  # 515: Bedford City, 902: In-state Unknown, 
                                       # 903: In-State Military Unknown

# NOTE: Bedford City reverted to town status in 2013 which is why it is not in ACS.
# For now, I will ignore these three "counties"

appdata <- merge(acs, schev, by=c("STATEFP", "COUNTYFP"))  # inner join

miss_var_summary(appdata)  

write_rds(appdata, "./Data/higher_ed_tab.Rds")