library(COVID19)

countries_with_state_data <- unique(covid19(level = 2)$administrative_area_level_1)

# [1] "United Kingdom" "Colombia"       "Latvia"         "Canada"         "Russia"        
# [6] "Czech Republic" "Japan"          "South Africa"   "France"         "Italy"         
#[11] "China"          "Haiti"          "Switzerland"    "Netherlands"    "Sweden"        
#[16] "India"          "United States"  "Australia"      "Belgium"        "Denmark" 

for (country in countries_with_state_data) {
  message(paste(toString(length(unique(covid19(c(country), level=2, verbose=FALSE)$deaths))), ',', country))
}
