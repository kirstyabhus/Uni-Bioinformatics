# Libraries that are essential for work with APIs in R
library(httr)
library(jsonlite)

# the url to access the API.
# the paste function is used to allow the url to be split on separate lines.
endpoint <- paste('https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;', 
'areaName=england&structure={"date":"date","newCases":"newCasesByPublishDate"}', sep="'")

# the endpoint url is used to call the API with the GET() function, to create a
# GET request. The returned data (response) is stored in response object.
httr::GET(
  url = endpoint,
  timeout(10)
) -> response

  
# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- jsonlite::fromJSON(json_text)

# The data frame containing the date and new cases are extracted into an object
covid_table <- data$data

# HIGHEST NUMBER OF NEW CASES
highest_case <- max(covid_table[1:500,"newCases"])

# POSITION OF HIGHEST NUMBER OF NEW CASES
highest_case_pos <- match(highest_case, covid_table[1:500,"newCases"])

# DATE OF HIGHEST NUMBER OF NEW CASES
print(covid_table[highest_case_pos,"date"])

# NUMBER OF NEW CASES IN LATEST ENTRY 
latest_day <- covid_table[1,"newCases"]

# NUMBER OF NEW CASES 300 days ago
day_300 <- covid_table[300,"newCases"]

# COMPARISON (THERE'S BEEN A DECREASE IN CASES NOW COMPARED TO 300 DAYS AGO)
latest_day - day_300
