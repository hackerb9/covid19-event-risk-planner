#Sys.setenv(PATH = with_path('/projects/covid19/bin', Sys.getenv("PATH")))


get_token()

args = commandArgs(trailingOnly=TRUE)
current_time <- args[1]
print(current_time)

getData <- function() {
  dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  data <- read.csv( dataurl, stringsAsFactors = FALSE) %>% mutate(date = as_date(date))
  county <<- st_read("map_data/tl_2017_us_county.geojson") 
  stateline <<- st_read("map_data/tl_2017_us_state.geojson")
  pop <- read.csv("map_data/county-population.csv", stringsAsFactors = FALSE)
  cur_date <- ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- ymd(cur_date) - 14
  data_cur <- data %>% filter(date == cur_date) %>% mutate(fips = case_when(
     county == "New York City" ~ 99999,
     TRUE ~ as.numeric(fips)
    )) %>%
      select(c(fips, cases, deaths))
  data_past <- data %>%
    filter(date == past_date)  %>% mutate(fips = case_when(
     county == "New York City" ~ 99999,
     TRUE ~ as.numeric(fips)
    )) %>%
    select(fips = fips, cases_past = cases)
  data_join <<- data_cur %>%
    inner_join(data_past, by = "fips") %>%
    inner_join(pop, by = "fips") 
  pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99" , "No or missing data")
}



# Calculate risk
calc_risk <- function(I, g, pop) {
  p_I <- I / pop
  r <- 1 - (1 - p_I)**g
  return(round(r*100, 1))
}


getData()


