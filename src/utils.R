last_date <- function() max(daily_reports$date)

last_confirmed <- function(country){
  daily_reports$confirmed[daily_reports$country == country &
                            daily_reports$date == last_date()]
  
}


lag_days <- function(country1, country2){
  first_day <- daily_reports$confirmed[daily_reports$country == country1] <= last_confirmed(country2)
  first_day <- which.min(first_day) - 1
} 

date_same_level <- function(country1, country2){
  daily_reports$date[daily_reports$country == country1][lag_days(country1, country2)]
}

