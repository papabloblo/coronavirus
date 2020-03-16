last_date <- function() max(daily_reports$date)

last_confirmed <- function(country, date_max = last_date()){
  daily_reports$confirmed[daily_reports$country == country &
                            daily_reports$date == date_max]
  
}


lag_days <- function(country1, country2, date_max = last_date()){
  first_day <- daily_reports$confirmed[daily_reports$country == country1] <= last_confirmed(country2, date_max)
  first_day <- which.min(first_day) - 1
} 

date_same_level <- function(country1, country2){
  daily_reports$date[daily_reports$country == country1][lag_days(country1, country2)]
}

