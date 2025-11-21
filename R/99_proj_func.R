#Function for date reformatting
reformat_dates <- function(date_char){
  date_split <- str_split_fixed(date_char, "/", n=3)

  # year takes the last two characters and combines them with 19, despite its length
  year_suffix <- substr(date_split[,3],
                        nchar(date_split[,3])-1,
                        nchar(date_split[,3]))

  #return (year_suffix)

  is(as.numeric(year_suffix) > 8)

  #Figures out the century
  year <- case_when(as.numeric(year_suffix) > 8 ~ str_c("19", year_suffix),
                    as.numeric(year_suffix) <= 8 ~ str_c("20", year_suffix))

  #return (year)

  # Month is the first element in the date_split matrix
  month <- date_split[,1]
  # Day is the second element in the date_split matrix
  day <- date_split[,2]
  # Combining the String to a date format this time in DD/MM/YY
  date_str_out <- str_c(day, "-", month, "-", year)
  # Returns as a Date.charactor in the format YY/MM/DD
  return (as.Date.character(date_str_out, "%d-%m-%Y"))
}
