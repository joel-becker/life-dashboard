#------------------------------------------------------------------------------#
# Functions needed inside app.R
# Author: Joel Becker

# Notes:
#
#------------------------------------------------------------------------------#


weighted_average <- function(xs, weights) {
    # returns weighted average for given weights
    sum(xs*weights) / sum(weights)
}


rolling_weighted_average <- function(xs, date, target_date, epsilon = 0.0035) {
    # returns weights for exponential moving average
    time_diff <- as.numeric(abs(difftime(target_date, date, units = "day")))
    weights <- exp(-epsilon*time_diff)
    
    weighted_average(xs, weights)
}

generate_lags <- function(var, n=10){
  # TODO: get this working with multiple variables at once
  var <- enquo(var)
  
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s lag %02d", quo_text(var), indices))
  
}

semipseudolog_trans <- function(base) {
  scales::trans_new(
    name      = 'pseudo log10',
    transform = function(x) asinh(x/2)/log(base),
    inverse   = function(y) 2 * sinh(y * log(base)),
    domain    = c(-Inf,Inf))
}

#custom_date_axis <- function(data) {
#  interval_length <- interval(ymd(min(data$date)), ymd(max(data$date)))
#  interval_duration_months <- interval_length %/% months(1)
#  
#  # how long is between approximately 6 break points?
#  months_between_breaks <- ceiling(interval_duration_months / 6)
#  
#  first_date_month <- month(min(data$date))
#  first_date_year  <- year(min(data$date))
#  first_date       <- ymd(paste0(first_date_year, "-", first_date_month, "-01"))
#  last_date_month  <- month(max(data$date))
#  last_date_year   <- year(max(data$date))
#  last_date        <- ymd(paste0(last_date_year, "-", last_date_month, "-01"))
#  
#  breaks <- seq(
#    first_date,
#    last_date,
#    by = paste0(months_between_breaks, " month")
#    #length.out = 8
#  )
#  
#  interval_endseq_to_lastdate <- interval(
#    max(breaks), last_date
#    ) %/% months(1)
#  
#  breaks <- breaks + months(interval_endseq_to_lastdate)
#  
#  labels = c(
#    paste0(c(
#      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
#      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
#      " '21")
#  )
#  
#  return(breaks)
#}