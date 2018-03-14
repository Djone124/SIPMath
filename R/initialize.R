#### Initialize the Global variables for adding to the dataframe ####

initialize.SIP <- function(df_name,trials) {
  df_name <- data.frame(runif(1:trials))
}
test <- initialize.SIP(df,100)
head(test)
