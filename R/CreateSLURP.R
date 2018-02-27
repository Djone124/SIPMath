index <- 1:1000
normdist1 <- rnorm(1000,mean= 0, sd= 1)
normdist2 <- rnorm(1000,mean= 0, sd= 1)
normdist3 <- rnorm(1000,mean= 0, sd= 1)
normdist4 <- rnorm(1000,mean= 0, sd= 1)
normdist5 <- rnorm(1000,mean= 0, sd= 1)
test2.df <- data.frame(index,normdist1,normdist2,normdist3,normdist4,normdist5)


CreateSLURP <- function(dataframe,filename,provenance,cvsr=2.0,average=FALSE,median=FALSE) {
  SIPS <- NULL
  res=""
  for (index in 1:ncol(dataframe)) {
    SIPS <- c(SIPS,
              paste( "<SIP name=",'"',colnames(dataframe[index]),'"',
                     " count=",'"',length(dataframe[,index]),'"',
                     " type=",'"',"CSV",'"',
                     " provenance=",'"',provenance,'"',
                     if (average==TRUE) paste(" average=",'"',mean(dataframe[,index]),'"') 
                     else "",
                     if (median==TRUE) paste(" median=",'"',median(dataframe[,index]),'"')
                     else "",
                     "> ",
                     paste( 
                       round(
                         dataframe[,index],
                         digits = cvsr) ,
                       collapse = ",", sep = ", "),   # This is the line which takes the data from that column
                     " </SIP>",
                     "\n",
                     sep = "",
                     collapse = "") )
  }
  for (items in SIPS) {
    res <- paste(res,items, sep = "")
  }
  write(
    paste( "<SLURP name=",'"',deparse(substitute(dataframe)),'"', 
           " provenance=",'"',provenance,'"',
           " count=",'"',nrow(dataframe),'"',
           "> ",
           "\n",                                                      # 
           res,
           "</SLURP>",
           "\n",
           sep = "",
           collapse = ""),
    deparse(substitute(filename)),sep = "\n") }

CreateSLURP(test2.df,testdfxml17.xml,provenance = "Testing with 1000 values",cvsr = 4,average = TRUE,median = TRUE)

