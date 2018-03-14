

index <- 1:1000
normdist1 <- rnorm(1000,mean= 0, sd= 1)
normdist2 <- rnorm(1000,mean= 0, sd= 1)
normdist3 <- rnorm(1000,mean= 0, sd= 1)
normdist4 <- rnorm(1000,mean= 0, sd= 1)
normdist5 <- rnorm(1000,mean= 0, sd= 1)
test2.df <- data.frame(index,normdist1,normdist2,normdist3,normdist4,normdist5)




CreateSLURP <- function(dataframe,filename,index=TRUE,provenance="",cvsr=2.0,average=FALSE,median=FALSE,meta=NULL) {
  SIPS <- NULL
  res=""
  if (index==TRUE) start<- 2 else start<- 1
  for (i in start:ncol(dataframe)) {
    SIPS <- c(SIPS,
              paste( "<SIP name=",'"',colnames(dataframe[i]),'"',
                     " count=",'"',length(dataframe[,i]),'"',
                     " type=",'"',"CSV",'"',
                     if (provenance!="") paste(" provenance=",'"',provenance,'"')
                     else "",
                     for (paste(" ",meta[,1],'="',meta[,i],'"'),
                     
                     if (average==TRUE) paste(" average=",'"',mean(dataframe[,i]),'"') 
                     else "",
                     if (median==TRUE) paste(" median=",'"',median(dataframe[,i]),'"')
                     else "",
                     "> ",
                     paste( 
                       round(
                         dataframe[,i],
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
           if (index==TRUE) paste(" count=",'"',ncol(dataframe)-1,'"')
           else paste(" count=",'"',ncol(dataframe),'"'),
           "> ",
           "\n",                                                      # 
           res,
           "</SLURP>",
           "\n",
           sep = "",
           collapse = ""),
    deparse(substitute(filename)),sep = "\n") }

CreateSLURP(test2.df,testdfxml17.xml,meta=meta.df,provenance = "Testing with 1000 values",cvsr = 4,average = FALSE,median = FALSE)

