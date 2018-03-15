

index <- 1:1000
normdist1 <- rnorm(1000,mean= 0, sd= 1)
normdist2 <- rnorm(1000,mean= 0, sd= 1)
normdist3 <- rnorm(1000,mean= 0, sd= 1)
normdist4 <- rnorm(1000,mean= 0, sd= 1)
normdist5 <- rnorm(1000,mean= 0, sd= 1)
test2.df <- data.frame(index,normdist1,normdist2,normdist3,normdist4,normdist5)

meta.df <- data.frame(c("Meta1","Meta2"),c(10,11),c(20,21),c(30,31),c(40,41),c(50,51))


CreateSLURP <- function(dataframe,filename,index=TRUE,provenance="",csvr="",average=FALSE,median=FALSE,meta=NULL) {
  #The CreateSLURP function will break the columns of a DataFrame into SIPS, by taking each column and pasting it into string.
  #The paste function will add all of the additional information to the string including necessary formatting.
  #The string will be saved to a variable to be added to the SLURP string, which has its own set of formatting.
  #The "write" function will save the string to a file in the current working directory with the name set by "filename".
  
  SIPS <- NULL      #SIPS is the variable to add all SIPS into 1 string. The format of the vector at the end is c( SIPS(1st SIP), SIPS(2nd SIP),... SIPS(n SIP) )
  metas <- NULL
  res <- ""            #res is the variable to take the vector of SIPS and create a single string from the vector of strings.
  res.meta <- ""
  if (index==TRUE) start<- 2 else start<- 1          #If there is an index, we dont want to add the index to the SLURP. Start at column 2 if there is an index
  
  metadata.function <- function(i) {                 #for loop iterrating over the number of columns:1 to n without an index, 2 to n with index
    for (j in 1:nrow(meta[i])) {
      metas <- c(metas,
                 paste(" ",meta[j,1],'="',meta[j,i],'"',collapse = "",sep = ""))                          #Paste metadata (in development)
      }
    for (metadata in metas) {
    res.meta <- paste(res.meta,metadata,sep = "")
    }
  return(res.meta)}

  for (i in start:ncol(dataframe)) {                 #for loop iterrating over the number of columns:1 to n without an index, 2 to n with index
    SIPS <- c(SIPS,
              paste( "<SIP name=",'"',colnames(dataframe[i]),'"',                                  #Paste the column name with SIP name
                     " count=",'"',length(dataframe[,i]),'"',                                      #Paste in the count of items in the column
                     " type=",'"',"CSV",'"',                                                       #Paste type with hardcoded default CSV
                     if (provenance!="") paste(" provenance=",'"',provenance,'"',sep = "")         #If Provenance is blank then skip, otherwise paste Provenance
                     else "",
                     metadata.function(i),                                                         #If there is metadata added, then use it by calling the metadata.function function
                     if (average==TRUE) paste(" average=",'"',mean(dataframe[,i]),'"')             #If average is true, take mean of column otherwise skip
                     else "",
                     if (median==TRUE) paste(" median=",'"',median(dataframe[,i]),'"')             #If median is true, take median of column otherwise skip
                     else "",
                     "> ",
                     paste( 
                       if (is.numeric(csvr)==TRUE) 
                         round(
                           dataframe[,i],                                                          #Paste the data from the current column
                           digits = as.numeric(csvr))                                              #Round by the CSVR argument
                       ,collapse = ",", sep = ", "),                                               #Separate the data with a comma
                     " </SIP>",                                                                    #End each string with the ending XML
                     "\n",                                                                         #At the end of the function, the write function will add each SIP to a new line with this
                     sep = "",                                                                     #
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

setwd("/Users/danny/GitHub/SIPMath")
CreateSLURP(test2.df,testdfxml21.xml,provenance = "Testing with 1000 values",csvr = 4,average = FALSE,median = FALSE,meta = meta.df)

