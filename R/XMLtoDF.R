XMLtoDF<- function (file_name){
  df<-xmlToDataFrame(file_name)
  df<-apply(df,1,function(x) as.numeric(strsplit(x,",")[[1]])) # df now has all slurps
  l<-xmlToList(file_name)
  new_names<-c()
  for( i in 1:length(l)){
    if(names(l)[i]=="SIP"){
      new_names<-c(new_names, l[[i]]$.attrs[[1]] )
    }
  }
  colnames(df)<-new_names
  return(df)
}