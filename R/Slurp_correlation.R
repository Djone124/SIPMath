Slurp_correlation<-function(file_name,coly,colx){
  #   file_name="thefile.xml"
  df<-XMLtoDF(file_name)
  linear_model<-lm(df[,coly]~df[,colx])
  
  if(length(colx)==1){
    par( mfrow = c(1,1))
    plot(df[,coly]~df[,colx],xlab=colx,ylab=coly,col=4)
    abline(lm(df[,coly]~df[,colx]),col=2,lwd=2)
  }
  else{
    no<-ceiling((length(colx))/2)
    par( mfrow = c(2,ceiling((length(colx)+1)/2)))
    sapply(colx,function(x){
      plot(df[,coly]~df[,x],xlab=x,ylab=coly,col=4)
      abline(lm(df[,coly]~df[,x]),col=2,lwd=2)
    })
  }
  return(summary(linear_model))
}