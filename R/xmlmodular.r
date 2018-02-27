library("XML")
library (lattice)
library(data.table)
setwd("C:/sim codes/R")
list.files()


# defining classes
Sipclass<-setClass("Sipclass",
                   slots=c(x="character",y="numeric"))

Slurpclass<-setClass("Slurpclass",
                     slots=c(att="character",siplist="list"))


# Making the Slurps

Read_XMLtoSLURP<-function(file_name){
  
      # Reading the XML file
      xml_read <- function(filename)
      {
                xmlfile=xmlParse(filename)
                class(xmlfile)
                xmltop = xmlRoot(xmlfile)
                class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
                l<-xmlToList(xmlfile)
                return(l)         
      }
      
      # Defining the Function to Fill teh Slurp with Sips
      setGeneric(name="Fill_Slurp",
                 def=function(Slurpclass){
                    standardGeneric("Fill_Slurp")
                  })
      suppressWarnings(
      setMethod(f="Fill_Slurp",
                signature="Slurpclass",
                definition=function(obj)
                {
                  l<-xml_read(file_name)
                  obj@att<-l$.attrs
                  for( i in 1:length(l)){
                    if(names(l)[i]=="SIP"){ # if it is of SIP type in xml
                      # getting and formatting the i'th Sip - as sipclass
                      sip_i<-Sipclass(x=l[i]$SIP$.attrs,
                                     y=as.numeric(strsplit(l[i]$SIP$text,",")[[1]]))
                      obj@siplist<-c(obj@siplist,sip_i)
                    }
                  }
                  return(obj)
                }))
      
      # Calling function to fill the slurp with Values
      slurp1<-Slurpclass(att="",siplist=list())
      return (Fill_Slurp(slurp1))
}

# Exporting to XML

write_SLURPtoXML<-function(slurp_Obj,target_filename){
      root=newXMLNode(name="SLURP",attrs=c(slurp_Obj@att))
      root
      for( i in 1:length(slurp_Obj@siplist)){
        #sipi<-newXMLNode(name="SIP",attrs=c(slurp1@siplist[[i]]@x),slurp1@siplist[[i]]@y,parent=root)
        sipi<-newXMLNode(name="SIP",attrs=c(slurp_Obj@siplist[[i]]@x),paste(slurp_Obj@siplist[[i]]@y,collapse=","),parent=root)
      }
      root
      saveXML(doc=root,file=target_filename)  
}

# Function to draw slurp data only into data table

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

# FUnction to vizualize the Slurp - Predefined viz

viz_SLURP_pairs<-function(file_name,cols){
      df<-XMLtoDF(file_name)
      pairs(df[,cols],col=c(3,4))
}

# Correlation Function

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
