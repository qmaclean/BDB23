
#### Reads in Plays for BDB 23

bdb_load_plays<-function(){
  
  require(tidyverse)
  
  plays<-read.csv("data/src/plays.csv") #%>%
    #separate(.data$personnelO,c("RBs","TEs","WRs"),sep=",") %>%
    #separate(.data$personnelD,c("DLs","LBs","DBs"),sep=",")
    ##### Join on manual charted data ###
  
  return(plays)
}

#plays<-bdb_load_plays()

