
#### Reads in Plays for BDB 23

bdb_load_plays<-function(){
  
  plays<-read.csv("data/plays.csv") %>%
    separate(.data$personnelO,c("RBs","TEs","WRs"),sep=",") %>%
    separate(.data$personnelD,c("DLs","LBs","DBs"),sep=",") %>%
    ##### Join on manual charted data ###
  
  return(plays)
}

