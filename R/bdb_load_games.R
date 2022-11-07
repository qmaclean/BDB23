

bdb_load_games<-function(){
  
  require(tidyverse)
  
  games<-read.csv("data/src/games.csv") 
  
  
  return(games)
  
}
