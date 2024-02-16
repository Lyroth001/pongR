#implementation of a simple pong game using ggplot and pygame
#nb - this may work in RStudio, it definetly wont work in posit cloud, as it requires creating a window
#this will work in Dataspell. Use that one. Its free for students, or using the early access one
library(reticulate)
library(ggplot2)
library(dplyr)
# run the below on first time use
#py_install("pygame")
use_virtualenv("r-reticulate")
game = import("pygame")
size = list(300,300)
count = 0
running = TRUE
game$init()
myScr = game$display$set_mode(size)
paddles = tibble(id = c(1,2),x = c(1,1000), y = c(30,30))
ball = tibble(x=500,y=15)
game$display$set_caption("Click in to control, mouse and WASD")

readEvents <<- function(){
  for(event in game$event$get()){
    if(event$type == game$KEYDOWN){
      print("here")
      if(event$key == game$K_w){
        #mov LHS up
        paddles <<- mutate(paddles, y=ifelse(id == 1, y = y+1, y))
        return(T)
      }
      if(event$key == game$K_s){
        #mov LHS down
        paddles <<- mutate(paddles, y=ifelse(id == 1, y = y-1, y))
        return(T)
      }
      if(event$key == game$K_e){
        game$quit()
        return(F)
      }
    }
    if(event$type == game$QUIT){
      game$quit()
      return(F)
    }
  }
  return(T)
}



# ggplot(paddles, aes(x = x, y=y)) + geom_boxplot() + geom_point(ball, aes(x=x,y=y),color = "white")
# what i need is an open pyGame screen to manage inputs,and use a ggplot for output
# This will need to be in a while loop, displaying paddles as a bar chart with a ball as a datapoint

while(running){
  # brutilisation of ggplot to display the game
  # current implementation wont work live, youll need to look at the graphs afterwards lol
  # its basically a powerpoint
  ggplot(data = paddles, aes(x = factor(id), y = y)) +
    geom_boxplot()+
    geom_point(aes(x = ball$x, y = ball$y), color = "white") +
    expand_limits(x = c(-3,3),y = c(-20,20)) #sets 'screen' size
  + labs(title = "Pong", x = "Player")
  running = readEvents()
}
paddles


#boxplot using ggplot with paddles as x and y and one point for the ball
#ar(aes(x = x, y = y), stat = "identity")