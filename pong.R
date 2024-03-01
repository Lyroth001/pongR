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
paddles = tibble(id = c(1,2), x = c(1,1000), xend = c(1,1000), y = 10, yend = 20)
ball = tibble(x=500,y=15,xDirection = -10, yDirection = 0)
score = tibble(id = c(1,2), score = c(0,0))
game$display$set_caption("Click in to control, mouse and WASD")

readEvents <<- function(){
  # Gets events from pygame, uses that to move paddles/quit
  for(event in game$event$get()){
    if(event$type == game$KEYDOWN){
      print("here")
      if(event$key == game$K_w){
        #mov LHS up
        paddles <<- mutate(paddles, y=ifelse(id == 1, y+1, y))
        paddles <<- mutate(paddles, yend=ifelse(id == 1, yend+1, yend))
        return(T)
      }
      if(event$key == game$K_s){
        #mov LHS down
        paddles <<- mutate(paddles, y=ifelse(id == 1, y-1, y))
        paddles <<- mutate(paddles, yend=ifelse(id == 1, yend-1, yend))
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

moveBall = function (){
    ball <<- mutate(ball, x = x + ball$xDirection, y = y + ball$yDirection)
    if(ball$x > 1000){
        ball <<- mutate(ball, x = 500, y = 15, xDirection = -10, yDirection = 0)
        score <<- mutate(score, score = ifelse(id == 1, score+1, score))
    }
    if(ball$x < 0){
        ball <<- mutate(ball, x = 500, y = 15, xDirection = 10, yDirection = 0)
        score <<- mutate(score, score = ifelse(id == 2, score+1, score))
    }
    if(ball$y > 30){
        ball <<- mutate(ball, yDirection = -1)
    }
    if(ball$y < 0){
        ball <<- mutate(ball, yDirection = 1)
    }
  if((ball$x==0 & ball$y >= paddles$y[1] & ball$y <= paddles$yend[1])|(ball$x==1000 & ball$y >= paddles$y[2] & ball$y <= paddles$yend[2])){
    ball <<- mutate(ball, xDirection = xDirection*-1, yDirection = sample(c(-1,1,0),1))
  }
}

# ggplot(paddles, aes(x = x, y=y)) + geom_boxplot() + geom_point(ball, aes(x=x,y=y),color = "white")
# what i need is an open pyGame screen to manage inputs,and use a ggplot for output
# This will need to be in a while loop, displaying paddles as a bar chart with a ball as a datapoint

while(running){
  # brutilisation of ggplot to display the game
  # current implementation wont work live, youll need to look at the graphs afterwards lol
  # its basically a powerpoint
  screen = ggplot() + # moved data and aesthetics to respective geoms (preference, not necessary)
    geom_segment(data = paddles, aes(x = x, xend = xend, y = y, yend = yend), linewidth = 3) +
    geom_point(data = ball, aes(x = x, y = y), color = "white") +
    coord_cartesian(ylim = c(0, 30)) + # scale y axis
    theme_dark() +
    labs(title = "Pong", subtitle = paste("Player 1: ", score$score[1], "Player 2: ", score$score[2]))
  running = readEvents()
  moveBall()
  print(screen)
}


