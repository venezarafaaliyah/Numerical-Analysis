x=seq(0,0.65,0.05);x

y=exp(-x)-x;y

plot(x,y, type="l")

A=data.frame(x,y);A

graph <- function(x){
  y=exp(-x)-x
  data.frame(x,y)
  plot(x,y,type="l")
}
graph(seq(0,0.65,0.05))
graph(seq(0.55,0.65,0.002))
graph(seq(0.55,0.60,0.0002))
graph <- function(x){
  y=sin (10*x) + cos (3*x);y
  data.frame(x,y)
  print(data.frame(x,y))
  plot(x,y,type="l")
}

graph(seq(0,5,0.5))
graph(seq(0.35,0.45,0.002))
graph(seq(0.362,0.364,0.0001))
graph(seq(0.3624,0.3625,0.000001))
