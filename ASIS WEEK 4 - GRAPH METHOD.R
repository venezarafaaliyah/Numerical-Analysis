x=seq(0,10,1);x
a=1/2*x+3;a
b=1/6*x+17/3;b
A=data.frame(x,a,b);A
library(ggplot2)
plot(x,a, type="l")

x <- seq(0,20, by = 1);x
y = ((1/2)*x)+3 ;y
z = (34/6) + ((1/6)*x) ;z

plot(x,y, type="l")
lines(x,z, type="l")
a =data.frame(x,y,z);a

graph(seq(0,20, by = 1))
graph <- function(x){
  y = (1/2)*x+3
  z = 34/6 + (1/6)*x
  data.frame(x,y,z)
  print(data.frame(x,y,z))
  plot(x,y, type="l")
  lines(x,z, type="l")
}
graph(seq(0,20, by = 1))
  
