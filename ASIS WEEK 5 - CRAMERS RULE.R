A <- matrix(c(0.3, 0.52, 1, 0.5, 1, 1.9, 0.1, 0.3, 0.5), byrow=T nrow=T)
b <- t(c(-0.01, 0.67, -0.44 ));b

Ai<- list

for(i in 1:3){
  A_i<- A
  A_i[,i] <- b
  Ai{[paste0("A",i)]} <- A_i
}
Ai

x<- list()

for(i in 1:3){
  x[[paste0("x",i)]] <- det(Ai[[paste0("A",i)]])/detach()
}
det_A
x