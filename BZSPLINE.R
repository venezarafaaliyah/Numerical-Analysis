u <- as.matrix(seq(0,1,0.2));u
x <- as.matrix(c(1,5,9,12));x
y <- as.matrix(c(2,7,4,6));y
z <- as.matrix(c(3,8,10,11));z
n <- length(x)

A1 <- matrix(0,6,1);A1
A2 <- matrix(0,6,1);A2
A3 <- matrix(0,6,1);A3
B <- matrix(0,4,1);B
pu <- matrix(0,4,1);pu
pux <- matrix(0,4,1);pux
puy <- matrix(0,4,1);puy
puz <- matrix(0,4,1);puz
ux <- matrix(0,6,1);ux
uy <- matrix(0,6,1);uy
uz <- matrix(0,6,1);uz

for(i in 1:length(u)){
  for(j in 1:n){
    B[j,] = choose((n-1),(j-1))*(u[i, ]^(j-1))*((1-u[i,])^((n-1)-(j-1)))
    pu[j,] = B[j,]*x[j, ]
    A1[i,] = sum(pu)
    print(B)
  }
  print(sum(pu))
}
A1
for(i in 1:length(u)){
  for(j in 1:n){
    B[j,] = choose((n-1),(j-1))*(u[i, ]^(j-1))*((1-u[i,])^((n-1)-(j-1)))
    pu[j,] = B[j,]*y[j, ]
    A2[i,] = sum(pu)
  }
  print(sum(pu))
}
A2
for(i in 1:length(u)){
  for(j in 1:n){
    B[j,] = choose((n-1),(j-1))*(u[i, ]^(j-1))*((1-u[i,])^((n-1)-(j-1)))
    pu[j,] = B[j,]*z[j, ]
    A3[i,] = sum(pu)
  }
  print(sum(pu))
}
A3
plot(A1,A2,type="l")
persp(A1,A2,A3)

Ux <- for(i in 1:length(u)){
  for(j in 1:n){
    B[j,] = choose((n-1),(j-1))(u[i, ]^(j-1))((1-u[i,])^((n-1)-(j-1)))
    pu[j,] = B[j,]*x[j, ]
    ux[i,] = sum(pu)
  }
  print(sum(pu))
}

Uy <- for(i in 1:length(u)){
  for(j in 1:n){
    B[j,] = choose((n-1),(j-1))(u[i, ]^(j-1))((1-u[i,])^((n-1)-(j-1)))
    pu[j,] = B[j,]*y[j, ]
    uy[i,] = sum(pu)
  }
  print(sum(pu))
}

Uz <- for(i in 1:length(u)){
  for(j in 1:n){
    B[j,] = choose((n-1),(j-1))(u[i, ]^(j-1))((1-u[i,])^((n-1)-(j-1)))
    pu[j,] = B[j,]*z[j, ]
    uz[i,] = sum(pu)
  }
  print(sum(pu))
}

A <- data.frame(ux,uy,uz);A

plot(ux, uy, type = "l")
scatterplot3d(ux,uy,uz)
