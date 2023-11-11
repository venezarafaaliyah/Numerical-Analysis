#NEWTON RAPHSON 
f1 <- function(x)(x-2)^2-log(x)
f2 <- function(x) 2*x-4-(1/x)
n=10
m=matrix(0,2,n)
x=matrix(0,2,n)
ea=matrix(0,2,n)
i=1
es=0.0005

NT <- function (f1,f2,xa){
  x[i]=xa-(f1(xa)/f2(xa))
  ea[i]=abs(((x[i]-xa)/x[i])*100)
  print(c(x[i],ea[i]))
  for(i in 1:n){
    i=i+1
    x[i]=x[i-1]-(f1(x[i-1])/f2(x[i-1]))
    ea[i]=abs(((x[i]-x[i-1])/x[i])*100)
    print(c(x[i],ea[i]))
    if(ea[i]<es){
      break}
  }
}

NT(f1,f2,1.4)
