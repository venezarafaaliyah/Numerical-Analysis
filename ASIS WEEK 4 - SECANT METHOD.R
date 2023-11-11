#SECANT METHOD 
f <- function(x) x-0.8-0.2*sin(x)
n=10
m=matrix(0,2,n)
x=matrix(0,2,n)
ea=matrix(0,2,n)
i=1
es=0.0005

graphic(f,seq(0,0.65,0.05))
NT <- function (f,xa,xb){
  x[1]=xb
  x[2]=xb-((f(xb)*(xa-xb))/(f(xa)-f(xb)))
  ea[i]=abs(((x[i]-xa)/x[i])*100)
  print(c(x[i],ea[i]))
  for(i in 1:n){
    i=i+1
    x[i+1]=x[i]-((f(x[i])*(x[i-1]-x[i]))/(f(x[i-1])-f(x[i])))
    ea[i]=abs(((x[i]-x[i-1])/x[i])*100)
    print(c(x[i],ea[i]))
    if(ea[i]<es){
      break}
  }
}

NT(f,0,0.55)

#SECANT METHOD 
f <- function(x) x-x*cos(x)
n=10
m=matrix(0,2,n)
x=matrix(0,2,n)
ea=matrix(0,2,n)
i=1
es=0.0005

graphic(f,seq(0,0.65,0.05))
NT <- function (f,xa,xb){
  x[1]=xb
  x[2]=xb-((f(xb)*(xa-xb))/(f(xa)-f(xb)))
  ea[i]=abs(((x[i]-xa)/x[i])*100)
  print(c(x[i],ea[i]))
  for(i in 1:n){
    i=i+1
    x[i+1]=x[i]-((f(x[i])*(x[i-1]-x[i]))/(f(x[i-1])-f(x[i])))
    ea[i]=abs(((x[i]-x[i-1])/x[i])*100)
    print(c(x[i],ea[i]))
    if(ea[i]<es){
      break}
  }
}

NT(f,6.8,6.85)
