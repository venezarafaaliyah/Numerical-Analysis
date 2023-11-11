#add new mean and variance
x<-c(4,6,12,9,8);x

R_mean <- function(mean,a){
  for (i in 1:length(a)){
    mean = ((i-1)/i)*mean +1/i*a[i]
    print(mean)
  }}
R_mean(0,x)

R_var <- function(a){
  mean = 0
  var = 0
  for (i in 1:length(a)){
    mean = ((i-1)/i)*mean +(1/i)*a[i]
    var = ((i-1)/i)*var+(1/(i-1))*(a[i]-mean)^2
    if (var == "NaN" ){
      var = 0
    }
    else {
      var = var
    }
    print(c(mean,var))
  }}
R_var(x)

#monte carlo1

g <- function(x)sin(x)
a=0
b=pi
c=min(g(a:b));c
d=max(g(a:b));d
n <- 10^6
x <- runif(n)
k <- function(y)(g(a+(b-a)*y)-c)/(d-c)
integrate(g,0,pi)
I = (b-a)*(d-c)*sum(k(x))/n+c*(b-a);I


g <- function(x) exp(-3*x)
a=0
b=20
c=min(g(a:b));c
d=max(g(a:b));d
x <- runif(n)
k<- function(y) (g(a+(b-a)*y)-c)/(d-c)
integrate(g,0,20)
I=(b-a)*(d-c)*sum(k(x))/n+c*(b-a);I

  
g <- function(x) exp(-x^2/2)
a=50
b=50
c=min(g(a:b));c
d=max(g(a:b));d
n <- 10^6
x <- runif(n)
k <- function(y)(g(a+(b-a)*y)-c)/(d-c)
integrate(g,-20,-20)
I = (b-a)*(d-c)*sum(k(x))/n+c*(b-a);I
sqrt(2*pi)

#monte carlo2
g<-function(f,a,b){
  p = runif(1e6)
  q= a+p*(b-a)
  for(i in 1:100){
    mean = ((i+1)/i)*mean(r)+1/i*r
  }
  print(mean)
}

f <- function(x) exp(-x*3)
g(f,0,20)
f <- function(x) sin(x)
g(f,0,pi)
f <- function(x) exp(-(x^2)/2)
g(f,-20,-20)
