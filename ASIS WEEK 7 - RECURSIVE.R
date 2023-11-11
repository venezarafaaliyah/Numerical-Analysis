#Combination

factorial <- function(n){
  if(n==0 || n==1){
    return(1)
  }else{return(n*factorial(n-1))
    }
}

n=factorial(5);n

combination_5_2=factorial(5)/(factorial(2)*factorial(3))
combination_5_2

combination -> function(a,b){
  combination1=(factorial(a)/(factorial(b)*factorial(a-b)))
  combination2=(factorial)
}

binomial_coef <- function(n,r){
  if(n==r | r==0){return(1)
  }
  else{
    result<-binomial_coef(n-1,r-1)+binomial_coef(n-1,r)
    return(result)
  }
}
binomial_coef(5,2)

variance <- function(n,x){
  x <-(c(1,2,3,4,5,6,7,8,9,10))
  if(n==0||n==1){
    return(1)
  }else(
    result<-((n/(n+1))*variance(n)+(1/n)*(x[n+1]-mean(n+1))^2)
  )
}

variance(5,x)

x <-(c(3.66,3.55,3.12,3.72,3.75))

miu <- mean(x);miu
miu <- sum(x)/length(x);miu

var <- var(x);var
var <-(1/length(x))*sum((x-miu)^2)

n_miu <- function(miu,a,i){
  n=length(x)
new=(1/(n+i))*((n+i-1)*miu+a)
print(new)
}
A <- n_miu(miu, 3.69, 1);A
B <- n_miu(A,3.2,2);B

miu <- mean(x)
new<- function(m,a,i){
  n=length(x)
  new=(1/(n+i))*((n+i-1)*m+a)
}

A <- new(miu, 3.69, 1);A
B <- new(A,3.2,2);B
C <- new(B, 3.86,3);C

miu <- mean(x)
varians <- sum((x-miu)^2)/length(x);varians
new_var <- function(v,b,m,i){
  n=length(x)
  nv=((n+i-1)/(n+i))*v+((1/(n+i-1))*(b-m)^2)
  print(nv)
}
D <- new_var(varians, 3.69,A, 1);D
E <- new_var(D,3.2,B,2);E
G <- new_var(E,3.86,C,3);G
#======================DIGABUNG=============================
nmv <- function(a,m,v,i){
  n=length(x)
  new=(1/(n+i))*((n+i-1)*m+a)
  newv=((n+i-1)/(n+i))*v+(1/(n+i-1)*(a-new))^2
  A = matrix(0,1,2)
  A[1,1]=new
  A[1,2]=newv
  A
}

P <- nmv(3.69,miu,var,1);P
Q <- nmv(3.2,P[1,1],P[1,2],2);Q
R <- nmv(3.86,Q[1,1],Q[1,2],3);R

new_mean <- function(meanExisting, n, new_data){
  return(n/(n+1)*meanExisting+1/(n+1)*new_data)
}
new_mean(10,6,6,7)
