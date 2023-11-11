x=c(1,5,9);x
y=c(9,3,5);y
u=seq(0,1,0.05);u

Bezier_spline <- function(n,k){
  for(k in 1:length(u)){
    for(j in 1:length(x)){
        b=combn((n-1),(k-1))*(u^(k-1))*((1-u)^(n-(k-1)))
        bs=b*x
    }
  }
  print(b)
  print(bs)
}
Bezier_spline()

