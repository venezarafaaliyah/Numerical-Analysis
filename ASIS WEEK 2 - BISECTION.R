#BISECTION
f=function(x)exp(-x)-x

graphic <- function(f,x){
  my.frame = data.frame(x,f(x))
  print(my.frame)
  plot(x,f(x), type="l")
}
graphic(f,seq(0,0.65,0.05))

bisectionroot = function(f, xmin, xmax, tol=1e-4){
  a= xmin; b= xmax
  #check inputs
  if (a >= b){
    cat("error: xmin > xmax \n")
    return(NULL)
  }
  if (f(a) == 0){
    return(a)
  }
  else if (f(b) == 0){
    return(b)
  }
  else if (f(a)*f(b) > 0){
    cat("error: f(xmin) and f(xmax) of same sign \n")
    return(NULL)
  }
  #if inputs OK, converge to root
  iter = 0
  while ((b-a) > tol){
    c= (a+b)/2
    if (f(c)==0){
      return (c)
    }
    else if (f(a)*f(c) <0){
      b =c 
    }
    else {
      a = c
    }
    iter = iter +1
  }
  return (c((a+b)/2, iter, (b-a))) #root, iterations, precision
}

bisectionroot(f, 0.55, 0.6)

# EXCERCISE BISECTION A
f=function(x) exp(-2*x)-sin(x)

graphic <- function(f,x){
  my.frame = data.frame(x,f(x))
  print(my.frame)
  plot(x,f(x), type="l")
}
graphic(f,seq(0,0.65,0.05))

bisectionroot = function(f, xmin, xmax, tol=1e-4){
  a= xmin; b= xmax
  #check inputs
  if (a >= b){
    cat("error: xmin > xmax \n")
    return(NULL)
  }
  if (f(a) == 0){
    return(a)
  }
  else if (f(b) == 0){
    return(b)
  }
  else if (f(a)*f(b) > 0){
    cat("error: f(xmin) and f(xmax) of same sign \n")
    return(NULL)
  }
  #if inputs OK, converge to root
  iter = 0
  while ((b-a) > tol){
    c= (a+b)/2
    if (f(c)==0){
      return (c)
    }
    else if (f(a)*f(c) <0){
      b =c 
    }
    else {
      a = c
    }
    iter = iter +1
  }
  return (c((a+b)/2, iter, (b-a))) #root, iterations, precision
}

bisectionroot(f,0.4,0.45)

# EXCERCISE BISECTION B
f=function(x) exp(x)-x^2+3*x-2

graphic <- function(f,x){
  my.frame = data.frame(x,f(x))
  print(my.frame)
  plot(x,f(x), type="l")
}
graphic(f,seq(0,0.65,0.05))

bisectionroot = function(f, xmin, xmax, tol=1e-4){
  a= xmin; b= xmax
  #check inputs
  if (a >= b){
    cat("error: xmin > xmax \n")
    return(NULL)
  }
  if (f(a) == 0){
    return(a)
  }
  else if (f(b) == 0){
    return(b)
  }
  else if (f(a)*f(b) > 0){
    cat("error: f(xmin) and f(xmax) of same sign \n")
    return(NULL)
  }
  #if inputs OK, converge to root
  iter = 0
  while ((b-a) > tol){
    c= (a+b)/2
    if (f(c)==0){
      return (c)
    }
    else if (f(a)*f(c) <0){
      b =c 
    }
    else {
      a = c
    }
    iter = iter +1
  }
  return (c((a+b)/2, iter, (b-a))) #root, iterations, precision
}

bisectionroot(f,0.25,0.3)

# EXCERCISE BISECTION C
f=function(x) x-2^(-x)

graphic <- function(f,x){
  my.frame = data.frame(x,f(x))
  print(my.frame)
  plot(x,f(x), type="l")
}
graphic(f,seq(0,0.65,0.05))

bisectionroot = function(f, xmin, xmax, tol=1e-4){
  a= xmin; b= xmax
  #check inputs
  if (a >= b){
    cat("error: xmin > xmax \n")
    return(NULL)
  }
  if (f(a) == 0){
    return(a)
  }
  else if (f(b) == 0){
    return(b)
  }
  else if (f(a)*f(b) > 0){
    cat("error: f(xmin) and f(xmax) of same sign \n")
    return(NULL)
  }
  #if inputs OK, converge to root
  iter = 0
  while ((b-a) > tol){
    c= (a+b)/2
    if (f(c)==0){
      return (c)
    }
    else if (f(a)*f(c) <0){
      b =c 
    }
    else {
      a = c
    }
    iter = iter +1
  }
  return (c((a+b)/2, iter, (b-a))) #root, iterations, precision
}

bisectionroot(f,0.6,0.65)

#EA
f=function(x)exp(-x)-x
m=matrix(0,2,5);m
ea=matrix(0,2,5)
es=0.005


bisection <- function(a,b){
  i=0
  for(i in 1:10){
    m[i]=(a+b)/2
    if(f(m[i])==0){
      return(m[i])}
    else if(f(a)*f(m[i])<0){
      b = m[i]}
    else{
      a = m[i]}
    ea=abs(((m[i]-m[i-1])/m[i])*100)
    print(c(a,b,f(a),f(b),m[i],ea))
    if(ea<es){
      break
    }
    i = i+1
  }
}

bisection(0.55,0.6)
