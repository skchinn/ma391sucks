library(MASS);library(NlcOptim)

### 1a ###
obj = function(x){x[1]^2+x[2]^2}
con = function(x){
  f = NULL
  f = rbind(f,x[1]+2*x[2]-5)
  return(list(ceq=f,c=NULL))
}
solnl(x0,objfun = obj,confun = con)

X = list(x=seq(0,500),y=seq(0,500))
Z = Outer(obj,X)
contour(X$x,X$y,Z)
abline(h=0,col="red",lwd=3)
abline(v=0,col="red",lwd=3)
A = matrix(c(1,2,1,0,0,1),nrow=3,byrow=T)
B = matrix(c(5,0,0),nrow=3)
ans = solnl(x0,objfun = obj,A=A,B=B)
print(ans)
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)

### 1b ###
obj = function(x){(x[1]^2-x[2]^2)*-1}
con = function(x){
  f = NULL
  f = rbind(f,-x[1]^2+2*x[2])
  return(list(ceq=f,c=NULL))
}
solnl(x,objfun = obj,confun = con)

### 1c ###
obj = function(x){E^((-x[1]*x[2])/4)}
con = function(x){
  f = NULL
  f = rbind(f,x[1]^2+x[2]^2-1)
  return(list(ceq=f,c=NULL))
}
solnl(x,objfun = obj,confun = con)

### 1d ###
obj = function(x){(x[1]^2+x[2]^2+x[3]^2)*-1}

x0 = c(1,1,1)
Aeq = matrix(c(1,0,2,1,1,0), nrow=2, byrow=TRUE)
Beq = matrix(c(6,12))
A=matrix(c(-1,0,0,0,-1,0,0,0,-1), nrow=3, byrow=T)
B=matrix(c(0,0,0))
solnl(x0,objfun = obj,Aeq = Aeq,Beq = Beq,A=A,B=B)


con = function(x){
  f = NULL
  f = rbind(f, x[1]+2*x[3]-6)
  f = rbind(f,x[1]+x[2]-12)
  return(list(ceq=f,c=NULL))
}
solnl(x,objfun = obj,confun = con)
