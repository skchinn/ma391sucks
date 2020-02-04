Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}

# want to max Rev x[1],x[2]
Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                     
                     6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
x=c(50000,50000)
# added ans= in after finding the answer
ans = optim(x,Rev,method = "L-BFGS-B")

#to find how many whales to harvest
Blue(ans$par)
Fin(ans$par)

## Sensitivity Analysis
# inside the function create the function that is getting optimized
# change 0.08 to r2
R2 = function(r2){
  fr2=function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                     
                     6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x=c(50000,50000)
  ans=optim(x,Rev,method="L-BFGS-B")
  return(ans)
}
# check the function by doing...
R2(0.08)

r = seq(0.06,0.1,0.01)
ans.x1=0
ans.x2=0
ans.rev=0
for (i in 1:length(r)){
  ans = R2(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.rev[i] = -ans$value
}
result=data.frame(growth_rate=r,x1=ans.x1,x2=ans.x2,revenue=ans.rev
                )
print(result)
