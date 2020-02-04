library(ma391chinn)
f = function(r){(1500-r)*(1+0.15*(r/100))}
df = function(r){fprime(f,r)}

r = seq(0,1000)
plot(r,f(r))
ans = bisection(df,0,600)
print(ans)
print(f(ans))
