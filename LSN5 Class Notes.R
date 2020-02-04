# multiply function by -1 because it is designed for minimization only
f = function(x){(x[1]*x[2]-2*x[1]-2*x[2]-x[1]^2-x[2]^2)*-1}
# give it a starting point
x = c(0,0)
# use optim function (part of base R package)
# (parameter aka starting point, function, method)
ans = optim(x, f, method = "BFGS")
# output
# $par is the maximizing point
# $value is the NEGATIVE of the actual max function value
# to use a value from this do ans$par or whatever value you want

print(ans$par)
print(ans$value)
print(ans$val)


## 