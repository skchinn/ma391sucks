## Chapter 1 Example Problem (P. 4)
#  The Pig Problem

## Bisection

bisection = function(f,a,b,tol=0.0001){
  if (f(a)*f(b) > 0){
    return ("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while (abs(f(middle))>tol){
      middle = (a+b)/2
      if (f(middle)*f(a)>0) (a = middle)
      else (b = middle)
      x=middle
      y=f(middle)
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return (middle)
  }
}

## F-Prime
fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

## dProfit
dProfit = function(x){fprime(profit,x)}

## Define your model as a function
profit = function(x){return((0.65-0.01*x)*(200+5*x)-0.45*x)}

## List the domain you want displayed on the graph
x = seq(0,20,1)

## Plot the model
plot(x,profit(x),type="o")

## Visually find where the max could be. Say around 8. Plug in 8 to our model to estimate the maximum profit.
print(profit(8))

## Solve the problem
## dProfit solves Profit' for 0
dProfit = function(x){fprime(profit,x)}
## Bisection applies the bisection method
ans = bisection(dProfit,0,20)
print(ans)

## Plug ans back into objective function to find maximum function
print(profit(ans))

## Sensitivity Analysis
## Concerned with assumption on price of pork decreasing 0.01 per day
## Making a for statement to observe sensitivity of the variable
ans.time=0
ans.prof=0
r = seq(0.008,0.012,0.001)
for (i in 1:length(r)){
  ## Change 0.01 to the variable r
  profit = function(x){return((0.65-r*x)*(200+5*x)-0.45*x)}
  dProfit = function(x){fprime(profit,x)}
  ans.time[i] = bisection(dProfit,0,20)
  ans.prof[i] = profit(ans.time[i])
}
  result = data.frame(price = r, time = ans.time, profit = ans.prof)
  print(result)
  