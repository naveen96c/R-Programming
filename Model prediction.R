#-------------Question 2------------------#
library(lpSolveAPI)
Y <- make.lp(0, 9)
lp.control(Y, sense= "maximize") 

set.objfn(Y, c(25, 10, 5, 21, 6, 1, 25, 10, 5))

add.constraint(Y, c(1,1,1,0,0,0,0,0,0), "<=", 4800)
add.constraint(Y, c(0,0,0,1,1,1,0,0,0), "<=", 3000)
add.constraint(Y, c(0,0,0,0,0,0,1,1,1), "<=", 3500)
add.constraint(Y, c(0.45,-0.55,-0.55,0,0,0,0,0,0),">=",0)
add.constraint(Y, c(-0.3,0.7,-0.3,0,0,0,0,0,0),">=",0)
add.constraint(Y, c(0,0,0,0.55,-0.45,-0.45,0,0,0),">=",0)
add.constraint(Y, c(0,0,0,-0.4,0.6,-0.4,0,0,0),">=",0)
add.constraint(Y, c(0,0,0,0,0,0,0.7,-0.3,-0.3),">=",0)
add.constraint(Y, c(0,0,0,0,0,0,-0.5,0.5,-0.5),">=",0)

#LINEAR PROGRAMMING model Results:


Row_Names <- c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9")
Col_Names <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9")
dimnames(Y) <- list(Row_Names, Col_Names)
solve(Y)
get.objective(Y)
get.variables(Y)
get.constraints(Y)


#---------question 3------------#
#player 1
model_LP <- make.lp(0, 8)
lp.control(model_LP, sense= "maximize")  

set.objfn(model_LP, c(0,0,0,0,0,0,0,1))

add.constraint(model_LP, c(-1,-2,-1,0,-1,-2,-1,1), "<=", 0)
add.constraint(model_LP, c(0,-1,-2,-2,-2,-1,0,1), "<=", 0)
add.constraint(model_LP, c(0,0,-2,-4,-2,0,0,1), "<=", 0)
add.constraint(model_LP, c(0,-1,-2,-2,-2,1,0,1), "<=", 0)
add.constraint(model_LP, c(-1,-2,-1,0,-1,-2,-1,1), "<=", 0)
add.constraint(model_LP, c(1,1,1,1,1,1,1,0), "=", 1)

set.bounds(model_LP, lower = c(0, 0, 0,0,0,0,0, -Inf))
Row_Names <- c("R1", "R2", "R3", "R4", "R5", "R6")
Col_Names <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7","v")
dimnames(model_LP) <- list(Row_Names, Col_Names)
solve(model_LP)
model_LP
get.objective(model_LP)
get.variables(model_LP)
get.constraints(model_LP)


# For Player2
x <- make.lp(0, 6)
lp.control(x, sense= "minimize") 

set.objfn(x, c(0, 0, 0,0,0, 1))

add.constraint(x, c(-1, 0,0,0,-1, 1), ">=",0)
add.constraint(x, c(-2, -1,0,-1,-2, 1), ">=",0)
add.constraint(x, c(-1, -2,-2,-2,-1, 1), ">=",0)
add.constraint(x, c(0, -2,-4,-2,0, 1), ">=",0)
add.constraint(x, c(-1, -2,-2,-2,-1, 1), ">=",0)
add.constraint(x, c(-2, -1,0,-1,-2, 1), ">=",0)
add.constraint(x, c(-1, 0,0,0,-1, 1), ">=",0)
add.constraint(x, c(1,1,1,1,1,0), "=", 1)
set.bounds(x, lower = c(0, 0, 0,0,0, -Inf))

Row_Names <- c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8")
Col_Names <- c("c1", "c2", "c3", "c4", "c5","v")
dimnames(x) <- list(Row_Names, Col_Names)
solve(x)
x
get.objective(x)
get.variables(x)
get.constraints(x)


