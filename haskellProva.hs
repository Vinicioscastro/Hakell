salary::Int->Float
salary 1 = 772.25
salary 2 = 2375.0
salary 3 = 1778.5
salary 4 = 6520.0
salary 5 = 3447.35
salary 6 = 5225.75
salary 7 = 8932.0
salary 8 = 648.5
salary 9 = 1982.4
salary 10 = 2557.45
salary _ = 0

sumList::(Int->Float)->Int->Float
sumList func n
  |func n == 0 = 0
  |otherwise = (func n) + (sumList func (n+1))

counterWithFilter::(Int->Float)->(Float->Bool)->Int->Int
counterWithFilter func test n
  |func n == 0 = 0
  |test (func n) = 1 + counterWithFilter func test (n+1)
  |otherwise = counterWithFilter func test (n+1)

closeToValue::(Int->Float)->Int->Float->Float->Float
closeToValue func n val closest
  |func n == 0 = closest
  | abs (val - (func n)) < abs (val-closest) = closeToValue func (n+1) val (func n)
  |otherwise = closeToValue func (n+1) val closest

calculatePensionTax::Float->Float
calculatePensionTax n
  |n >= 6433.57 = n * 0.22
  |n >= 3305.23 = n * 0.14
  |n >= 2203.49 = n * 0.12
  |n >= 1100.01 = n * 0.09
  |otherwise = 0

sumMasckedList::(Int->Float)->(Float->Float)->Int->Float
sumMasckedList func mask n
  |func n == 0 = 0
  |otherwise = mask (func n) + (sumMasckedList func mask (n+1))

monthlyExpense::Float
monthlyExpense = sumList salary 1

countSalaryAbove::Float->Int
countSalaryAbove n = counterWithFilter salary (>n) 1

salariesCloseTo::Float->Float
salariesCloseTo n = closeToValue salary 1 n (salary 1)

totalPensionExpenditure::Float
totalPensionExpenditure = sumMasckedList salary calculatePensionTax 1

---------------------------------------------------

salary::Int->Float
salary 1 = 772.25
salary 2 = 2375.0
salary 3 = 1778.5
salary 4 = 6520.0
salary 5 = 3447.35
salary 6 = 5225.75
salary 7 = 8932.0
salary 8 = 648.5
salary 9 = 1982.4
salary 10 = 2557.45
salary _ = 0

monthlyExpense::(Int -> Float)->Float
monthlyExpense func = sumSalary func 1

sumSalary::(Int -> Float)->Int->Float
sumSalary func index 
  |func index == 0 = 0
  |otherwise = (func index) + (sumSalary func (index+1))

main = print $ monthlyExpense salary




maispro :: Float->Int ->Float
maispro valor n
    |x == 0 = 0
    |x > 0 = min (abs (salario n - valor)) (maispro (valor) (n-1))
    |otherwise = 0
    
--main = print $ maispro 500 10



