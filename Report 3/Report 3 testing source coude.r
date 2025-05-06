macierz <- matrix(c(1,2,3,4,5,6,7,8,9,-5,10,4), nrow = 4)

print(macierz)



print(apply(macierz, 1, which.max))