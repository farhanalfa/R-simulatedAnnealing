#========================= MATHEMATIC FUNCTION =========================

fitness    <- function(x1,x2){
  return(-((sin(x1)*cos(x2)) + ((4/5)*(exp(1-sqrt((x1^2)+(x2^2)))))))
}

#==================== SIMULATED ANNEALING FUNCTION =====================

simulatedAnn <- function(x1, x2, temp, finalTemp, coolingRate){

  currentSt_x1 = x1
  currentSt_x2 = x2
  currentCost  = fitness(x1,x2)

  bestSolution_x1 = x1
  bestSolution_x2 = x2
  bestCost        = currentCost

  while(temp > finalTemp){
    for (i in 1:100) {
      x1       = runif(1, -10, 10)
      x2       = runif(1, -10, 10)
      newSt_x1 = x1
      newSt_x2 = x2
      newCost  = fitness(newSt_x1, newSt_x2)
    }
    if(newCost < currentCost){
      currentSt_x1 = newSt_x1
      currentSt_x2 = newSt_x2
      currentCost  = newCost
      if(currentCost < bestCost){
        bestSolution_x1 = currentSt_x1
        bestSolution_x2 = currentSt_x2
        bestCost        = currentCost
      }
    }
    else{
      deltaE = newCost - currentCost
      if((exp(-(deltaE)/temp))>runif(1,0,1)){
        currentSt_x1 = newSt_x1
        currentSt_x2 = newSt_x2
        currentCost  = newCost
      }
    }
    temp = temp*coolingRate;
  }
  print(paste0("Nilai Fungsi pada Solusi Numerik = ", bestCost))
  print(paste0("Nilai X1 Solusi Numerik          = ", bestSolution_x1))
  print(paste0("Nilai X2 Solusi Numerik          = ", bestSolution_x2))
  print(paste0("Nilai Fungsi pada Solusi Eksa    = ", fitness(0,0)))
  print(paste0("Nilai Akurasi                    = ", 1-(((fitness(0,0)-bestCost)/fitness(0,0))*(100/100))))
}

#======================= MAIN PROGRAM =======================
temp        = 100
finalTemp   = 0.0001
coolingRate = 0.9999

x1          = runif(1, -10, 10)
x2          = runif(1, -10, 10)

simulatedAnn(x1, x2, temp, finalTemp, coolingRate)
