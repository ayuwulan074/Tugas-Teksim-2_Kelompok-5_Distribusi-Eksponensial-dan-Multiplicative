#TUGAS 2 TEKNIK SIMULASI 
#Kelompok 5 : Multiplicative dan Distribusi Eksponensial 
#Nama Anggota Kelompok:
    #1. Andri Suherdi
    #2. Khansa Nimal Abidah
    #3. Ayu Wulandari

#Multiplicative dengan distribusi eksponensial 
generate_ekspo<-function(a,z0,m,n){
  xi <- matrix(NA, n, 4)
  colnames(xi) <- c("aZ", "Xi", "Ui", "Eksponensial")
  for (i in 1:n)
  {
    aZ <-xi[i,1] <-(a*z0)
    Xi <-xi[i,2] <-xi[i,1] %% m  
    Ui <-xi[i,3] <-xi[i,2] / m
    z0 <-xi[i,2]
    lambda<-1
    xi[i,4] <-log(Ui)/lambda
  }
  View(xi)
}
generate_ekspo(45,21139,417,150)
