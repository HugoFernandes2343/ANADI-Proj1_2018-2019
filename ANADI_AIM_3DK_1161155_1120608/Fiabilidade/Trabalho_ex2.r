### Ex2
## a)
# disp = up/total



calc_disp<- function(table){
  up <- nrow(subset(table, Duration >=2))
  calc <- (up / nrow(table)) * 100
  
  return(calc)
}

alinA <- function(){

  #filtro - total (durante todo o período de verificação) de cada um dos serviços.
  for(serv in unique(vpnsessions[[1]])){
    assign(paste0("alineaA__total_", serv), subset(vpnsessions, Server_Name==serv), envir = .GlobalEnv)
  }
  #filtro - no ano de 2017 do servidor "vsrv10".
  alineaA_2017_vsrv10 <<- subset(vpnsessions, year(Date_Hour_Start)==2017 & Server_Name == "vsrv10")
  #no mês de março 2017 do servidor "vsrv16".
  alineaA_2017_03_vsrv16 <<- subset(vpnsessions, month(Date_Hour_Start)==3 & year(Date_Hour_Start)==2017 & Server_Name == "vsrv16")
  #no dia 28 de fevereiro 2017 do servidor "vsrv17".
  alineaA_2017_02_28_vsrv16 <<- subset(vpnsessions, day(Date_Hour_Start)==28 & month(Date_Hour_Start)==3 & year(Date_Hour_Start)==2017 & Server_Name == "vsrv17")
  
  
  
  Results_a <<- setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                            c("nome", "disponibilidade"))
  
  temp_tabelas <- apropos("alineaA_")
  
  for(temp_tabName in temp_tabelas){
    temp_tab <- get(temp_tabName, envir = .GlobalEnv)
    
    Results_a[nrow(Results_a) + 1,] <<-
      list(temp_tabName, calc_disp(temp_tab))
    
  }
  
  rm(list = apropos("alineaA_"), envir = .GlobalEnv)

}


## b)

calc_fail <- function(table){
  
  up <- nrow(subset(table, Duration >=2))
  down <- nrow(subset(table, Duration <2))
  total <- nrow(table)
  calc <- down/(up*(1440/total))
  
  return(calc)
}

alinB <- function(){
  
  alineaB_jan <<- subset(vpnsessions, day(Date_Hour_Start)==1 & month(Date_Hour_Start)==1 & year(Date_Hour_Start)==2017 & Server_Name == "vsrv8")
  alineaB_fev <<- subset(vpnsessions, day(Date_Hour_Start)==1 & month(Date_Hour_Start)==2 & year(Date_Hour_Start)==2017 & Server_Name == "vsrv8")
  
  alineaB_jan <<- calc_fail(alineaB_jan)
  alineaB_fev <<- calc_fail(alineaB_fev)
  
  curve( exp(-alineaB_jan * x) , from=0 , to=1500, xlab = "min", ylab = "Fiabilidade", main = "1 de Janeiro")
  curve( exp(-alineaB_fev * x) , from=0 , to=1500, xlab = "min", ylab = "Fiabilidade", main = "1 de Fevereiro")
}


## c)


alinC <- function(){
  AlineaC_2017_12 <- subset(vpnsessions, year(Date_Hour_Start)==2017 & month(Date_Hour_Start)==12)
  
  falhas <- c()
  for (day in unique(day(AlineaC_2017_12[[3]]))){
    falhas <- c(falhas, calc_fail(subset(AlineaC_2017_12, day(AlineaC_2017_12[[3]])==day)))
  }
  
  t.test(falhas, mu = 0.01, conf.level = 0.95, alternative = "less")
}

## d)

mtbf<- function(table){
  
  time <- as.numeric(difftime(table$Date_Hour_Start, lag(table$Date_Hour_Start), units = "mins"))
  time <- time[!is.na(time)]
  return(time)
}


alinD <- function(){
  
  alineaC_vsrv10 <- subset(vpnsessions, Server_Name == "vsrv10" & Duration < 2)
  alineaC_vsrv17 <- subset(vpnsessions, Server_Name == "vsrv17" & Duration < 2)
  
  alineaC_vsrv10 <- mtbf(alineaC_vsrv10)
  alineaC_vsrv17 <- mtbf(alineaC_vsrv17)
  
  val = stack(list(alineaC_vsrv10=alineaC_vsrv10, alineaC_vsrv17=alineaC_vsrv17)) 
  
  library(car)         
  leveneTest(values~ind, val, center=mean)
  
  t.test(values~ind, val, conf.level = 0.95,  alternative = "two.sided")
  
  
}













