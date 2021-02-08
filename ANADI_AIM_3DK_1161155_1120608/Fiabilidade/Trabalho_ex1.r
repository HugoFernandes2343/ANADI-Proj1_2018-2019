#####Duração das sessoes - Sess#####
### Com a variavel global "vpnsessions" compilar todo o codigo e correr a função Sess()

### Função filtra vpnsessions nos intervalos indicados
Gen_Tables_Sess <- function(){
  
### Para o conjunto de todos os servidores - cts
  #print(paste("A filtrar para o conjunto de todos os servidores..."))
  
# Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  Sess_cts_total <<- subset(vpnsessions, Duration>=2)

# Para o ano de 2017.
  Sess_cts_2017 <<- subset(Sess_cts_total, year(Date_Hour_Start)==2017)

# Paro o ano de 2018.
  Sess_cts_2018 <<- subset(Sess_cts_total, year(Date_Hour_Start)==2018)

# Para cada mês dos anos de 2017 e 2018.
  for (month in unique(month(Sess_cts_2017[[3]]))){
    assign(paste0("Sess_cts_2017-",month), subset(Sess_cts_2017, month(Date_Hour_Start)==month), envir = .GlobalEnv)
  }
  for (month in unique(month(Sess_cts_2018[[3]]))){
    assign(paste0("Sess_cts_2018-",month), subset(Sess_cts_2018, month(Date_Hour_Start)==month), envir = .GlobalEnv)
  }


###No conjunto de todos os servidores, para cada tipo de protocolo de VPN - vpn
  #print(paste("A filtrar para cada tipo de VPN..."))
# Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(vpn in unique(Sess_cts_total[[2]])){
    assign(paste0("Sess_vpn_total_", vpn), subset(Sess_cts_total, Protocol==vpn), envir = .GlobalEnv)
  }
# Para o ano de 2017.
  for(vpn in unique(Sess_cts_2017[[2]])){
    assign(paste0("Sess_vpn_2017_", vpn), subset(Sess_cts_2017, Protocol==vpn), envir = .GlobalEnv)
  }
# Para o ano de 2018.
  for(vpn in unique(Sess_cts_2018[[2]])){
    assign(paste0("Sess_vpn_2018_", vpn), subset(Sess_cts_2018, Protocol==vpn), envir = .GlobalEnv)
  }
# Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Sess_cts_2017[[3]]))){
    for(vpn in unique(Sess_cts_2017[[2]])){
      assign(paste0("Sess_vpn_2017-", month, "_", vpn), subset(Sess_cts_2017, month(Date_Hour_Start)==month & Protocol==vpn), envir = .GlobalEnv)
    }
  }
  for(month in unique(month(Sess_cts_2018[[3]]))){
    for(vpn in unique(Sess_cts_2018[[2]])){
      assign(paste0("Sess_vpn_2018-", month, "_", vpn), subset(Sess_cts_2018, month(Date_Hour_Start)==month & Protocol==vpn), envir = .GlobalEnv)
    }
  }


###Para cada servidor - serv
  #print(paste("A filtrar para cada servidor..."))
# Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(serv in unique(Sess_cts_total[[1]])){
    assign(paste0("Sess_serv_total_", serv), subset(Sess_cts_total, Server_Name==serv), envir = .GlobalEnv)
  }
# Para o ano de 2017.
  for(serv in unique(Sess_cts_2017[[1]])){
    assign(paste0("Sess_serv_2017_", serv), subset(Sess_cts_2017, Server_Name==serv), envir = .GlobalEnv)
  }
# Para o ano de 2018.
  for(serv in unique(Sess_cts_2018[[1]])){
    assign(paste0("Sess_serv_2018_", serv), subset(Sess_cts_2018, Server_Name==serv), envir = .GlobalEnv)
  }
# Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Sess_cts_2017[[3]]))){
    for(serv in unique(Sess_cts_2017[[1]])){
      assign(paste0("Sess_serv_2017-", month, "_", serv), subset(Sess_cts_2017, month(Date_Hour_Start)==month & Server_Name==serv), envir = .GlobalEnv)
    }
  }
  for(month in unique(month(Sess_cts_2018[[3]]))){
    for(serv in unique(Sess_cts_2018[[1]])){
      assign(paste0("Sess_serv_2018-", month, "_", serv), subset(Sess_cts_2018, month(Date_Hour_Start)==month & Server_Name==serv), envir = .GlobalEnv)
    }
  }


### Em cada servidor, para cada tipo de protocolo de VPN - serv&vpn
  #print(paste("A filtrar para cada servidor e VPN..."))
# Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(serv in unique(Sess_cts_total[[1]])){
    for(vpn in unique(Sess_cts_total[[2]])){
      assign(paste0("Sess_serv&vpn_total_", serv, "_", vpn), subset(Sess_cts_total, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
# Para o ano de 2017.
  for(serv in unique(Sess_cts_2017[[1]])){
    for(vpn in unique(Sess_cts_2017[[2]])){
      assign(paste0("Sess_serv&vpn_2017_", serv, "_", vpn), subset(Sess_cts_2017, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
# Para o ano de 2018.
  for(serv in unique(Sess_cts_2018[[1]])){
    for(vpn in unique(Sess_cts_2018[[2]])){
      assign(paste0("Sess_serv&vpn_2018_", serv, "_", vpn), subset(Sess_cts_2018, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
# Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Sess_cts_2017[[3]]))){
    for(serv in unique(Sess_cts_2017[[1]])){
      for(vpn in unique(Sess_cts_2017[[2]])){
        assign(paste0("Sess_serv&vpn_2017-", month, "_", serv, "_", vpn), subset(Sess_cts_2017, month(Date_Hour_Start)==month & Server_Name==serv & Protocol==vpn), envir = .GlobalEnv)
      }
    }
  }
  for(month in unique(month(Sess_cts_2018[[3]]))){
    for(serv in unique(Sess_cts_2018[[1]])){
      for(vpn in unique(Sess_cts_2018[[2]])){
        assign(paste0("Sess_serv&vpn_2018-", month, "_", serv, "_", vpn), subset(Sess_cts_2018, month(Date_Hour_Start)==month & Server_Name==serv & Protocol==vpn), envir = .GlobalEnv)
      }
    }
  }

}


### Funcao processa as tabelas criadas e cria nova tabela com os resultados
Sess <- function(){
  
  Gen_Tables_Sess()
  
  Results_Sess <<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                           c("name", "min", "max", "median", "std deviation"))
  #temp_tabelas <- setdiff(Filter(function(x) inherits(get(x), "data.frame"), ls()), c("vpnsessions", "Results_Sess"))
  temp_tabelas <- apropos("Sess_")
  
  for(temp_tabName in temp_tabelas){
    temp_tab <- get(temp_tabName, envir = .GlobalEnv)
    if( nrow(temp_tab) == 0 ){
      Results_Sess[nrow(Results_Sess) + 1,] <<- 
        list(temp_tabName,"no data", "no data", "no data", "no data")
    } else {
      temp_min <- min(as.numeric(unlist(temp_tab[5])))
      temp_max <- max(as.numeric(unlist(temp_tab[5])))
      temp_med <- mean(as.numeric(unlist(temp_tab[5])))
      temp_sd <- sd(as.numeric(unlist(temp_tab[5])))
      
      Results_Sess[nrow(Results_Sess) + 1,] <<-
        list(temp_tabName,temp_min, temp_max, temp_med,temp_sd)
    }
  }
  
    rm(list = apropos("Sess_"), envir = .GlobalEnv)
  
}








#####Tempo entre falhas consecutivas - Fail #####
### Com a variavel global "vpnsessions" compilar todo o codigo e correr a função Fail()

Gen_Tables_Fail <- function(){


### Para o conjunto de todos os servidores - cts

# Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  Fail_cts_total <<- subset(vpnsessions, Duration<2, select = -c(Duration))

# Para o ano de 2017.
  Fail_cts_2017 <<- subset(Fail_cts_total, year(Date_Hour_Start)==2017)

# Paro o ano de 2018.
  Fail_cts_2018 <<- subset(Fail_cts_total, year(Date_Hour_Start)==2018)

# Para cada mês dos anos de 2017 e 2018.
  for (month in unique(month(Fail_cts_2017[[3]]))){
    assign(paste0("Fail_cts_2017-",month), subset(Fail_cts_2017, month(Date_Hour_Start)==month), envir = .GlobalEnv)
  }
  for (month in unique(month(Fail_cts_2018[[3]]))){
    assign(paste0("Fail_cts_2018-",month), subset(Fail_cts_2018, month(Date_Hour_Start)==month), envir = .GlobalEnv)
  }
  

  
###No conjunto de todos os servidores, para cada tipo de protocolo de VPN - vpn

# Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(vpn in unique(Fail_cts_total[[2]])){
    assign(paste0("Fail_vpn_total_", vpn), subset(Fail_cts_total, Protocol==vpn), envir = .GlobalEnv)
  }
# Para o ano de 2017.
  for(vpn in unique(Fail_cts_2017[[2]])){
    assign(paste0("Fail_vpn_2017_", vpn), subset(Fail_cts_2017, Protocol==vpn), envir = .GlobalEnv)
  }
# Para o ano de 2018.
  for(vpn in unique(Fail_cts_2018[[2]])){
    assign(paste0("Fail_vpn_2018_", vpn), subset(Fail_cts_2018, Protocol==vpn), envir = .GlobalEnv)
  }
# Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Fail_cts_2017[[3]]))){
    for(vpn in unique(Fail_cts_2017[[2]])){
      assign(paste0("Fail_vpn_2017-", month, "_", vpn), subset(Fail_cts_2017, month(Date_Hour_Start)==month & Protocol==vpn), envir = .GlobalEnv)
    }
  }
  for(month in unique(month(Fail_cts_2018[[3]]))){
    for(vpn in unique(Fail_cts_2018[[2]])){
      assign(paste0("Fail_vpn_2018-", month, "_", vpn), subset(Fail_cts_2018, month(Date_Hour_Start)==month & Protocol==vpn), envir = .GlobalEnv)
    }
  }
  
  
###Para cada servidor - serv
#print(paste("A filtrar para cada servidor..."))
# Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(serv in unique(Fail_cts_total[[1]])){
    assign(paste0("Fail_serv_total_", serv), subset(Fail_cts_total, Server_Name==serv), envir = .GlobalEnv)
  }
# Para o ano de 2017.
  for(serv in unique(Fail_cts_2017[[1]])){
    assign(paste0("Fail_serv_2017_", serv), subset(Fail_cts_2017, Server_Name==serv), envir = .GlobalEnv)
  }
# Para o ano de 2018.
  for(serv in unique(Fail_cts_2018[[1]])){
    assign(paste0("Fail_serv_2018_", serv), subset(Fail_cts_2018, Server_Name==serv), envir = .GlobalEnv)
  }
# Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Fail_cts_2017[[3]]))){
    for(serv in unique(Fail_cts_2017[[1]])){
      assign(paste0("Fail_serv_2017-", month, "_", serv), subset(Fail_cts_2017, month(Date_Hour_Start)==month & Server_Name==serv), envir = .GlobalEnv)
    }
  }
  for(month in unique(month(Fail_cts_2018[[3]]))){
    for(serv in unique(Fail_cts_2018[[1]])){
      assign(paste0("Fail_serv_2018-", month, "_", serv), subset(Fail_cts_2018, month(Date_Hour_Start)==month & Server_Name==serv), envir = .GlobalEnv)
    }
  }
  

### Em cada servidor, para cada tipo de protocolo de VPN - serv&vpn
#print(paste("A filtrar para cada servidor e VPN..."))
# Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(serv in unique(Fail_cts_total[[1]])){
    for(vpn in unique(Fail_cts_total[[2]])){
      assign(paste0("Fail_serv&vpn_total_", serv, "_", vpn), subset(Fail_cts_total, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
# Para o ano de 2017.
  for(serv in unique(Fail_cts_2017[[1]])){
    for(vpn in unique(Fail_cts_2017[[2]])){
      assign(paste0("Fail_serv&vpn_2017_", serv, "_", vpn), subset(Fail_cts_2017, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
# Para o ano de 2018.
  for(serv in unique(Fail_cts_2018[[1]])){
    for(vpn in unique(Fail_cts_2018[[2]])){
      assign(paste0("Fail_serv&vpn_2018_", serv, "_", vpn), subset(Fail_cts_2018, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
# Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Fail_cts_2017[[3]]))){
    for(serv in unique(Fail_cts_2017[[1]])){
      for(vpn in unique(Fail_cts_2017[[2]])){
        assign(paste0("Fail_serv&vpn_2017-", month, "_", serv, "_", vpn), subset(Fail_cts_2017, month(Date_Hour_Start)==month & Server_Name==serv & Protocol==vpn), envir = .GlobalEnv)
      }
    }
  }
  for(month in unique(month(Fail_cts_2018[[3]]))){
    for(serv in unique(Fail_cts_2018[[1]])){
      for(vpn in unique(Fail_cts_2018[[2]])){
        assign(paste0("Fail_serv&vpn_2018-", month, "_", serv, "_", vpn), subset(Fail_cts_2018, month(Date_Hour_Start)==month & Server_Name==serv & Protocol==vpn), envir = .GlobalEnv)
      }
    }
  }
  
}


### Funcao processa as tabelas criadas e cria nova tabela com os resultados
  
Fail <- function(){
  
  Gen_Tables_Fail()
  
  Results_Fail <<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                            c("name", "min", "max", "median", "std deviation"))
  
  temp_tabelas <- apropos("Fail_")
  
  for(temp_tabName in temp_tabelas){
    temp_tab <- get(temp_tabName, envir = .GlobalEnv)
    if( nrow(temp_tab) == 0 ){
      Results_Fail[nrow(Results_Fail) + 1,] <<- 
        list(temp_tabName,"no data", "no data", "no data", "no data")
    } else if(nrow(temp_tab) == 1) {
      Results_Fail[nrow(Results_Fail) + 1,] <<- 
        list(temp_tabName,"one entry only", "one entry only", "one entry only", "one entry only")
    }else{
      
      temp_tab["Minutes between failures"] <- 
        as.numeric(difftime(temp_tab$Date_Hour_Start, lag(temp_tab$Date_Hour_Start), units = "mins"))
      
      temp_min <- min(as.numeric(unlist(temp_tab[5])), na.rm = TRUE)
      temp_max <- max(as.numeric(unlist(temp_tab[5])), na.rm = TRUE)
      temp_med <- mean(as.numeric(unlist(temp_tab[5])), na.rm = TRUE)
      temp_sd <- sd(as.numeric(unlist(temp_tab[5])), na.rm = TRUE)
      
      Results_Fail[nrow(Results_Fail) + 1,] <<-
        list(temp_tabName,temp_min, temp_max, temp_med,temp_sd)
    }
  }
  
  rm(list = apropos("Fail_"), envir = .GlobalEnv)
  
}




#####Número de sessões simultâneas - Nrs #####


### Função filtra vpnsessions nos intervalos indicados
Gen_Tables_Nrs <- function(){
  
  ### Para o conjunto de todos os servidores - cts
  # Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  Nrs_cts_total <<- subset(vpnsessions, Duration>0)

  # Para o ano de 2017.
  Nrs_cts_2017 <<- subset(Nrs_cts_total, year(Date_Hour_Start)==2017)

  # Paro o ano de 2018.
  Nrs_cts_2018 <<- subset(Nrs_cts_total, year(Date_Hour_Start)==2018)

  # Para cada mês dos anos de 2017 e 2018.
  for (month in unique(month(Nrs_cts_2017[[3]]))){
    assign(paste0("Nrs_cts_2017-",month), subset(Nrs_cts_2017, month(Date_Hour_Start)==month), envir = .GlobalEnv)
  }
  for (month in unique(month(Nrs_cts_2018[[3]]))){
    assign(paste0("Nrs_cts_2018-",month), subset(Nrs_cts_2018, month(Date_Hour_Start)==month), envir = .GlobalEnv)
  }


  ###No conjunto de todos os servidores, para cada tipo de protocolo de VPN - vpn
  # Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(vpn in unique(Nrs_cts_total[[2]])){
    assign(paste0("Nrs_vpn_total_", vpn), subset(Nrs_cts_total, Protocol==vpn), envir = .GlobalEnv)
  }
  # Para o ano de 2017.
  for(vpn in unique(Nrs_cts_2017[[2]])){
    assign(paste0("Nrs_vpn_2017_", vpn), subset(Nrs_cts_2017, Protocol==vpn), envir = .GlobalEnv)
  }
  # Para o ano de 2018.
  for(vpn in unique(Nrs_cts_2018[[2]])){
    assign(paste0("Nrs_vpn_2018_", vpn), subset(Nrs_cts_2018, Protocol==vpn), envir = .GlobalEnv)
  }
  # Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Nrs_cts_2017[[3]]))){
    for(vpn in unique(Nrs_cts_2017[[2]])){
      assign(paste0("Nrs_vpn_2017-", month, "_", vpn), subset(Nrs_cts_2017, month(Date_Hour_Start)==month & Protocol==vpn), envir = .GlobalEnv)
    }
  }
  for(month in unique(month(Nrs_cts_2018[[3]]))){
    for(vpn in unique(Nrs_cts_2018[[2]])){
      assign(paste0("Nrs_vpn_2018-", month, "_", vpn), subset(Nrs_cts_2018, month(Date_Hour_Start)==month & Protocol==vpn), envir = .GlobalEnv)
    }
  }

  
  ###Para cada servidor - serv
  # Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(serv in unique(Nrs_cts_total[[1]])){
    assign(paste0("Nrs_serv_total_", serv), subset(Nrs_cts_total, Server_Name==serv), envir = .GlobalEnv)
  }
  # Para o ano de 2017.
  for(serv in unique(Nrs_cts_2017[[1]])){
    assign(paste0("Nrs_serv_2017_", serv), subset(Nrs_cts_2017, Server_Name==serv), envir = .GlobalEnv)
  }
  # Para o ano de 2018.
  for(serv in unique(Nrs_cts_2018[[1]])){
    assign(paste0("Nrs_serv_2018_", serv), subset(Nrs_cts_2018, Server_Name==serv), envir = .GlobalEnv)
  }
  # Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Nrs_cts_2017[[3]]))){
    for(serv in unique(Nrs_cts_2017[[1]])){
      assign(paste0("Nrs_serv_2017-", month, "_", serv), subset(Nrs_cts_2017, month(Date_Hour_Start)==month & Server_Name==serv), envir = .GlobalEnv)
    }
  }
  for(month in unique(month(Nrs_cts_2018[[3]]))){
    for(serv in unique(Nrs_cts_2018[[1]])){
      assign(paste0("Nrs_serv_2018-", month, "_", serv), subset(Nrs_cts_2018, month(Date_Hour_Start)==month & Server_Name==serv), envir = .GlobalEnv)
    }
  }
  
  
  ### Em cada servidor, para cada tipo de protocolo de VPN - serv&vpn
  # Para a totalidade dos dados disponíveis (2016-12-25 até 2019-02-28).
  for(serv in unique(Nrs_cts_total[[1]])){
    for(vpn in unique(Nrs_cts_total[[2]])){
      assign(paste0("Nrs_serv&vpn_total_", serv, "_", vpn), subset(Nrs_cts_total, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
  # Para o ano de 2017.
  for(serv in unique(Nrs_cts_2017[[1]])){
    for(vpn in unique(Nrs_cts_2017[[2]])){
      assign(paste0("Nrs_serv&vpn_2017_", serv, "_", vpn), subset(Nrs_cts_2017, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
  # Para o ano de 2018.
  for(serv in unique(Nrs_cts_2018[[1]])){
    for(vpn in unique(Nrs_cts_2018[[2]])){
      assign(paste0("Nrs_serv&vpn_2018_", serv, "_", vpn), subset(Nrs_cts_2018, Server_Name==serv & Protocol == vpn), envir = .GlobalEnv)
    }
  }
  # Para cada mês dos anos de 2017 e 2018.
  for(month in unique(month(Nrs_cts_2017[[3]]))){
    for(serv in unique(Nrs_cts_2017[[1]])){
      for(vpn in unique(Nrs_cts_2017[[2]])){
        assign(paste0("Nrs_serv&vpn_2017-", month, "_", serv, "_", vpn), subset(Nrs_cts_2017, month(Date_Hour_Start)==month & Server_Name==serv & Protocol==vpn), envir = .GlobalEnv)
      }
    }
  }
  for(month in unique(month(Nrs_cts_2018[[3]]))){
    for(serv in unique(Nrs_cts_2018[[1]])){
      for(vpn in unique(Nrs_cts_2018[[2]])){
        assign(paste0("Nrs_serv&vpn_2018-", month, "_", serv, "_", vpn), subset(Nrs_cts_2018, month(Date_Hour_Start)==month & Server_Name==serv & Protocol==vpn), envir = .GlobalEnv)
      }
    }
  }
  
}



Calc_Nrs <- function(){
  
  
  print(paste0("Filtering tables."))
  Gen_Tables_Nrs()
  
  temp_tabelas <- apropos("Nrs_")
  count <- 1
  total <- length(temp_tabelas)
  
  print(paste0("Calculating..."))
  for(temp_tabName in temp_tabelas){
    
    print(paste0(count, "/", total))
    count <- count + 1
    temp_tab <- get(temp_tabName, envir = .GlobalEnv)
    
     tabela <- as.POSIXct(rep(NA,0))
     if(nrow(temp_tab) >= 1){
       for(i in 1:nrow(temp_tab)){
         tabela <- c(tabela, seq.POSIXt(temp_tab[[i,3]], temp_tab[[i,4]], by="min"))
       }
     }
    
    # indx <- 1
    # if(nrow(temp_tab) >= 1){
    #   for(i in 1:nrow(temp_tab)){
    #     a_adicionar <-  seq.POSIXt(temp_tab[[i,3]], temp_tab[[i,4]], by="min")
    #     indxj <- 1
    #     for(j in indx:(indx+length(a_adicionar))){
    #       tabela[[j]] <- a_adicionar[indxj]
    #       indxj <- indxj + 1
    #     }
    #     indx <- indx+nrow(temp_tab)+1
    #   }
    # }
    # print(paste0("A limpar."))
    # tabela <- tabela[!is.na(tabela)]
    
    temp <- setNames(as.data.frame(tabela), "Timestamp")
    temp <- count(temp, Timestamp, name = "Count")
    
    assign(paste0(temp_tabName),temp, envir = .GlobalEnv )
  }
}


Nrs <- function(){
  Calc_Nrs()
  
  temp_tabelas <- apropos("Nrs_")
  
  for(temp_tabName in temp_tabelas){
    temp_tab <- get(temp_tabName, envir = .GlobalEnv)
    if( nrow(temp_tab) == 0 ){
      Results_Nrs[nrow(Results_Nrs) + 1,] <<- 
        list(temp_tabName,"no data", "no data", "no data", "no data")
    } else if(nrow(temp_tab) == 1) {
      Results_Nrs[nrow(Results_Nrs) + 1,] <<- 
        list(temp_tabName,"one entry only", "one entry only", "one entry only", "one entry only")
    }else{
      
      temp_tab["Minutes between failures"] <- 
        as.numeric(difftime(temp_tab$Date_Hour_Start, lag(temp_tab$Date_Hour_Start), units = "mins"))
      
      temp_min <- min(as.numeric(unlist(temp_tab[5])), na.rm = TRUE)
      temp_max <- max(as.numeric(unlist(temp_tab[5])), na.rm = TRUE)
      temp_med <- mean(as.numeric(unlist(temp_tab[5])), na.rm = TRUE)
      temp_sd <- sd(as.numeric(unlist(temp_tab[5])), na.rm = TRUE)
      
      Results_Nrs[nrow(Results_Nrs) + 1,] <<-
        list(temp_tabName,temp_min, temp_max, temp_med,temp_sd)
    }
  }
  
  #rm(list = apropos("Fail_"), envir = .GlobalEnv)
  
  
  
}

