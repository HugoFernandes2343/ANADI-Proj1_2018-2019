library(readr)
vpnsessions <- read_table2("E:/Faculdade/ANADI/vpnsessions.txt", 
                           col_names = c("Server_Name","Protocol","Date_of_Access", "Time_of_Start", "Time_of_end", "skip1","Duration","skip2"), col_types = cols(Server_Name = col_character(), 
                                                                                                                                                                  Protocol = col_character(), Date_Of_Access = col_date(format = "%Y-%m-%d"), 
                                                                                                                                                                  Time_of_Start = col_time(format = "%H:%M"), 
                                                                                                                                                                  Time_of_end = col_time(format = "%H:%M"), 
                                                                                                                                                                  skip1 = col_skip(), Duration = col_integer(), 
                                                                                                                                                                  skip2 = col_skip()))


library(tidyverse)
library(lubridate)

######alterar tabela inicial######
#é preciso juntar a coluna de data e hora de inicio, mudando tambem o formato e depois vai se usar o tempo de duração
#para calcular a data e hora de termino da sessao, limpar as colunas e mostrar a tabela pela data de inicio e hora
vpnsessions = unite(vpnsessions,
                    "Date_Hour_Start",
                    c("Date_of_Access",
                    "Time_of_Start"),
                    sep=" ",
                    remove=FALSE)

vpnsessions = mutate(vpnsessions,
                     Date_Hour_Start= ymd_hms(Date_Hour_Start))


vpnsessions["Date_Hour_End"] = vpnsessions$Date_Hour_Start + vpnsessions$Duration * 60

vpnsessions[4] = NULL
vpnsessions[5] = NULL
vpnsessions[6] = NULL

vpnsessions <- vpnsessions[c(1,2,3,5,4)]

#data de inicio
vpnsessions <- vpnsessions[order(vpnsessions$Date_Hour_Start), ]




View(vpnsessions)


vpnsessions