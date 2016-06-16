table <- read.csv(file = "./PLANILHA-LEI-ROUANET.csv")

table$CNPJ...CPF <- as.character(table$CNPJ...CPF) 
table$vlCaptado <- as.numeric(gsub("[,]","",table$vlCaptado))

cap.table <- table[table$vlCaptado != 0,]
rouanet.tab <- cap.table[duplicated(cap.table$CNPJ...CPF)==FALSE,]

vlTotalCapitado <- NULL; qntdade.vezes <- NULL; j <- 1
for (i in rouanet.tab$CNPJ...CPF) {
  n <- which(i == cap.table$CNPJ...CPF)
  qntdade.vezes[j] <-  length(n)
  vlTotalCapitado[j] <- sum(cap.table$vlCaptado[n])
  j <- j +1
}

rouanet.tab$liberacoes <- qntdade.vezes
rouanet.tab$valorCapitado <- vlTotalCapitado
colnames(rouanet.tab)[5] <- "Projeto"
rm(list=setdiff(ls(), "rouanet.tab"))

save.image("rouanet.Rdata")

library(plotly)
load("./rouanet.Rdata")
plot_ly(y = rouanet.tab$Captacao, type = "box") %>%
  add_trace(y = rouanet.tab$Captacao, type = "box")