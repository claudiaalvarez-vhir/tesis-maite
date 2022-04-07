# **********************************
#   Análisis tesis Maite Castañ
# 3. Incidencia de las exacerbaciones
# **********************************



# 3.1. Incidència de les exacerbacions en funció del GOLD ####


cont <- function(var1, var2, var3, var4){
  c(sum(!is.na(var1)), paste0(round(mean(var1, na.rm=T),2), " (", round(sd(var1, na.rm=T),2), ") [",round(min(var1, na.rm = T), 2), "-", round(max(var1, na.rm = T), 2), "]"),
    sum(!is.na(var2)), paste0(round(mean(var2, na.rm=T),2), " (", round(sd(var2, na.rm=T),2), ") [",round(min(var2, na.rm = T), 2), "-", round(max(var2, na.rm = T), 2), "]"),
    sum(!is.na(var3)), paste0(round(mean(var3, na.rm=T),2), " (", round(sd(var3, na.rm=T),2), ") [",round(min(var3, na.rm = T), 2), "-", round(max(var3, na.rm = T), 2), "]"),
    sum(!is.na(var4)), paste0(round(mean(var4, na.rm=T),2), " (", round(sd(var4, na.rm=T),2), ") [",round(min(var4, na.rm = T), 2), "-", round(max(var4, na.rm = T), 2), "]"))
}



cat <- function(var1, var2, var3, var4, var5, i){
  c(paste0(table(var1)[i], " (", round(table(var1)[i]/sum(!is.na(var1))*100, 2), "%)"),
    paste0(table(var2)[i], " (", round(table(var2)[i]/sum(!is.na(var2))*100, 2), "%)"),
    paste0(table(var3)[i], " (", round(table(var3)[i]/sum(!is.na(var3))*100, 2), "%)"),
    paste0(table(var4)[i], " (", round(table(var4)[i]/sum(!is.na(var4))*100, 2), "%)"),
    paste0(table(var5)[i], " (", round(table(var4)[i]/sum(!is.na(var4))*100, 2), "%)"))
}



media <- function(var){
  paste0(round(mean(var, na.rm=T),2), " (", round(sd(var, na.rm=T),2), ") [",round(min(var, na.rm = T), 2), "-", round(max(var, na.rm = T), 2), "]")
}

pct <- function(var, i){
  paste0(table(var)[i], " (", round(table(var)[i]/sum(!is.na(var))*100, 2), "%)")
}

chi <- function(var1, var2){
  tryCatch(round(chisq.test(var1,var2, simulate.p.value = T)$p.value, 3), error=function(err) "")
}

anv <- function(var1, var2){
  aa <- aov(var1 ~ var2)
  round(summary(aa)[[1]][[1,"Pr(>F)"]],3)
  
}

### Primer any

taula2_1 <- rbind(c("", paste0("GOLD 1 (N=", table(any_1$GOLD)[1], ")"), paste0("GOLD 2 (N=", table(any_1$GOLD)[2], ")"), paste0("GOLD 3 (N=", table(any_1$GOLD)[3], ")"), paste0("GOLD 4 (N=", table(any_1$GOLD)[4], ")"), paste0("Total (N=", nrow(any_1), ")"), "p-valor"),
                  c("Número d'exacerbacions", "", "", "", "", "", chi(any_1$GOLD, any_1$exa1)),
                  c("Cap exacerbació", cat(any_11$exa1, any_12$exa1,any_13$exa1,any_14$exa1, any_1$exa1, 1), ""),
                  c("1-2 exacerbacions", cat(any_11$exa1, any_12$exa1,any_13$exa1,any_14$exa1, any_1$exa1, 2), ""),
                  c("3 o més exacerbacions", cat(any_11$exa1, any_12$exa1,any_13$exa1,any_14$exa1, any_1$exa1, 3), ""))

taula3_1 <- rbind(c("", "n (range)", "Mean (SD)", "Median [IQR]"),
                  c("GOLD severity stage", "", "", ""),
                  c("GOLD 1", paste0(table(any_1$GOLD)[1], " (", min(any_11$Episodis_1), "-", max(any_11$Episodis_1), ")"), paste0(round(mean(any_11$Episodis_1),2), " (", round(sd(any_11$Episodis_1),2), ")"), paste0(median(any_11$Episodis_1), " [", quantile(any_11$Episodis_1)[2], "-", quantile(any_11$Episodis_1)[4], "]")),
                  c("GOLD 2", paste0(table(any_1$GOLD)[2], " (", min(any_12$Episodis_1), "-", max(any_12$Episodis_1), ")"), paste0(round(mean(any_12$Episodis_1),2), " (", round(sd(any_12$Episodis_1),2), ")"), paste0(median(any_12$Episodis_1), " [", quantile(any_12$Episodis_1)[2], "-", quantile(any_12$Episodis_1)[4], "]")),
                  c("GOLD 3", paste0(table(any_1$GOLD)[3], " (", min(any_13$Episodis_1), "-", max(any_13$Episodis_1), ")"), paste0(round(mean(any_13$Episodis_1),2), " (", round(sd(any_13$Episodis_1),2), ")"), paste0(median(any_13$Episodis_1), " [", quantile(any_13$Episodis_1)[2], "-", quantile(any_13$Episodis_1)[4], "]")),
                  c("GOLD 4", paste0(table(any_1$GOLD)[4], " (", min(any_14$Episodis_1), "-", max(any_14$Episodis_1), ")"), paste0(round(mean(any_14$Episodis_1),2), " (", round(sd(any_14$Episodis_1),2), ")"), paste0(median(any_14$Episodis_1), " [", quantile(any_14$Episodis_1)[2], "-", quantile(any_14$Episodis_1)[4], "]")),
                  c("Total", paste0(nrow(any_1), " (", min(any_1$Episodis_1), "-", max(any_1$Episodis_1), ")"), paste0(round(mean(any_1$Episodis_1),2), " (", round(sd(any_1$Episodis_1),2), ")"), paste0(median(any_1$Episodis_1), " [", quantile(any_1$Episodis_1)[2], "-", quantile(any_1$Episodis_1)[4], "]")),
                  c("p-value", "", krus(any_1$GOLD, any_1$Episodis_1), ""))

write.csv(taula2_1, "Resultados/taula2_1_010222.csv")
write.csv(taula3_1, "Resultados/taula3_1_010222.csv")

### 3.1.2. Segon any 


taula2_2 <- rbind(c("", paste0("GOLD 1 (N=", table(any_2$GOLD_segon)[1], ")"), paste0("GOLD 2 (N=", table(any_2$GOLD_segon)[2], ")"), paste0("GOLD 3 (N=", table(any_2$GOLD_segon)[3], ")"), paste0("GOLD 4 (N=", table(any_2$GOLD_segon)[4], ")"), paste0("Total (N=", nrow(any_2), ")"), "p-valor"),
                  c("Número d'exacerbacions", "", "", "", "", "", chi(any_2$GOLD_segon, any_2$exa2)),
                  c("Cap exacerbació", cat(any_21$exa2, any_22$exa2,any_23$exa2,any_24$exa2, any_2$exa2, 1), ""),
                  c("1-2 exacerbacions", cat(any_21$exa2, any_22$exa2,any_23$exa2,any_24$exa2, any_2$exa2, 2), ""),
                  c("3 o més exacerbacions", cat(any_21$exa2, any_22$exa2,any_23$exa2,any_24$exa2, any_2$exa2, 3), ""))

taula3_2 <- rbind(c("", "n (range)", "Mean (SD)", "Median [IQR]"),
                  c("GOLD severity stage", "", "", ""),
                  c("GOLD 1", paste0(table(any_2$GOLD_segon)[1], " (", min(any_21$Episodis_2), "-", max(any_21$Episodis_2), ")"), paste0(round(mean(any_21$Episodis_2),2), " (", round(sd(any_21$Episodis_2),2), ")"), paste0(median(any_21$Episodis_2), " [", quantile(any_21$Episodis_2)[2], "-", quantile(any_21$Episodis_2)[4], "]")),
                  c("GOLD 2", paste0(table(any_2$GOLD_segon)[2], " (", min(any_22$Episodis_2), "-", max(any_22$Episodis_2), ")"), paste0(round(mean(any_22$Episodis_2),2), " (", round(sd(any_22$Episodis_2),2), ")"), paste0(median(any_22$Episodis_2), " [", quantile(any_22$Episodis_2)[2], "-", quantile(any_22$Episodis_2)[4], "]")),
                  c("GOLD 3", paste0(table(any_2$GOLD_segon)[3], " (", min(any_23$Episodis_2), "-", max(any_23$Episodis_2), ")"), paste0(round(mean(any_23$Episodis_2),2), " (", round(sd(any_23$Episodis_2),2), ")"), paste0(median(any_23$Episodis_2), " [", quantile(any_23$Episodis_2)[2], "-", quantile(any_23$Episodis_2)[4], "]")),
                  c("GOLD 4", paste0(table(any_2$GOLD_segon)[4], " (", min(any_24$Episodis_2), "-", max(any_24$Episodis_2), ")"), paste0(round(mean(any_24$Episodis_2),2), " (", round(sd(any_24$Episodis_2),2), ")"), paste0(median(any_24$Episodis_2), " [", quantile(any_24$Episodis_2)[2], "-", quantile(any_24$Episodis_2)[4], "]")),
                  c("Total", paste0(nrow(any_2), " (", min(any_2$Episodis_2), "-", max(any_2$Episodis_2), ")"), paste0(round(mean(any_2$Episodis_2),2), " (", round(sd(any_2$Episodis_2),2), ")"), paste0(median(any_2$Episodis_2), " [", quantile(any_2$Episodis_2)[2], "-", quantile(any_2$Episodis_2)[4], "]")),
                  c("p-value", "", krus(any_2$GOLD_segon, any_2$Episodis_2), ""))

write.csv(taula2_2, "Resultados/taula2_2_010222.csv")
write.csv(taula3_2, "Resultados/taula3_2_010222.csv")


### 3.1.3. Període sencer


taula2_3 <- rbind(c("", paste0("GOLD 1 (N=", table(any_2$GOLD)[1], ")"), paste0("GOLD 2 (N=", table(any_2$GOLD)[2], ")"), paste0("GOLD 3 (N=", table(any_2$GOLD)[3], ")"), paste0("GOLD 4 (N=", table(any_2$GOLD)[4], ")"), paste0("Total (N=", nrow(any_2), ")"), "p-valor"),
                  c("Número d'exacerbacions", "", "", "", "", "", chi(any_2$GOLD, any_2$exatot)),
                  c("Cap exacerbació", cat(any_21$exatot, any_22$exatot,any_23$exatot,any_24$exatot, any_2$exatot, 1), ""),
                  c("1-2 exacerbacions", cat(any_21$exatot, any_22$exatot,any_23$exatot,any_24$exatot, any_2$exatot, 2), ""),
                  c("3 o més exacerbacions", cat(any_21$exatot, any_22$exatot,any_23$exatot,any_24$exatot, any_2$exatot, 3), ""))

taula3_3 <- rbind(c("", "n (range)", "Mean (SD)", "Median [IQR]"),
                  c("GOLD severity stage", "", "", ""),
                  c("GOLD 1", paste0(table(any_2$GOLD)[1], " (", min(any_21$Episodis_tot), "-", max(any_21$Episodis_tot), ")"), paste0(round(mean(any_21$Episodis_tot),2), " (", round(sd(any_21$Episodis_tot),2), ")"), paste0(median(any_21$Episodis_tot), " [", quantile(any_21$Episodis_tot)[2], "-", quantile(any_21$Episodis_tot)[4], "]")),
                  c("GOLD 2", paste0(table(any_2$GOLD)[2], " (", min(any_22$Episodis_tot), "-", max(any_22$Episodis_tot), ")"), paste0(round(mean(any_22$Episodis_tot),2), " (", round(sd(any_22$Episodis_tot),2), ")"), paste0(median(any_22$Episodis_tot), " [", quantile(any_22$Episodis_tot)[2], "-", quantile(any_22$Episodis_tot)[4], "]")),
                  c("GOLD 3", paste0(table(any_2$GOLD)[3], " (", min(any_23$Episodis_tot), "-", max(any_23$Episodis_tot), ")"), paste0(round(mean(any_23$Episodis_tot),2), " (", round(sd(any_23$Episodis_tot),2), ")"), paste0(median(any_23$Episodis_tot), " [", quantile(any_23$Episodis_tot)[2], "-", quantile(any_23$Episodis_tot)[4], "]")),
                  c("GOLD 4", paste0(table(any_2$GOLD)[4], " (", min(any_24$Episodis_tot), "-", max(any_24$Episodis_tot), ")"), paste0(round(mean(any_24$Episodis_tot),2), " (", round(sd(any_24$Episodis_tot),2), ")"), paste0(median(any_24$Episodis_tot), " [", quantile(any_24$Episodis_tot)[2], "-", quantile(any_24$Episodis_tot)[4], "]")),
                  c("Total", paste0(nrow(any_2), " (", min(any_2$Episodis_tot), "-", max(any_2$Episodis_tot), ")"), paste0(round(mean(any_2$Episodis_tot),2), " (", round(sd(any_2$Episodis_tot),2), ")"), paste0(median(any_2$Episodis_tot), " [", quantile(any_2$Episodis_tot)[2], "-", quantile(any_2$Episodis_tot)[4], "]")),
                  c("p-value", "", krus(any_2$GOLD, any_2$Episodis_tot), ""))

write.csv(taula2_3, "Resultados/taula2_3_010222.csv")
write.csv(taula3_3, "Resultados/taula3_3_010222.csv")
