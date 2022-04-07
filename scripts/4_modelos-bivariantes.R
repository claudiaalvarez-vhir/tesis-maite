# 3.2. Análisis bivariante ####
any_1$abs
a <- glm(ex1 ~ any_1$vacgrip_1, data=any_1, family="binomial")
summary(a)
coef(summary(a))[,4]
i <- 2
c(round(exp(coef(a))[i], 3), paste0(round(exp(confint(a))[i,1], 3), "-", round(exp(confint(a))[i,2], 3)), round(coef(summary(a))[i,4],3))

bivariant <- function(var1, var2,  i=2){
  a <- glm(var1 ~ var2, family="binomial")
  x <- c(round(exp(coef(a))[i], 3), paste0(round(exp(confint(a))[i,1], 3), "-", round(exp(confint(a))[i,2], 3)), round(coef(summary(a))[i,4],3))
  x
}

taula4_1 <- rbind(c("", "IR", "CI 95%", "p-valor"),
                  c("Edat", bivariant(any_1$ex1, any_1$edat_1)),
                  c("Sexe (home)", bivariant(any_1$ex1, any_1$sexe)),
                  c("ABS (respecte ABS1)", "","",""),
                  c("    ABS2", bivariant(any_1$ex1, any_1$abs, 2)),
                  c("    ABS3", bivariant(any_1$ex1, any_1$abs, 3)),
                  c("    ABS4", bivariant(any_1$ex1, any_1$abs, 4)),
                  c("    ABS5", bivariant(any_1$ex1, any_1$abs, 5)),
                  c("    ABS6", bivariant(any_1$ex1, any_1$abs, 6)),
                  c("    ABS7", bivariant(any_1$ex1, any_1$abs, 7)),
                  c("BMI", bivariant(any_1$ex1, any_1$imc)),
                  c("Tabaquisme (respecte no fumador)", "","",""),
                  c("    Ex fumador", bivariant(any_1$ex1, any_1$tabac, 2)),
                  c("    Fumador", bivariant(any_1$ex1, any_1$tabac, 3)),
                  c("Depressió", bivariant(any_1$ex1, any_1$dep)),
                  c("Ansietat", bivariant(any_1$ex1, any_1$ans)),
                  c("FEV", bivariant(any_1$ex1, any_1$fev)),
                  c("FEV1.FVC", bivariant(any_1$ex1, any_1$fev1fvc)),
                  c("Insuficiència cardíaca", bivariant(any_1$ex1, any_1$ic)),
                  c("IRC", bivariant(any_1$ex1, any_1$irc)),
                  c("HTA", bivariant(any_1$ex1, any_1$hta)),
                  c("Vacuna antigripal", bivariant(any_1$ex1, any_1$vacgrip_1)),
                  c("Vacuna antipneumocòccica", bivariant(any_1$ex1, any_1$vacpneumo_1)),
                  c("Enolisme (respecte abstemi)", "","",""),
                  c("    Baix risc", bivariant(any_1$ex1, any_1$enolic, 2)),
                  c("    Alt risc", bivariant(any_1$ex1, any_1$enolic, 3)),
                  c("Charlson", bivariant(any_1$ex1, any_1$charlson_2)),
                  c("MRC", bivariant(any_1$ex1, any_1$mrc_2)),
                  c("Bodex", bivariant(any_1$ex1, any_1$bodex)),
                  c("HBA1C", bivariant(any_1$ex1, any_1$hba1c)),
                  c("GOLD (respecte GOLD1)", "","",""),
                  c("    GOLD2", bivariant(any_1$ex1, any_1$GOLD, 2)),
                  c("    GOLD3-4", bivariant(any_1$ex1, any_1$GOLD, 3)),
                  c("Ex previes", bivariant(any_1$ex1, any_1$Episodis_pre))
                  
)



taula4_2 <- rbind(c("", "IR", "CI 95%", "p-valor"),
                  c("Edat", bivariant(any_2$ex2, any_2$edat_2)),
                  c("Sexe (home)", bivariant(any_2$ex2, any_2$sexe)),
                  c("ABS (respecte ABS1)", "","",""),
                  c("    ABS2", bivariant(any_2$ex2, any_2$abs, 2)),
                  c("    ABS3", bivariant(any_2$ex2, any_2$abs, 3)),
                  c("    ABS4", bivariant(any_2$ex2, any_2$abs, 4)),
                  c("    ABS5", bivariant(any_2$ex2, any_2$abs, 5)),
                  c("    ABS6", bivariant(any_2$ex2, any_2$abs, 6)),
                  c("    ABS7", bivariant(any_2$ex2, any_2$abs, 7)),
                  c("BMI", bivariant(any_2$ex2, any_2$imc_segon)),
                  c("Tabaquisme (respecte no fumador)", "","",""),
                  c("    Ex fumador", bivariant(any_2$ex2, any_2$tabac_segon, 2)),
                  c("    Fumador", bivariant(any_2$ex2, any_2$tabac_segon, 3)),
                  c("Depressió", bivariant(any_2$ex2, any_2$dep)),
                  c("Ansietat", bivariant(any_2$ex2, any_2$ans)),
                  c("FEV", bivariant(any_2$ex2, any_2$fev_segon)),
                  c("FEV1.FVC", bivariant(any_2$ex2, any_2$fev1fvc_segon)),
                  c("Insuficiència cardíaca", bivariant(any_2$ex2, any_2$ic_segon)),
                  c("IRC", bivariant(any_2$ex2, any_2$irc_segon)),
                  c("HTA", bivariant(any_2$ex2, any_2$hta_segon)),
                  c("Vacuna antigripal", bivariant(any_2$ex2, any_2$vacgrip_2)),
                  c("Vacuna antipneumocòccica", bivariant(any_2$ex2, any_2$vacpneumo_2)),
                  c("Enolisme (respecte abstemi)", "","",""),
                  c("    Baix risc", bivariant(any_2$ex2, any_2$enolic_segon, 2)),
                  c("    Alt risc", bivariant(any_2$ex2, any_2$enolic_segon, 3)),
                  c("Charlson", bivariant(any_2$ex2, any_2$charlson_2)),
                  c("MRC", bivariant(any_2$ex2, any_2$mrc_2)),
                  c("Bodex", bivariant(any_2$ex2, any_2$bodex_segon)),
                  c("HBA1C", bivariant(any_2$ex2, any_2$hba1c_segon)),
                  c("GOLD (respecte GOLD1)", "","",""),
                  c("    GOLD2", bivariant(any_2$ex2, any_2$GOLD_segon, 2)),
                  c("    GOLD3-4", bivariant(any_2$ex2, any_2$GOLD_segon, 3)),
                  c("Ex previes", bivariant(any_2$ex2, any_2$Episodis_1))
                  
)
bivariant(any_1$ex1, any_1$fev1fvc)

taula4_3 <- rbind(c("", "IR", "CI 95%", "p-valor"),
                  c("Edat", bivariant(any_2$extot, any_2$edat_1)),
                  c("Sexe (home)", bivariant(any_2$extot, any_2$sexe)),
                  c("ABS (respecte ABS1)", "","",""),
                  c("    ABS2", bivariant(any_2$extot, any_2$abs, 2)),
                  c("    ABS3", bivariant(any_2$extot, any_2$abs, 3)),
                  c("    ABS4", bivariant(any_2$extot, any_2$abs, 4)),
                  c("    ABS5", bivariant(any_2$extot, any_2$abs, 5)),
                  c("    ABS6", bivariant(any_2$extot, any_2$abs, 6)),
                  c("    ABS7", bivariant(any_2$extot, any_2$abs, 7)),
                  c("BMI", bivariant(any_2$extot, any_2$imc)),
                  c("Tabaquisme (respecte no fumador)", "","",""),
                  c("    Ex fumador", bivariant(any_2$extot, any_2$tabac, 2)),
                  c("    Fumador", bivariant(any_2$extot, any_2$tabac, 3)),
                  c("Depressió", bivariant(any_2$extot, any_2$dep)),
                  c("Ansietat", bivariant(any_2$extot, any_2$ans)),
                  c("FEV", bivariant(any_2$extot, any_2$fev)),
                  c("FEV1.FVC", bivariant(any_2$extot, any_2$fev1fvc)),
                  c("Insuficiència cardíaca", bivariant(any_2$extot, any_2$ic)),
                  c("IRC", bivariant(any_2$extot, any_2$irc)),
                  c("HTA", bivariant(any_2$extot, any_2$hta)),
                  c("Vacuna antigripal 1er any", bivariant(any_2$extot, any_2$vacgrip_1)),
                  c("Vacuna antipneumocòccica 1er any", bivariant(any_2$extot, any_2$vacpneumo_1)),
                  c("Vacuna antigripal 2n any", bivariant(any_2$extot, any_2$vacgrip_2)),
                  c("Vacuna antipneumocòccica 2n any", bivariant(any_2$extot, any_2$vacpneumo_2)),
                  c("Enolisme (respecte abstemi)", "","",""),
                  c("    Baix risc", bivariant(any_2$extot, any_2$enolic, 2)),
                  c("    Alt risc", bivariant(any_2$extot, any_2$enolic, 3)),
                  c("Charlson", bivariant(any_2$extot, any_2$charlson_2)),
                  c("MRC", bivariant(any_2$extot, any_2$mrc_2)),
                  c("Bodex", bivariant(any_2$extot, any_2$bodex)),
                  c("HBA1C", bivariant(any_2$extot, any_2$hba1c)),
                  c("GOLD (respecte GOLD1)", "","",""),
                  c("    GOLD2", bivariant(any_2$extot, any_2$GOLD, 2)),
                  c("    GOLD3-4", bivariant(any_2$extot, any_2$GOLD, 3)),
                  c("Ex previes", bivariant(any_2$extot, any_2$Episodis_pre))
                  
)

write.csv(taula4_1, "Resultados/taula41_020222.csv")
write.csv(taula4_2, "Resultados/taula42_020222.csv")
write.csv(taula4_3, "Resultados/taula43_020222.csv")
