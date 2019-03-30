library("lavaan")
library("semPlot")

gendata <- read.csv(file="../data/generated.csv", header=TRUE, sep="\t")

model1 <- "
dijital_kanal =~ hata_islem_sayisi + islem_ortalama_tiklama + cevrim_ici_sure + basarili_giris_sayisi + gezdigi_sayfa_sayisi
sosyal_medya =~ begeni + takip + pozitif_yorum + negatif_yorum + sosyal_medya_reklamlari
deneyim =~ memnuniyet + tavsiye
deneyim ~ dijital_kanal + sosyal_medya
elde_tutma =~ devam_eden_musteri + terk
elde_tutma ~ deneyim
"

model2 <- "
dijital_kanal =~ hata_islem_sayisi + islem_ortalama_tiklama + cevrim_ici_sure + basarili_giris_sayisi + gezdigi_sayfa_sayisi
sosyal_medya =~ begeni + takip + pozitif_yorum + negatif_yorum + sosyal_medya_reklamlari
deneyim =~ memnuniyet + tavsiye
deneyim ~ dijital_kanal + sosyal_medya
elde_tutma =~ devam_eden_musteri + terk
elde_tutma ~ deneyim
geri_kazanma =~ aktiflestirilmis_musteri + geri_kazanilmis_musteri
geri_kazanma ~ deneyim + elde_tutma
"

model3 <- "
dijital_kanal =~ hata_islem_sayisi + islem_ortalama_tiklama + cevrim_ici_sure + basarili_giris_sayisi + gezdigi_sayfa_sayisi
sosyal_medya =~ begeni + takip + pozitif_yorum + negatif_yorum + sosyal_medya_reklamlari
deneyim =~ memnuniyet + tavsiye
deneyim ~ dijital_kanal + sosyal_medya
buyume =~ capraz_satis
buyume ~ deneyim
elde_tutma =~ devam_eden_musteri + terk
elde_tutma ~ deneyim + buyume
geri_kazanma =~ aktiflestirilmis_musteri + geri_kazanilmis_musteri
geri_kazanma ~ deneyim + elde_tutma
"

model4 <- "
dijital_kanal =~ hata_islem_sayisi + islem_ortalama_tiklama + cevrim_ici_sure + basarili_giris_sayisi + gezdigi_sayfa_sayisi
sosyal_medya =~ begeni + takip
deneyim =~ memnuniyet + tavsiye + pozitif_yorum + negatif_yorum + sosyal_medya_reklamlari + gelir
deneyim ~ dijital_kanal + sosyal_medya
buyume =~ capraz_satis
buyume ~ deneyim
elde_tutma =~ devam_eden_musteri + terk
elde_tutma ~ deneyim + buyume
geri_kazanma =~ aktiflestirilmis_musteri + geri_kazanilmis_musteri
geri_kazanma ~ deneyim + elde_tutma
"

fit4 <- sem(model4, gendata, std.lv = TRUE, std.ov = TRUE,
            control = list(maxit = 100000),
            estimator = "ULS",
            meanstructure = TRUE,
            optim.method = "BFGS",
            verbose=TRUE)
fitmeasures(fit4, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))

semPaths(fit4, "std", nCharNodes = 35, layout="tree",
         intercepts = FALSE, pastel = TRUE, residuals = FALSE, label.prop = 0.93,
         sizeMan = 5.5, sizeLat = 8, font = 4, fade=FALSE, reorder=FALSE)

fit1 <- sem(model1, gendata, std.lv = TRUE, std.ov = TRUE,
            control = list(maxit = 100000),
            estimator = "ULS",
            meanstructure = TRUE,
            optim.method = "BFGS",
            verbose=TRUE)

fit2 <- sem(model2, gendata, std.lv = TRUE, std.ov = TRUE,
            control = list(maxit = 100000),
            estimator = "ULS",
            meanstructure = TRUE,
            optim.method = "BFGS",
            verbose=TRUE)

fit3 <- sem(model3, gendata, std.lv = TRUE, std.ov = TRUE,
            control = list(maxit = 100000),
            estimator = "ULS",
            meanstructure = TRUE,
            optim.method = "BFGS",
            verbose=TRUE)

semPaths(fit3, "std", nCharNodes = 35, layout="tree",
         intercepts = FALSE, pastel = TRUE, residuals = FALSE, label.prop = 0.93,
         sizeMan = 5.5, sizeLat = 8, font = 4, fade=FALSE, reorder=FALSE)

summary(fit1, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)
summary(fit2, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)
summary(fit3, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

parameterestimates(fit3)

sfit <- standardizedsolution(fit1)

sfit[sfit$op == "=~",]

fitmeasures(fit1)

fitmeasures(fit1, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))
fitmeasures(fit2, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))
fitmeasures(fit3, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))


