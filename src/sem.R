library("lavaan")
library("semPlot")

gendata <- read.csv(file="../data/generated.csv", header=TRUE, sep="\t")

model1 <- "
#klasik_kanal =~ atm_kullanim + sube_kullanim + sube_sure
dijital_kanal =~ basarili_giris_sayisi + gezdigi_sayfa_sayisi + hata_ile_sonlanan_islem_sayisi + bir_islem_icin_ortalama_tiklama_sayisi + cevrim_ici_sure
#musteri_durum =~ yeni_musteri + capraz_satis + yukari_satis +devam_eden_musteri + terk + geri_kazanilmis_musteri + aktiflestirilmis_musteri
deneyim ~ dijital_kanal# + klasik_kanal
deneyim =~ tavsiye + memnuniyet
#musteri_durum ~ deneyim
"

model2 <- "
klasik_kanal =~ atm_kullanim + sube_kullanim + sube_sure
dijital_kanal =~ basarili_giris_sayisi + gezdigi_sayfa_sayisi + hata_ile_sonlanan_islem_sayisi + bir_islem_icin_ortalama_tiklama_sayisi + cevrim_ici_sure
#musteri_durum =~ yeni_musteri + capraz_satis + yukari_satis +devam_eden_musteri + terk + geri_kazanilmis_musteri + aktiflestirilmis_musteri
deneyim ~ dijital_kanal + klasik_kanal
deneyim =~ memnuniyet + tavsiye
#musteri_durum ~ deneyim
"

model3 <- "
#klasik_kanal =~ atm_kullanim + sube_kullanim + sube_sure
dijital_kanal =~ basarili_giris_sayisi + gezdigi_sayfa_sayisi + hata_ile_sonlanan_islem_sayisi + bir_islem_icin_ortalama_tiklama_sayisi + cevrim_ici_sure
deneyim =~ memnuniyet + tavsiye
deneyim ~ dijital_kanal # + klasik_kanal
kazanma =~ yeni_musteri
buyutme =~ capraz_satis + yukari_satis
elde_tutma =~ devam_eden_musteri + terk
geri_kazanma =~ geri_kazanilmis_musteri + aktiflestirilmis_musteri
kazanma ~ deneyim
buyutme ~ kazanma + deneyim
elde_tutma ~ buyutme + deneyim
geri_kazanma ~ elde_tutma + deneyim
"

model4 <- "
klasik_kanal =~ atm_kullanim + sube_kullanim + sube_sure
dijital_kanal =~ basarili_giris_sayisi + gezdigi_sayfa_sayisi + hata_ile_sonlanan_islem_sayisi + bir_islem_icin_ortalama_tiklama_sayisi + cevrim_ici_sure
deneyim =~  memnuniyet + tavsiye + yeni_musteri + devam_eden_musteri + terk + geri_kazanilmis_musteri + aktiflestirilmis_musteri + capraz_satis + yukari_satis
deneyim ~ dijital_kanal + klasik_kanal
klasik_kanal ~ klasik_kanal
"

# musteri_profil =~ ay + kredi_karti_aldi + kredi_karti_sahibi + kredi_karti_ilk_ay_harcama + kredi_karti_harcama + onceki_ay_kredi_karti_harcama + toplam_varlik + toplam_varlik_gecen_ay + toplam_varlik_onceki_3_ay_ortalama + acik_kredi + onceki_ay_urun_sayisi + urun_sayisi

fit1 <- sem(model4, gendata, std.lv = TRUE, std.ov = TRUE,
            #control = list(iter.max = 200000, eval.max = 200000),
            control = list(maxit = 100000),
            estimator = "ULS",
            meanstructure = TRUE,
            optim.method = "BFGS",
            verbose=TRUE)

semPaths(fit1, "std", nCharNodes = 35, layout="tree",
         intercepts = FALSE, pastel = TRUE, width = 40, residuals = FALSE,
         sizeMan = 5.5, sizeLat = 7, font = 32)
lavInspect(fit1, "optim.gradient")
summary(fit1, fit.measures=TRUE, standardized=TRUE)

parameterestimates(fit1)

sfit <- standardizedsolution(fit1)

sfit[sfit$op == "=~",]

fitmeasures(fit1)

fitmeasures(fit1, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))

