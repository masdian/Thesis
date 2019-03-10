#   Citeko (96751) - Kab Bogor - Jabar (-6.7	106.85)
#   Jatiwangi (96791) - Majalengka - Jabar (-6.7344	108.263)
#   Bogor (96753) - Kota Bogor - Jabar (-6.55	106.75)
#   Bandung (96783) - Kota Bandung - Jabar (-6.88356	107.59733)
#   Serang (96737) - Kota Serang - Banten (-6.11185	106.11)
#   Mempawah (96583) - Kab Pontianak - Kalbar (0.075	109.19)
#   Sultan Syarif Kasim II (96109) - Pekanbaru - Riau (0.45924	101.44743)
#   Pattimura (97724) - Kota Ambon - Maluku (-3.7114	128.0957)

# fungsi untuk menentukan batas kiri-kanan dan atas-bawah grid 
# suatu lokasi berdasarkan banyaknya grid 0.5 yang diambil

lonlat <- function(x, n){
  a <- floor(x)
  b <- x - a
  if (b > 0.5) {
    l <- a + 0.5
    r <- l + 0.5
    B <- l - ((n%/%2)-1)*0.5
    A <- r + ((n%/%2)-1)*0.5
  } else {
    l <- a
    r <- l + 0.5
    B <- l - ((n%/%2)-1)*0.5
    A <- r + ((n%/%2)-1)*0.5
  }
  m <- data.frame(B = B, A = A)
}

#   Bogor (96753) (-6.55, 106.75) lat=[-8, -5.5] lon=[105.5, 108]
#   Citeko (96751) (-6.7	106.85) lat=[-8, -5.5] lon=[105.5, 108]
#   Jatiwangi (96791) (-6.7344	108.263) lat=[-8, -5.5] lon=[105, 107.5]
#   Bandung (96783) (-6.88356	107.59733) lat=[-8, -5.5] lon=[106.5, 109]
#   Serang (96737) (-6.11185	106.11) lat=[-7.5, -5] lon=[105, 107.5]
#   Mempawah (96583) (0.075	109.19) lat=[-1, 1.5] lon=[108, 110.5]
#   Sultan Syarif Kasim II (96109) (0.45924	101.44743) lat=[-1, 1.5] lon=[100, 102.5]
#   Pattimura (97724) (-3.7114	128.0957) lat=[-5, -2.5] lon=[127, 129.5]

lon <- lonlat(128.0957, 6)
lat <- lonlat(-3.7114, 6)
lon$B ; lon$A
lat$B ; lat$A
x <- seq(lon$B, lon$A, by = 0.5)
y <- seq(lat$B, lat$A, by = 0.5)
xy <- expand.grid(x,y)
plot(xy, type="p", pch = 16, lty=2,lwd=2, axes = T, main = "",xlab = "",ylab = "",xlim = c(min(x)-1, max(x)+1),ylim = c(min(y)-1, max(y)+1))
points(128.0957, -3.7114, pch = 17, col = 4)
for (i in 1:6) {
  segments(min(x)+0.5*(i-1), min(y), min(x)+0.5*(i-1), max(y), lwd = 3)
  segments(min(x), min(y)+0.5*(i-1), max(x), min(y)+0.5*(i-1), lwd = 3)
}
xx <- seq(lon$B-0.25, lon$A+0.25, by = 0.5)
yy <- seq(lat$B-0.25, lat$A+0.25, by = 0.5)
for (i in 1:7) {
  segments(min(xx)+0.5*(i-1), min(yy), min(xx)+0.5*(i-1), max(yy), col = "blue", lwd = 0.5)
  segments(min(xx), min(yy)+0.5*(i-1), max(xx), min(yy)+0.5*(i-1), col = "blue", lwd = 0.5)
}

## batas atas -5.5
## batas bawah -8
## batas kiri 105.5
## batas kanan 108

### mengubah prate_df menjadi GCM
gcm <- prate_df
head(gcm[,1:5])

### mengubah variabel x menjadi lon dan variabel y menjadi lat
colnames(gcm)[1] <- "lon"
colnames(gcm)[2] <- "lat"
head(gcm[,1:5])

#   Bogor (96753) (-6.55, 106.75) lat=[-8, -5.5] lon=[105.5, 108]
library(dplyr)
bogor <- gcm %>%
  filter(lon >= 105.5, lon <= 108, lat <= -5.5, lat >= -8)
## transpose dan membuang lon dan lat
bogor <- t(bogor)
bogor <- bogor[-1:-2,]
## menambahkan tanggal
tgl <- seq(as.Date("1979-01-01"), as.Date("2018-12-31"), by = "days")
bogor <- data.frame(tgl, bogor)

#   Citeko (96751) (-6.7	106.85) lat=[-8, -5.5] lon=[105.5, 108]
citeko <- gcm %>%
  filter(lat >= -8, lat <= -5.5, lon >= 105.5, lon <= 108)
## transpose dan membuang lon dan lat
citeko <- t(citeko)
citeko <- citeko[-1:-2,]
## menambahkan tanggal
citeko <- data.frame(tgl, citeko)

#   Jatiwangi (96791) (-6.7344	108.263) lat=[-8, -5.5] lon=[105, 107.5]
jtwi <- gcm %>%
  filter(lat >= -8, lat <= -5.5, lon >= 105, lon <= 107.5)
## transpose dan membuang lon dan lat
jtwi <- t(jtwi)
jtwi <- jtwi[-1:-2,]
## menambahkan tanggal
jtwi <- data.frame(tgl, jtwi)

#   Bandung (96783) (-6.88356	107.59733) lat=[-8, -5.5] lon=[106.5, 109]
bdg <- gcm %>%
  filter(lat >= -8, lat <= -5.5, lon >=106.5, lon <= 109)
## transpose dan membuang lon dan lat
bdg <- t(bdg)
bdg <- bdg[-1:-2,]
## menambahkan tanggal
bdg <- data.frame(tgl, bdg)

#   Serang (96737) (-6.11185	106.11) lat=[-7.5, -5] lon=[105, 107.5]
srg <- gcm %>%
  filter(lat >= -7.5, lat <= -5, lon >= 105, lon <= 107.5)
## transpose dan membuang lon dan lat
srg <- t(srg)
srg <- srg[-1:-2,]
## menambahkan tanggal
srg <- data.frame(tgl, srg)

#   Mempawah (96583) (0.075	109.19) lat=[-1, 1.5] lon=[108, 110.5]
mph <- gcm %>%
  filter(lat >= -1, lat <= 1.5, lon >= 108, lon <= 110.5)
## transpose dan membuang lon dan lat
mph <- t(mph)
mph <- mph[-1:-2,]
## menambahkan tanggal
mph <- data.frame(tgl, mph)

#   Sultan Syarif Kasim II (96109) (0.45924	101.44743) lat=[-1, 1.5] lon=[100, 102.5]
skii <- gcm %>%
  filter(lat >= -1, lat <= 1.5, lon >= 100, lon <= 102.5)
## transpose dan membuang lon dan lat
skii <- t(skii)
skii <- skii[-1:-2,]
## menambahkan tanggal
skii <- data.frame(tgl, skii)

#   Pattimura (97724) (-3.7114	128.0957) lat=[-5, -2.5] lon=[127, 129.5]
patt <- gcm %>%
  filter(lat >= -5, lat <= -2.5, lon >= 127, lon <= 129.5)
## transpose dan membuang lon dan lat
patt <- t(patt)
patt <- patt[-1:-2,]
## menambahkan tanggal
patt <- data.frame(tgl, patt)

## satuan precipitation rate kg/(m2.s) 
## tidak perlu diubah

####### Membaca data BMKG #####
library(readxl)
bmkg <- read_excel("~/Downloads/Respons.xlsx", 
                    sheet = "Jawa_Barat", 
                    col_types = c("text", "numeric", "text", "skip", "skip", "numeric"))
dim(bmkg)
head(bmkg)
unique(bmkg$Nama)

### mengubah variabel tanggal jadi date
bmkg$Tanggal <- as.Date(bmkg$Tanggal)
library(lubridate)

### membersihkan data dari 8888 dan 9999
bmkg <- bmkg %>% filter(Hujan != 8888, Hujan != 9999)

### memilih periode tahun yang akan digunakan
### misalkan yang akan digunakan adalah 2010-2018
bmkg <- bmkg %>% filter(Tanggal >= "2010-01-01")
head(bmkg)

### data bmkg stasiun Bogor
bmkg_bog <- bmkg %>% filter(ID == 96753) %>% select(Tanggal, Hujan)

### data bmkg stasiun Citeko
bmkg_citeko <- bmkg %>% filter(ID == 96751) %>% select(Tanggal, Hujan)

### data bmkg stasiun Jatiwangi
bmkg_jtwi <- bmkg %>% filter(ID == 96791) %>% select(Tanggal, Hujan)

### data bmkg stasiun Bandung
bmkg_bdg <- bmkg %>% filter(ID == 96783) %>% select(Tanggal, Hujan)

### data bmkg stasiun Serang
bmkg_srg <- bmkg %>% filter(ID == 96737) %>% select(Tanggal, Hujan)

### data bmkg stasiun Mempawah
bmkg_mph <- bmkg %>% filter(ID == 96583) %>% select(Tanggal, Hujan)

### data bmkg stasiun Syarif Kasim II
bmkg_skii <- bmkg %>% filter(ID == 96109) %>% select(Tanggal, Hujan)

### data bmkg stasiun Pattimura
bmkg_patt <- bmkg %>% filter(ID == 97724) %>% select(Tanggal, Hujan)


### menggabungkan data GCM dengan data BMKG dan membuang kolom tgl
dat_bog <- right_join(bogor, bmkg_bog, by = c("tgl" = "Tanggal"))
dat_bog <- dat_bog[,-1]

dat_citeko <- right_join(citeko, bmkg_citeko, by = c("tgl" = "Tanggal"))
dat_citeko <- dat_citeko[,-1]

dat_jtwi <- right_join(jtwi, bmkg_jtwi, by = c("tgl" = "Tanggal"))
dat_jtwi <- dat_jtwi[,-1]

dat_bdg <- right_join(bdg, bmkg_bdg, by = c("tgl" = "Tanggal"))
dat_bdg <- dat_bdg[,-1]

dat_srg <- right_join(srg, bmkg_srg, by = c("tgl" = "Tanggal"))
dat_srg <- dat_srg[,-1]

dat_mph <- right_join(mph, bmkg_mph, by = c("tgl" = "Tanggal"))
dat_mph <- dat_mph[,-1]

dat_skii <- right_join(skii, bmkg_skii, by = c("tgl" = "Tanggal"))
dat_skii <- dat_skii[,-1]

dat_patt <- right_join(patt, bmkg_patt, by = c("tgl" = "Tanggal"))
dat_patt <- dat_patt[,-1]

library(caret)
library(pls)

##### validasi silang menggunakan caret #####
CVPls_caret <- function(data, lipatan, ulangan){
  hasil <- data.frame()
  pls <- train(Hujan ~ . , data = data, method = "pls", scale = T,
               trControl = trainControl("repeatedcv", number = lipatan, repeats = ulangan, savePredictions = T),
               tuneLength = 10)
  rataan <- apply(pls$resample[,1:2],2,mean)
  hasil <- list(rataan = rataan, hasil = pls$resample[,1:2])
}

### Prediksi curah hujan tanpa pengelompokan (langsung)
awal <- Sys.time()
pls_bog <- CVPls_caret(dat_bog, 5, 20)
pls_citeko <- CVPls_caret(dat_citeko, 5, 20)
pls_jtwi <- CVPls_caret(dat_jtwi, 5, 20)
pls_bdg <- CVPls_caret(dat_bdg, 5, 20)
pls_srg <- CVPls_caret(dat_srg, 5, 20)
pls_mph <- CVPls_caret(dat_mph, 5, 20)
pls_skii <- CVPls_caret(dat_skii, 5, 20)
pls_patt <- CVPls_caret(dat_patt, 5, 20)
akhir <- Sys.time()
akhir - awal

rmse_pls <- data.frame(pls_bog$hasil[,1], pls_citeko$hasil[,1], pls_jtwi$hasil[,1],
                       pls_bdg$hasil[,1], pls_srg$hasil[,1], pls_mph$hasil[,1],
                       pls_skii$hasil[,1], pls_patt$hasil[,1])

boxplot(rmse_pls, names = c("Bogor", "Citeko", "Jatiwangi", "Bandung",
                             "Serang", "Mempawah", "SKII", "Pattimura"),
        ylab = "RMSEP", xlab = "Pos Hujan", col = rainbow(8))

##### pembagian kelompok curah hujan
dat_bog_k <- dat_bog
dat_bog_k$q <- ifelse(dat_bog_k$Hujan == 0, 1, ifelse(dat_bog_k$Hujan <= 50, 2, 3))
dat_bog_k$q <- factor(dat_bog_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_bog_k$q))
#data <- dat_bog_k[,-37]   ### akurasi 65-66%

# pembagian berdasar kuartil data
dat_bog_k$q <- ifelse(dat_bog_k$Hujan>0, with(dat_bog_k[dat_bog_k$Hujan>0,], 
                                  findInterval(dat_bog_k[dat_bog_k$Hujan>0,]$Hujan, 
                                               c(-Inf, quantile(dat_bog_k[dat_bog_k$Hujan>0,]$Hujan, 
                                                                probs=c(0.25, .5, .75)), Inf))), 1)
dat_bog_k$q <- factor(dat_bog_k$q, labels = c("Q1", "Q2", "Q3", "Q4"), ordered = T)

dat_bog_k$q <- ifelse(dat_bog_k$Hujan>0, with(dat_bog_k[dat_bog_k$Hujan>0,], 
                                              findInterval(dat_bog_k[dat_bog_k$Hujan>0,]$Hujan, 
                                                           c(-Inf, quantile(dat_bog_k[dat_bog_k$Hujan>0,]$Hujan, 
                                                                            probs=c(0, .5)), Inf))), 1)
dat_bog_k$q <- factor(dat_bog_k$q, labels = c("Q1", "Q2", "Q3"), ordered = T)
prop.table(table(dat_bog_k$q))
data <- dat_bog_k[,-37]      ### akurasi 44-45%

###### citeko ######
dat_citeko_k <- dat_citeko
dat_citeko_k$q <- ifelse(dat_citeko_k$Hujan == 0, 1, ifelse(dat_citeko_k$Hujan <= 50, 2, 3))
dat_citeko_k$q <- factor(dat_citeko_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_citeko_k$q))
#data <- dat_citeko_k[,-37]    #### akurasi 76-77%

##### tidak hujan, dan median
dat_citeko_k$q <- ifelse(dat_citeko_k$Hujan>0, with(dat_citeko_k[dat_citeko_k$Hujan>0,], 
                                              findInterval(dat_citeko_k[dat_citeko_k$Hujan>0,]$Hujan, 
                                                           c(-Inf, quantile(dat_citeko_k[dat_citeko_k$Hujan>0,]$Hujan, 
                                                                            probs=c(0, .5)), Inf))), 1)
dat_citeko_k$q <- factor(dat_citeko_k$q, labels = c("Q1", "Q2", "Q3"), ordered = T)
prop.table(table(dat_citeko_k$q))
data <- dat_citeko_k[,-37]       #### akurasi 43-44%

#### Jatiwangi
dat_jtwi_k <- dat_jtwi
dat_jtwi_k$q <- ifelse(dat_jtwi_k$Hujan == 0, 1, ifelse(dat_jtwi_k$Hujan <= 50, 2, 3))
dat_jtwi_k$q <- factor(dat_jtwi_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_jtwi_k$q))
#data <- dat_jtwi_k[,-37]     #### akurasi 69-70%

##### tidak hujan, dan median
dat_jtwi_k$q <- ifelse(dat_jtwi_k$Hujan>0, with(dat_jtwi_k[dat_jtwi_k$Hujan>0,], 
                                                    findInterval(dat_jtwi_k[dat_jtwi_k$Hujan>0,]$Hujan, 
                                                                 c(-Inf, quantile(dat_jtwi_k[dat_jtwi_k$Hujan>0,]$Hujan, 
                                                                                  probs=c(0, .5)), Inf))), 1)
dat_jtwi_k$q <- factor(dat_jtwi_k$q, labels = c("Q1", "Q2", "Q3"), ordered = T)
prop.table(table(dat_jtwi_k$q))
data <- dat_jtwi_k[,-37]   #### akurasi 58 - 59%

#### Bandung
dat_bdg_k <- dat_bdg
dat_bdg_k$q <- ifelse(dat_bdg_k$Hujan == 0, 1, ifelse(dat_bdg_k$Hujan <= 50, 2, 3))
dat_bdg_k$q <- factor(dat_bdg_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_bdg_k$q))
#data <- dat_bdg_k[,-37]    #### akurasi 75-76%

##### tidak hujan, dan median
dat_bdg_k$q <- ifelse(dat_bdg_k$Hujan>0, with(dat_bdg_k[dat_bdg_k$Hujan>0,], 
                                                findInterval(dat_bdg_k[dat_bdg_k$Hujan>0,]$Hujan, 
                                                             c(-Inf, quantile(dat_bdg_k[dat_bdg_k$Hujan>0,]$Hujan, 
                                                                              probs=c(0, .5)), Inf))), 1)
dat_bdg_k$q <- factor(dat_bdg_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_bdg_k$q))
data <- dat_bdg_k[,-37]    #### akurasi 50-52%

#### Serang
dat_srg_k <- dat_srg
dat_srg_k$q <- ifelse(dat_srg_k$Hujan == 0, 1, ifelse(dat_srg_k$Hujan <= 50, 2, 3))
dat_srg_k$q <- factor(dat_srg_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_srg_k$q))
#data <- dat_srg_k[,-37]    #### 67-68%

##### tidak hujan, dan median
dat_srg_k$q <- ifelse(dat_srg_k$Hujan>0, with(dat_srg_k[dat_srg_k$Hujan>0,], 
                                              findInterval(dat_srg_k[dat_srg_k$Hujan>0,]$Hujan, 
                                                           c(-Inf, quantile(dat_srg_k[dat_srg_k$Hujan>0,]$Hujan, 
                                                                            probs=c(0, .5)), Inf))), 1)
dat_srg_k$q <- factor(dat_srg_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_srg_k$q))
data <- dat_srg_k[,-37]    #### 50-52%

#### Mempawah
dat_mph_k <- dat_mph
dat_mph_k$q <- ifelse(dat_mph_k$Hujan == 0, 1, ifelse(dat_mph_k$Hujan <= 50, 2, 3))
dat_mph_k$q <- factor(dat_mph_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_mph_k$q))
#data <- dat_mph_k[,-37]     #### akurasi 65%

##### tidak hujan, dan median
dat_mph_k$q <- ifelse(dat_mph_k$Hujan>0, with(dat_mph_k[dat_mph_k$Hujan>0,], 
                                              findInterval(dat_mph_k[dat_mph_k$Hujan>0,]$Hujan, 
                                                           c(-Inf, quantile(dat_mph_k[dat_mph_k$Hujan>0,]$Hujan, 
                                                                            probs=c(0, .5)), Inf))), 1)
dat_mph_k$q <- factor(dat_mph_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_mph_k$q))
data <- dat_mph_k[,-37]      #### akurasi 45 - 46%

#### Syarif Kasim II
dat_skii_k <- dat_skii
dat_skii_k$q <- ifelse(dat_skii_k$Hujan == 0, 1, ifelse(dat_skii_k$Hujan <= 50, 2, 3))
dat_skii_k$q <- factor(dat_skii_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_skii_k$q))
#data <- dat_skii_k[,-37]      #### akurasi 61 - 62%

##### tidak hujan, dan median
dat_skii_k$q <- ifelse(dat_skii_k$Hujan>0, with(dat_skii_k[dat_skii_k$Hujan>0,], 
                                              findInterval(dat_skii_k[dat_skii_k$Hujan>0,]$Hujan, 
                                                           c(-Inf, quantile(dat_skii_k[dat_skii_k$Hujan>0,]$Hujan, 
                                                                            probs=c(0, .5)), Inf))), 1)
dat_skii_k$q <- factor(dat_skii_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_skii_k$q))
data <- dat_skii_k[,-37]       #### 40 - 44%

#### Pattimura
dat_patt_k <- dat_patt
dat_patt_k$q <- ifelse(dat_patt_k$Hujan == 0, 1, ifelse(dat_patt_k$Hujan <= 50, 2, 3))
dat_patt_k$q <- factor(dat_patt_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_patt_k$q))
#data <- dat_patt_k[,-37]    #### akurasi 69-69%

##### tidak hujan, dan median
dat_patt_k$q <- ifelse(dat_patt_k$Hujan>0, with(dat_patt_k[dat_patt_k$Hujan>0,], 
                                                findInterval(dat_patt_k[dat_patt_k$Hujan>0,]$Hujan, 
                                                             c(-Inf, quantile(dat_patt_k[dat_patt_k$Hujan>0,]$Hujan, 
                                                                              probs=c(0, .5)), Inf))), 1)
dat_patt_k$q <- factor(dat_patt_k$q, labels = c("K1", "K2", "K3"), ordered = T)
prop.table(table(dat_patt_k$q))
data <- dat_patt_k[,-37]    #### 57-58%


#### menggunakan caret
CVPlsGr <- function(data, lipatan, ulangan){
  hasil_CV <- data.frame()
  hasil <- data.frame()
  for (i in 1:ulangan) {
    #set.seed(i)
    fold <- createFolds(data[, ncol(data)], k = lipatan, list = T)
    for (i in 1:lipatan) {
      latih <- data[-fold[[i]],]
      uji <- data[fold[[i]],]
      mod <- NULL
      for (j in 2:length(unique(data[,ncol(data)]))) {
        dat <- latih[latih[,ncol(latih)]==levels(latih$q)[j],]
        dat <- dat[,-ncol(dat)]
        model <- train(Hujan ~ . , data = dat, method = "pls", scale = T,
                     trControl = trainControl("cv", number = 5, savePredictions = F),
                     tuneLength = 10)
        mod[[levels(latih$q)[j]]] <- model
      }
      for (j in 1:nrow(uji)) {
        if (uji$Hujan[j]==0) {
          uji$pred[j] <- 0 
        } else {
          uji$pred[j] <- predict(mod[[as.character(uji$q[j])]], uji[j,1:36], ncomp = mod[[as.character(uji$q[j])]]$bestTune)
        }
      }
      RMSE <- RMSE(uji$pred, uji$Hujan)
      cor <- cor(uji$pred, uji$Hujan)
      hasil <- cbind(RMSE, cor)
      hasil_CV <- rbind(hasil_CV, hasil)
    }
  }
  rataan <- apply(hasil_CV, 2, mean)
  hasil <- list(rataan = rataan, hasil = hasil_CV)
}

awal <- Sys.time()
plsg_bog <- CVPlsGr(dat_bog_k, 5, 20)
plsg_citeko <- CVPlsGr(dat_citeko_k, 5, 20)
plsg_jtwi <- CVPlsGr(dat_jtwi_k, 5, 20)
plsg_bdg <- CVPlsGr(dat_bdg_k, 5, 20)
plsg_srg <- CVPlsGr(dat_srg_k, 5, 20)
plsg_mph <- CVPlsGr(dat_mph_k, 5, 20)
plsg_skii <- CVPlsGr(dat_skii_k, 5, 20)
plsg_patt <- CVPlsGr(dat_patt_k, 5, 20)
akhir <- Sys.time()
akhir - awal

rmse_plsg <- data.frame(plsg_bog$hasil[,1], plsg_citeko$hasil[,1], plsg_jtwi$hasil[,1],
                       plsg_bdg$hasil[,1], plsg_srg$hasil[,1], plsg_mph$hasil[,1],
                       plsg_skii$hasil[,1], plsg_patt$hasil[,1])
boxplot(rmse_plsg, names = c("Bogor", "Citeko", "Jatiwangi", "Bandung",
                            "Serang", "Mempawah", "SKII", "Pattimura"),
        ylab = "RMSEP", xlab = "Pos Hujan", col = rainbow(8))

pls_hasil <- rbind(pls_bog$rataan, pls_citeko$rataan, pls_jtwi$rataan, pls_bdg$rataan, 
                   pls_srg$rataan, pls_mph$rataan, pls_skii$rataan, pls_patt$rataan)
plsg_hasil <- rbind(plsg_bog$rataan, plsg_citeko$rataan, plsg_jtwi$rataan, plsg_bdg$rataan, 
                   plsg_srg$rataan, plsg_mph$rataan, plsg_skii$rataan, plsg_patt$rataan)
banding <- data.frame(pls_hasil, plsg_hasil)
banding <- banding[,c(1,3,2,4)]
names(banding) <- c("RMSE", "RMSE Grup", "Cor", "Cor Grup")
banding <- data.frame(Stasiun = c("Bogor", "Citeko", "Jatiwangi", "Bandung",
                                  "Serang", "Mempawah", "SKII", "Pattimura"),
                      banding)
banding


########
########    Multi Class
########    Random Forest
########

### Pendekatan One vs All untuk random Forest
RF_OVA <- function(latih, uji) {
  kel <- unique(latih[, ncol(latih)])
  n_kel <- length(kel)
  
  library(dplyr)
  
  y <- NULL
  for (i in 1:n_kel) {
    y[[i]] <- factor(ifelse(latih[,ncol(latih)] == levels(kel)[i], levels(kel)[i], 0), exclude = NA)
  }
  
  mod <- NULL
  for (i in 1 : n_kel) {
    data <- data.frame(latih[, -ncol(latih)], y[[i]])
    assign(paste0("data",i),data)
    model <- randomForest(x = data[,-ncol(data)], y = data[,ncol(data)], data = data)
    assign(paste0("model",i),model)
    mod[[i]] <- model
  }
  
  a <- NULL
  for (i in 1:n_kel) {
    pred <- predict(mod[i], uji, type = "prob")
    pred <- data.frame(pred)
    pred <- data.frame(kelas = colnames(pred), t(pred))
    a <- rbind(a, pred)
  }
  pred <- t(a[,-1])
  
  for (i in 1:n_kel) {
    k <- apply(a[a[,1] == as.character(levels(kel)[i]),-1],2,sum)
    pred <- data.frame(pred, k)
  }
  
  a <- pred
  a$class <- apply(a[,(n_kel*2+1):ncol(a)],1,which.max)
  
  kel_1 <- data.frame(class = 1:n_kel, kelas = levels(kel)[1:n_kel])
  a <- left_join(a, kel_1, by = c("class" = "class"))
  a$kelas
}

######################################################
### Pendekatan One vs All untuk Rotation Forest
RoF_OVA <- function(latih, uji) {
  kel <- unique(latih[, ncol(latih)])
  n_kel <- length(kel)
  
  library(dplyr)
  
  y <- NULL
  for (i in 1:n_kel) {
    y[[i]] <- factor(ifelse(latih[,ncol(latih)] == levels(kel)[i], 1, 0), exclude = NA)
  }
  
  mod <- NULL
  for (i in 1 : n_kel) {
    data <- data.frame(latih[, -ncol(latih)], y[[i]])
    #assign(paste0("data",i),data)
    model <- rotationForest(x = data[,-c((ncol(data)-1),ncol(data))], y = data[,ncol(data)])
    #assign(paste0("model",i),model)
    mod[[i]] <- model
  }
  
  a <- NULL
  for (i in 1:n_kel) {
    pred <- predict(object = mod[i], newdata = uji[,-c((ncol(data)-1),ncol(data))], all = T)
    pred <- data.frame(pred)
    pred1 <- rbind(apply(pred,1,mean), 1-apply(pred,1,mean))
    var <- c(toString(levels(kel)[i]),0)
    pred <- data.frame(kelas = var, pred1)
    a <- rbind(a, pred)
  }
  pred <- t(a[,-1])
  
  for (i in 1:n_kel) {
    k <- apply(a[a[,1] == toString(levels(kel)[i]),-1],2,sum)
    pred <- data.frame(pred, k)
  }
  
  a <- pred
  a$class <- apply(a[,(n_kel*2+1):ncol(a)],1,which.max)
  
  kel_1 <- data.frame(class = 1:n_kel, kelas = levels(kel)[1:n_kel])
  a <- left_join(a, kel_1, by = c("class" = "class"))
  a$kelas
}

### Pendekatan One vs One untuk random forest
RF_OVO <- function(latih, uji) {
  kel <- unique(latih[, ncol(latih)])
  n_kel <- length(kel)
  com <- combn(unique(latih[, ncol(latih)]), 2)
  com <- t(com)
  dat <- data.frame(com)
  dat <- dat[complete.cases(dat),]
  library(dplyr)
  
  mod <- NULL
  for (i in 1 : nrow(dat)) {
    data <- latih %>% filter(latih[, ncol(latih)] == dat[i,1] | latih[, ncol(latih)] == dat[i,2])
    data[,ncol(data)] <- factor(data[,ncol(data)], exclude = NA)
    #assign(paste0("data",i),data)
    model <- randomForest(x = data[,-ncol(data)], y = data[,ncol(data)], data = data)
    #assign(paste0("model",i),model)
    mod[[i]] <- model
  }
  
  a <- NULL
  for (i in 1:nrow(dat)) {
    pred <- predict(mod[i], uji, type = "prob")
    pred <- data.frame(pred)
    pred <- data.frame(kelas = colnames(pred), t(pred))
    a <- rbind(a, pred)
  }
  pred <- t(a[,-1])
  
  for (i in 1:n_kel) {
    k <- apply(a[a[,1] == as.character(levels(kel)[i]),-1],2,sum)
    pred <- data.frame(pred, k)
  }
  
  a <- pred
  a$class <- apply(a[,(nrow(dat)*2+1):ncol(a)],1,which.max)
  
  kel_1 <- data.frame(class = 1:n_kel, kelas = levels(kel)[1:n_kel])
  a <- left_join(a, kel_1, by = c("class" = "class"))
  a$kelas
}


###########################################################
### Pendekatan One vs One untuk Rotation Forest khusus data curah hujan
RoF_OVO <- function(latih, uji) {
  kel <- unique(latih[, ncol(latih)])
  n_kel <- length(kel)
  com <- combn(unique(latih[, ncol(latih)]), 2)
  com <- t(com)
  dat <- data.frame(com)
  dat <- dat[complete.cases(dat),]
  library(dplyr)
  
  mod <- NULL
  for (i in 1 : nrow(dat)) {
    data <- latih %>% filter(latih[, ncol(latih)] == dat[i,1] | latih[, ncol(latih)] == dat[i,2])
    data[,ncol(data)] <- factor(ifelse(data[,ncol(data)] == dat[i,1], 1, 0), exclude = NA)
    #assign(paste0("data",i),data)
    model <- rotationForest(x = data[,-c((ncol(data)-1),ncol(data))], y = data[,ncol(data)])
    #assign(paste0("model",i),model)
    mod[[i]] <- model
  }
  
  a <- NULL
  for (i in 1:nrow(dat)) {
    pred <- predict(object = mod[i], newdata = uji[,-c((ncol(data)-1),ncol(data))], all = T)
    pred <- data.frame(pred)
    pred1 <- rbind(apply(pred,1,mean), 1-apply(pred,1,mean))
    var <- data.frame(dat[i,1], dat[i,2])
    pred <- data.frame(kelas = t(var), pred1)
    a <- rbind(a, pred)
  }
  pred <- t(a[,-1])
  
  for (i in 1:n_kel) {
    k <- apply(a[a[,1] == as.character(levels(kel)[i]),-1],2,sum)
    pred <- data.frame(pred, k)
  }
  
  a <- pred
  a$class <- apply(a[,(nrow(dat)*2+1):ncol(a)],1,which.max)
  
  kel_1 <- data.frame(class = 1:n_kel, kelas = levels(kel)[1:n_kel])
  a <- left_join(a, kel_1, by = c("class" = "class"))
  a$kelas
}

### model random forest biasa
rf <- randomForest(x = latih[,1:(ncol(latih)-1)], y = latih[,ncol(latih)], data = latih)
pred_rf <- predict(rf, uji)


#### Validasi silang RF, OVO, dan OVA

CVRF <- function(data, lipatan, ulangan){
  hasil <- data.frame()
  hasil_CV <- data.frame()
  for (i in 1 : ulangan) {
    fold <- createFolds(data[,ncol(data)], k = lipatan, list = T)
    for (j in 1 : lipatan) {
      latih <- data[-fold[[j]],]
      uji <- data[fold[[j]],]
      
      pred_OVA <- RF_OVA(latih = latih[,-(ncol(latih)-1)], uji = uji[,-(ncol(uji)-1)])
      pred_RoFOVA <- RoF_OVA(latih = latih[,-(ncol(latih)-1)], uji = uji[,c(-(ncol(uji)-1),-ncol(uji))])
      pred_OVO <- RF_OVO(latih = latih[,-(ncol(latih)-1)], uji = uji[,-(ncol(uji)-1)])
      pred_RoFOVO <- RoF_OVO(latih = latih[,-(ncol(latih)-1)], uji = uji[,c(-(ncol(uji)-1),-ncol(uji))])
      rf <- randomForest(x = latih[,1:(ncol(latih)-2)], 
                         y = latih[,ncol(latih)],
                         data = latih)
      pred_rf <- predict(rf, uji)
      
      ### akurasi 
      acc_ova <- confusionMatrix(pred_OVA, uji[,ncol(uji)])$overall[[1]]
      acc_RoFova <- confusionMatrix(pred_RoFOVA, uji[,ncol(uji)])$overall[[1]]
      acc_ovo <- confusionMatrix(pred_OVO, uji[,ncol(uji)])$overall[1]
      acc_RoFovo <- confusionMatrix(pred_RoFOVO, uji[,ncol(uji)])$overall[1]
      acc_rf <- confusionMatrix(pred_rf, uji[,ncol(uji)])$overall[1]
      
      
      hasil <- cbind(acc_rf, acc_ova, acc_ovo, acc_RoFova, acc_RoFovo)
      hasil_CV <- rbind(hasil_CV, hasil)
    }
  }
  rataan <- apply(hasil_CV, 2, mean)
  hasil <- list(rataan = rataan, hasil = hasil_CV)
}

CVRF_bog <- CVRF(dat_bog_k, 5, 20)
CVRF_citeko <- CVRF(dat_citeko_k, 5, 20)
CVRF_jtwi <- CVRF(dat_jtwi_k, 5, 20)
CVRF_bdg <- CVRF(dat_bdg_k, 5, 20)
CVRF_srg <- CVRF(dat_srg_k, 5, 20)
CVRF_mph <- CVRF(dat_mph_k, 5, 20)
CVRF_skii <- CVRF(dat_skii_k, 5, 20)
CVRF_patt <- CVRF(dat_patt_k, 5, 20)

CVRF_hasil <- rbind(CVRF_bog$rataan, CVRF_citeko$rataan, CVRF_jtwi$rataan, CVRF_bdg$rataan, 
                    CVRF_srg$rataan, CVRF_mph$rataan, CVRF_skii$rataan, CVRF_patt$rataan)
CVRF_hasil
akurasi_bog <- CVRF_bog$hasil
akurasi_bog$winner <- apply(akurasi_bog,1,which.max)
prop.table(table(akurasi_bog$winner))
CVRF_bog$rataan


#### Validasi silang klasifikasi dan RKTP

CVRFPls <- function(data, lipatan, ulangan){
  hasil <- data.frame()
  hasil_CV <- data.frame()
  for (i in 1 : ulangan) {
    fold <- createFolds(data[,ncol(data)], k = lipatan, list = T)
    for (j in 1 : lipatan) {
      latih <- data[-fold[[j]],]
      uji <- data[fold[[j]],]
      mod <- NULL
      for (j in 2:length(unique(data[,ncol(data)]))) {
        dat <- latih[,-ncol(latih)]
        #model <- plsr(Hujan ~ ., data = dat, validation = "CV", scale = T)
        model <- train(Hujan ~ . , data = dat, method = "pls", scale = T,
                     trControl = trainControl("cv", number = 5),
                     tuneLength = 10)
        mod[[levels(latih$q)[j]]] <- model
      }
      pred_OVA <- RF_OVA(latih = latih[,-(ncol(latih)-1)], uji = uji[,-(ncol(uji)-1)])
      pred_RoFOVA <- RoF_OVA(latih = latih[,-(ncol(latih)-1)], uji = uji[,c(-(ncol(uji)-1),-ncol(uji))])
      pred_OVO <- RF_OVO(latih = latih[,-(ncol(latih)-1)], uji = uji[,-(ncol(uji)-1)])
      pred_RoFOVO <- RoF_OVO(latih = latih[,-(ncol(latih)-1)], uji = uji[,c(-(ncol(uji)-1),-ncol(uji))])
      rf <- randomForest(x = latih[,1:(ncol(latih)-2)], 
                         y = latih[,ncol(latih)],
                         data = latih)
      pred_rf <- predict(rf, uji)
      ### akurasi 
      acc_ova <- confusionMatrix(pred_OVA, uji[,ncol(uji)])$overall[[1]]
      acc_rova <- confusionMatrix(pred_RoFOVA, uji[,ncol(uji)])$overall[[1]]
      acc_ovo <- confusionMatrix(pred_OVO, uji[,ncol(uji)])$overall[1]
      acc_rovo <- confusionMatrix(pred_RoFOVO, uji[,ncol(uji)])$overall[1]
      acc_rf <- confusionMatrix(pred_rf, uji[,ncol(uji)])$overall[1]
      
      ### Prediksi curah hujan random forest OVA
      pls_OVA <- NULL
      for (j in 1:nrow(uji)) {
        if (pred_OVA[j]==levels(uji$q)[1]) {
          pls_OVA[j] <- 0 
        } else {
          pls_OVA[j] <- predict(mod[[as.character(pred_OVA[j])]], uji[j,1:36], ncomp = mod[[as.character(uji$q[j])]]$bestTune)
        }
      }
      RMSE_OVA <- RMSE(pls_OVA, uji[,ncol(uji)-1])
      cor_OVA <- cor(pls_OVA, uji[,ncol(uji)-1])
      
      ### Prediksi curah hujan rotation forest OVA
      pls_ROVA <- NULL
      for (j in 1:nrow(uji)) {
        if (pred_RoFOVA[j]==levels(uji$q)[1]) {
          pls_ROVA[j] <- 0 
        } else {
          pls_ROVA[j] <- predict(mod[[as.character(pred_RoFOVA[j])]], uji[j,1:36], ncomp = mod[[as.character(uji$q[j])]]$bestTune)
        }
      }
      RMSE_ROVA <- RMSE(pls_ROVA, uji[,ncol(uji)-1])
      cor_ROVA <- cor(pls_ROVA, uji[,ncol(uji)-1])
      
      ### Prediksi curah hujan OVO
      pls_OVO <- NULL
      for (j in 1:nrow(uji)) {
        if (pred_OVO[j]==levels(uji$q)[1]) {
          pls_OVO[j] <- 0 
        } else {
          pls_OVO[j] <- predict(mod[[as.character(pred_OVO[j])]], uji[j,1:36], ncomp = mod[[as.character(uji$q[j])]]$bestTune)
        }
      }
      RMSE_OVO <- RMSE(pls_OVO, uji[,ncol(uji)-1])
      cor_OVO <- cor(pls_OVO, uji[,ncol(uji)-1])
      
      ### Prediksi curah hujan rotation forest OVO
      pls_ROVO <- NULL
      for (j in 1:nrow(uji)) {
        if (pred_RoFOVO[j]==levels(uji$q)[1]) {
          pls_ROVO[j] <- 0 
        } else {
          pls_ROVO[j] <- predict(mod[[as.character(pred_RoFOVO[j])]], uji[j,1:36], ncomp = mod[[as.character(uji$q[j])]]$bestTune)
        }
      }
      RMSE_ROVO <- RMSE(pls_ROVO, uji[,ncol(uji)-1])
      cor_ROVO <- cor(pls_ROVO, uji[,ncol(uji)-1])
      
      ### Prediksi curah hujan reguler random forest
      pls_rf <- NULL
      for (j in 1:nrow(uji)) {
        if (pred_rf[j]==levels(uji$q)[1]) {
          pls_rf[j] <- 0 
        } else {
          pls_rf[j] <- predict(mod[[as.character(pred_rf[j])]], uji[j,1:36], ncomp = mod[[as.character(uji$q[j])]]$bestTune)
        }
      }
      RMSE_RF <- RMSE(pls_rf, uji[,ncol(uji)-1])
      cor_RF <- cor(pls_rf, uji[,ncol(uji)-1])
      
      hasil <- cbind(acc_rf, acc_ova, acc_rova, acc_ovo, acc_rovo, RMSE_RF, RMSE_OVA, 
                     RMSE_ROVA, RMSE_OVO, RMSE_ROVO, cor_RF, cor_OVA, cor_ROVA, cor_OVO, cor_ROVO)
      hasil_CV <- rbind(hasil_CV, hasil)
    }
  }
  rataan <- apply(hasil_CV, 2, mean)
  hasil <- list(rataan = rataan, hasil = hasil_CV)
}

plsrf_bog <- CVRFPls(dat_bog_k, 5, 20)
plsrf_citeko <- CVRFPls(dat_citeko_k, 5, 20)
plsrf_jtwi <- CVRFPls(dat_jtwi_k, 5, 20)
plsrf_bdg <- CVRFPls(dat_bdg_k, 5, 20)
plsrf_srg <- CVRFPls(dat_srg_k, 5, 20)
plsrf_mph <- CVRFPls(dat_mph_k, 5, 20)
plsrf_skii <- CVRFPls(dat_skii_k, 5, 20)
plsrf_patt <- CVRFPls(dat_patt_k, 5, 20)

plsrf_hasil <- rbind(plsrf_bog$rataan, plsrf_citeko$rataan, plsrf_jtwi$rataan, plsrf_bdg$rataan, 
                   plsrf_srg$rataan, plsrf_mph$rataan, plsrf_skii$rataan, plsrf_patt$rataan)

banding <- data.frame(pls_hasil, plsg_hasil)
banding <- banding[,c(1,3,2,4)]
names(banding) <- c("RMSE", "RMSE Grup", "Cor", "Cor Grup")
plsrf_hasil <- data.frame(Stasiun = c("Bogor", "Citeko", "Jatiwangi", "Bandung",
                                  "Serang", "Mempawah", "SKII", "Pattimura"),
                      plsrf_hasil)
plsrf_hasil



