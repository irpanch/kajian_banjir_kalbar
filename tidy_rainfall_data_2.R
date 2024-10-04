# remove list history
rm(list=ls())

# load library ------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(hydroTSM)
library(plotly)
library(lattice)
require(lattice)
library(ggbreak)

# setting label -----------------------------------------------------------

dbt <- "Debit"
hjn <- "Hujan"
dbt_title <- expression("Debit Harian"~(m^3/dtk))
dbt_labs <- expression("Debit"~(m^3/dtk))
hjn_title <- "Nilai Curah Hujan (mm)"
hjn_labs <- "Curah Hujan (mm)"

jenis_data <- hjn        # rubah sesuai jenis data yang ada, hujan atau debit?
judul_data <- hjn_title  # rubah sesuai jenis data yang ada, hujan atau debit?
judul_label <- hjn_labs  # rubah sesuai jenis data yang ada, hujan atau debit?

# import and checking data  -----------------------------------------------

# import data
data <- read.csv("code/rekap_hujan_harian.csv")
str(data) # cek struktur data

# rubah format kolom satu menjadi tanggal (semula karakter)
data$Date <- as.Date(data[,1],format="%d-%b-%y")


# cek nama kolom
names(data)

# rubah data sta. yang masih karakter  menjadi numerik
data[,2] <- as.numeric(as.character(data[,2]))
data_date <- as.POSIXct(data$Date)

write.csv(data,"output/data_clean.csv")


summary(data)
g <- ggplot(data,aes(x=Date, y=rahadi_oesman))+geom_line(col="blue3") + 
  labs(title=judul_data,subtitle = "Sta_Rahadi Oesman",x = "Waktu",y=judul_label) +
  theme_update(plot.title=element_text(hjust=0.5))+
  theme_update(plot.subtitle=element_text(hjust=0.5))+
  theme_update(axis.title.y=element_text(angle=90))



g2 <- g + scale_x_date(
  breaks = seq(min(data$Date),max(data$Date),length=6),date_labels = "%b-%y")

g2

index_max <- data[,2] == max(data[,2])
index_min <- data[,2] == min(data[,2])

# buat grafik time series data dengan ggplot
library(lubridate)

## tambah kolom hari, tahun, bulan
data$DOY <- as.numeric(yday(data$Dates))
data$YEAR <- as.numeric(format(data$Dates,"%Y"))
data$MONTH <- as.numeric(format(data$Dates,"%m"))

# set matrixplot ----------------------------------------------------------
# set matrixplot
zoo_rr_data <- zoo(data[,2],data$Date) 

sapply(zoo_rr_data, class)

smry(zoo_rr_data)

# extract jumlah hujan bulanan ----------------------------------------------------
# extract total monthly precipitation
## sta 1
bulanan_zoo_rr_data <- daily2monthly(zoo_rr_data, FUN=sum, na.rm = T)
bulanan_rata2_zoo <- daily2monthly(zoo_rr_data, FUN=mean, na.rm = T)



# matrixplot sta1 -----------------------------------------------------------------
mat <- matrix(bulanan_zoo_rr_data, ncol=12, byrow = T)
mat_rata2 <- matrix(bulanan_rata2_zoo, ncol=12, byrow = T)
colnames(mat) <- month.abb
rownames(mat) <- unique(format(time(bulanan_zoo_rr_data), "%Y"))

colnames(mat_rata2) <- month.abb
rownames(mat_rata2) <- unique(format(time(bulanan_rata2_zoo), "%Y"))

# matrixplot untuk jumlah curah hujan
matrixplot_jml_hjn <- print(matrixplot(mat, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan (mm) Sta. Rahadi Oesman")) #ganti judul kalau data hujan.



# matrixplot untuk rata-rata curah hujan
matrixplot_rata2_hjn <- print(matrixplot(mat_rata2, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Rahadi Oesman")) #ganti judul kalau data hujan.

write.csv(mat,"output/jumlah_hujan_bulanan_rahadi_oesman.csv")
write.csv(mat_rata2,"output/rata2_hujan_bulanan_rahadi_oesman.csv")


