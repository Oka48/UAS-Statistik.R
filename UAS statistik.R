# Nama: Basoka Akbar Manurung/1915051068

install.packages("ggplot2")
install.packages("car")

# Membuat dataset baru di R
data <- data.frame(
  ID = 1:15,
  Jam_Belajar = c(1.8, 2.1, 3.2, 4.5, 5.5, 2.6, 6.0, 3.8, 4.2, 2.9, 3.6, 5.2, 6.5, 4.8, 2.3),
  Nilai_Ujian = c(65, 70, 72, 80, 85, 68, 90, 78, 75, 71, 77, 84, 92, 79, 70)
)

# Mengkategorikan "Jam Belajar" menjadi kelompok: Sedikit, Cukup, dan Banyak
data$Kategori_Jam_Belajar <- cut(
  data$Jam_Belajar,
  breaks = c(0, 2.5, 4.5, 6.5),
  labels = c("Sedikit", "Cukup", "Banyak")
)

# Memuat pustaka yang dibutuhkan
library(ggplot2)
library(car)

# Uji Asumsi 1: Normalitas (Shapiro-Wilk test)
anova_groups <- split(data$Nilai_Ujian, data$Kategori_Jam_Belajar)
normality_results <- lapply(anova_groups, shapiro.test)

# Uji Asumsi 2: Homogenitas Varians (Levene's Test)
levene_test <- leveneTest(Nilai_Ujian ~ Kategori_Jam_Belajar, data = data)

# Uji Asumsi 3: Kesamaan rata-rata (One-Way ANOVA)
anova_result <- aov(Nilai_Ujian ~ Kategori_Jam_Belajar, data = data)
anova_summary <- summary(anova_result)

# Visualisasi Boxplot
ggplot(data, aes(x = Kategori_Jam_Belajar, y = Nilai_Ujian)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Distribusi Nilai Ujian Berdasarkan Kategori Jam Belajar") +
  xlab("Kategori Jam Belajar") +
  ylab("Nilai Ujian") +
  theme_minimal()

# Output summary
cat("Hasil Uji Asumsi dan Analisis ANOVA:\n\n")

cat("Normalitas:\n")
for (group in names(normality_results)) {
  cat(sprintf("  - %s: Statistic = %.3f, p-value = %.3f\n", 
              group, normality_results[[group]]$statistic, normality_results[[group]]$p.value))
}

cat("\nHomogenitas Varians:\n")
print(levene_test)

cat("\nHasil ANOVA:\n")
print(anova_summary)

# -----------------interpretasi---------------------------
cat("\nInterpretasi Deskriptif:\n")

# Normalitas
cat("Hasil uji normalitas menunjukkan:\n")
for (group in names(normality_results)) {
  cat(sprintf("  - Grup %s memiliki nilai p sebesar %.3f\n", 
              group, normality_results[[group]]$p.value))
}

# Homogenitas Varians
cat("\nHasil uji homogenitas varians:\n")
cat(sprintf("  - Nilai p Levene's Test adalah %.3f\n", levene_test$`Pr(>F)`[1]))

# Kesamaan Rata-rata (ANOVA)
cat("\nHasil uji kesamaan rata-rata (ANOVA):\n")
anova_pvalue <- summary(anova_result)[[1]][["Pr(>F)"]][1]
cat(sprintf("  - Nilai p ANOVA adalah %.3f\n", anova_pvalue))

cat("\nCatatan:\n")
cat("  - Jika nilai p < 0.05, hasil uji menunjukkan perbedaan signifikan atau pelanggaran asumsi.\n")
cat("  - Jika nilai p > 0.05, tidak ada perbedaan signifikan atau asumsi terpenuhi.\n")
