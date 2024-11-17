setwd("D:/ANDAT")
# no. 2
#b. read your data
data <- read.csv("online_course_engagement_data.csv")
#c.	Check the packaging
library(ggplot2)
library(dplyr)
library(ggplot2)
health_data <- data %>% filter(CourseCategory == "Health")
dim(health_data) # Menghitung jumlah baris dan kolom
nrow(health_data) # Jumlah baris (jumlah observasi)
ncol(health_data) # Jumlah kolom (jumlah variabel)
# Analisis struktur data
str(health_data)
# d.Look at the top and the bottom of your data  
# Melihat head data (data teratas)
head(health_data)
# Melihat tail data (data terbawah)
tail(health_data)
# e.	Check your “n”s f. Validate with at least one external data source 
# Periksa jumlah data di seluruh data set
nrow(data)
# Periksa jumlah data setelah filter (kategori Health)
nrow(health_data)
# Melihat distribusi jumlah video yang ditonton
table(health_data$NumberOfVideosWatched)
# Melihat distribusi skor kuis
summary(health_data$QuizScores)
# Contoh penyaringan data berdasarkan kriteria tertentu
# Filter data berdasarkan kondisi tertentu
filtered_data <- health_data %>%
  filter(NumberOfVideosWatched > 10 & QuizScores > 60)
# Pilih kolom untuk diperiksa
selected_data <- filtered_data %>%
  select(UserID, NumberOfVideosWatched, QuizScores, CompletionRate)
# Tampilkan data yang difilter
print(selected_data)
#f.	Validate with at least one external data source
# Menampilkan ringkasan statistik untuk variabel relevan
summary(health_data[, c("NumberOfVideosWatched", "QuizScores", "CompletionRate")])
# Menghitung kuartil untuk variabel "CompletionRate", "NumberOfVideosWatched", dan "QuizScores" (kategori Health)
quartiles_health <- health_data %>%
  summarise(
    CompletionRate_Q1 = quantile(CompletionRate, 0.25),
    CompletionRate_Q2 = quantile(CompletionRate, 0.50),
    CompletionRate_Q3 = quantile(CompletionRate, 0.75),
    VideosWatched_Q1 = quantile(NumberOfVideosWatched, 0.25),
    VideosWatched_Q2 = quantile(NumberOfVideosWatched, 0.50),
    VideosWatched_Q3 = quantile(NumberOfVideosWatched, 0.75),
    QuizScores_Q1 = quantile(QuizScores, 0.25),
    QuizScores_Q2 = quantile(QuizScores, 0.50),
    QuizScores_Q3 = quantile(QuizScores, 0.75)
  )
# Menampilkan hasil
print(quartiles_health)
#g.	Make a plot (gunakan package ggplot2) 
# Membuat scatter plot dengan ggplot2
ggplot(health_data, aes(x = NumberOfVideosWatched, y = CompletionRate, size = QuizScores, color = QuizScores)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_c() +
  labs(
    title = "hubungan antara Videos Watched, Quiz Scores, and Completion Rate (Health)",
    x = "Number of Videos Watched",
    y = "Completion Rate",
    size = "Quiz Scores",
    color = "Quiz Scores"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#h.	Try the easy solution first 
# Model 1: Pengaruh NumberOfVideosWatched terhadap CompletionRate
model1 <- lm(CompletionRate ~ NumberOfVideosWatched, data = health_data)
summary(model1)
# Model 2: Pengaruh QuizScores terhadap CompletionRate
model2 <- lm(CompletionRate ~ QuizScores, data = health_data)
summary(model2)
# Mengelompokkan NumberOfVideosWatched menjadi kategori (diskrit)
health_data <- health_data %>%
  mutate(VideoGroup = cut(NumberOfVideosWatched, 
                          breaks = c(0, 5, 10, 15, 20), 
                          labels = c("0-5", "6-10", "11-15", "16-20")))
# Membuat boxplot dengan facet_wrap
ggplot(health_data, aes(x = VideoGroup, y = CompletionRate, fill = VideoGroup)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Boxplot of Completion Rate by Video Groups and Quiz Groups (Health Category)",
    x = "Number of Videos Watched (Grouped)",
    y = "Completion Rate"
  ) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(~cut(QuizScores, breaks = c(0, 50, 70, 90, 100), 
                  labels = c("0-50", "51-70", "71-90", "91-100")), 
             labeller = label_both) + # Membuat facet berdasarkan kelompok QuizScores
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#no. 3
# a. •	Menerapkan model normal
# Parameter distribusi normal
set.seed(123)  # Agar hasil reproduktif
n_sim <- 1000  # Jumlah data simulasi
mean_completion <- 50  # Rata-rata CompletionRate (asumsi)
sd_completion <- 15    # Standar deviasi CompletionRate (asumsi)
# Simulasi CompletionRate
simulated_completion <- rnorm(n_sim, mean = mean_completion, sd = sd_completion)
# Membuat data frame untuk visualisasi
sim_data <- data.frame(CompletionRate = simulated_completion)
# Visualisasi histogram distribusi normal
ggplot(sim_data, aes(x = CompletionRate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean_completion, sd = sd_completion), 
                color = "red", size = 1.2) +
  labs(
    title = "Simulated Normal Distribution of Completion Rate",
    x = "Completion Rate",
    y = "Density"
  ) +
  theme_minimal()
#b.	buat histogram variabel dependen dan bandingkan dengan histogram data yang berdistribusi normal! 
# Filter data untuk kategori Health
health_data <- data %>% filter(CourseCategory == "Health")
# Ambil kolom CompletionRate
completion_rate <- health_data$CompletionRate
# Hitung rata-rata dan standar deviasi
mean_completion <- mean(completion_rate)
sd_completion <- sd(completion_rate)
# Simulasi distribusi normal berdasarkan rata-rata dan standar deviasi
simulated_data <- rnorm(1000, mean = mean_completion, sd = sd_completion)
#c.	Reacting to Data: 
# Gabungkan data aktual dan simulasi untuk plot
combined_data <- data.frame(
  CompletionRate = c(completion_rate, simulated_data),
  Type = c(rep("Actual Data", length(completion_rate)), rep("Simulated Normal", length(simulated_data)))
)
# Plot distribusi
ggplot(combined_data, aes(x = CompletionRate, fill = Type)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Actual vs Simulated Normal Distribution (Completion Rate)",
    x = "Completion Rate",
    y = "Density"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#no. 3 
# i. i.	Apakah perlu memperbaiki model ekspektasi? Jika tidak, berilah keterangannya!
# Gabungkan data aktual dan simulasi untuk plot
combined_data <- data.frame(
  CompletionRate = c(completion_rate, simulated_data),
  Type = c(rep("Actual Data", length(completion_rate)), rep("Simulated Normal", length(simulated_data)))
)
# Plot distribusi
ggplot(combined_data, aes(x = CompletionRate, fill = Type)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Actual vs Simulated Normal Distribution (Completion Rate)",
    x = "Completion Rate",
    y = "Density"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Uji normalitas (Shapiro-Wilk test)
shapiro_test <- shapiro.test(health_data$CompletionRate)
print(shapiro_test)
install.packages("fitdistrplus")
library(fitdistrplus) # Untuk distribusi
# Fit distribusi log-normal
fit_lognormal <- fitdist(health_data$CompletionRate, "lnorm")
summary(fit_lognormal)
# Tambahkan kategori "Tinggi" dan "Rendah" untuk QuizScores
health_data <- health_data %>%
  mutate(QuizScoreCategory = ifelse(QuizScores > median(QuizScores), "High", "Low"))
# Plot distribusi CompletionRate berdasarkan kategori QuizScores
ggplot(health_data, aes(x = CompletionRate, fill = QuizScoreCategory)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribusi Completion Rate Berdasarkan Quiz Scores",
       x = "Completion Rate", y = "Density", fill = "Quiz Score Category") +
  theme_minimal()
