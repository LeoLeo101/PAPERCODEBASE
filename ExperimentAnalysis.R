# =============================================================================
# Setup
# =============================================================================

rm(list = ls())

library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)
library(e1071)
library(broom)
library(gridExtra)
library(stringr)
library(readr)     
library(knitr)       




# Read data ---------------------------------------------------------------
#dfraw <- read.csv("C: ...... ArtificialFinancialMarketWithPassiveInvestors experiment-table.csv", skip = 6)

df <- dfraw %>%
  filter(step>=100)

summary(df)
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    )
)


mean_cl_normal <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  z <- qnorm(0.5 + conf/2)
  data.frame(y = m, ymin = m - z * se, ymax = m + z * se)
}

# =============================================================================
# 1) H1 Volatility vs B&H share
# =============================================================================

dfHP1_1 <- df %>%
  filter(X.step. == 3000) %>%
  select(share.buy.hold, meanVOl) %>%
  group_by(share.buy.hold) %>%
  summarise(meanVol = mean(meanVOl), .groups = "drop")

ggplot(dfHP1_1, aes(x = share.buy.hold, y = meanVol)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Relationship between Buy & Hold share and cumulated volatility",
    x = "Buy & Hold Share",
    y = "Mean cumulated Volatility"
  ) +
  theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10))
  )

# 1.2 Rolling volatility distribution by B&H share 
dfHP1_2 <- df %>%
  group_by(share.buy.hold, X.run.number.) %>%
  mutate(
    vol_roll_mean = rollapplyr(
      volatility.indicator, width = 500, FUN = mean,
      fill = NA, align = "right"
    )
  ) %>%
  ungroup()

ggplot(
  dfHP1_2 %>% filter(!is.na(vol_roll_mean)),
  aes(x = factor(share.buy.hold), y = vol_roll_mean, fill = factor(share.buy.hold))
) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2, width = 0.7) +
  # Mean across runs for each B&H share (overlaid line and points)
  stat_summary(fun = mean, aes(group = 1), geom = "line", color = "black", linewidth = 1) +
  stat_summary(fun = mean, aes(group = 1), geom = "point", color = "black", size = 2) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  guides(fill = "none") +
  labs(x = "B&H Share", y = "Volatility (rolling mean W = 500)") +
  theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10))
  )

# 1.2 (alt view) Spaghetti plot of runs + per-share average line
df_line <- df %>%
  group_by(share.buy.hold, X.run.number.) %>%
  mutate(
    vol_roll_mean = rollapplyr(
      volatility.indicator, width = 500, FUN = mean,
      fill = NA, align = "right"
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(vol_roll_mean))

# Custom color palette per B&H level 
custom_cols <- c(
  "0"    = "black",
  "0.01" = "red",
  "0.05" = "blue",
  "0.1"  = "darkgreen",
  "0.2"  = "green",
  "0.3"  = "gray",
  "0.4"  = "orange"
)

ggplot(
  df_line,
  aes(x = X.step., y = vol_roll_mean,
      color = factor(share.buy.hold),
      group = interaction(share.buy.hold, X.run.number.))
) +
  geom_line(alpha = 0.25) +
  stat_summary(aes(group = share.buy.hold),
               fun = mean, geom = "line", linewidth = 1.1) +
  scale_color_manual(values = custom_cols, name = "B&H Share") +
  labs(y = "Volatility (rolling mean W = 500)", x = "Tick") +
  theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10))
  )

# 1.3  Linear dose response: volatility-indicator ~ B&H share
m1 <- lm(volatility.indicator ~ share.buy.hold, data = df)
summary(m1)
broom::tidy(m1)
broom::glance(m1)

# =============================================================================
# 2) H2 Volatility clustering: ACF of |returns|
# =============================================================================

# For each run and B&H share, compute ACF of |returns| for lags 1..5
feat_H2 <- df %>%
  group_by(share.buy.hold, X.run.number.) %>%
  summarise(
    acf_vals = list(acf(abs(returns), plot = FALSE, lag.max = 5)$acf[2:6]),
    .groups = "drop"
  ) %>%
  unnest_wider(acf_vals, names_sep = "_lag")

# Visual summary focusing on lag 1 (with normal-theory CI bars)
ggplot(
  feat_H2,
  aes(x = factor(share.buy.hold), y = acf_vals_lag1, fill = factor(share.buy.hold))
) +
  stat_summary(fun.data = mean_cl_normal, geom = "col", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  guides(fill = "none") +
  labs(x = "B&H Share", y = "ACF lag 1 of |r|") +
  theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10))
  )

# Reshape ACF lags to long for a simple linear model across lags
feat_long <- feat_H2 %>%
  pivot_longer(
    cols = starts_with("acf_vals_lag"),
    names_to = "lag",
    values_to = "acf_value"
  ) %>%
  mutate(
    lag = as.integer(gsub("acf_vals_lag", "", lag))  # convert "acf_vals_lagX" → X
  )

model_acf <- lm(acf_value ~ share.buy.hold + lag, data = feat_long)
summary(model_acf)
broom::tidy(model_acf)

# =============================================================================
# 3) H3 Tail risk: exceedance probability and variance
# =============================================================================

k <- 3  # 3-sigma threshold for extreme-move frequency

df_thr <- df %>%
  group_by(share.buy.hold, X.run.number.) %>%
  summarise(
    vol_run  = var(returns, na.rm = TRUE),                               # run-level variance
    sig      = sd(returns, na.rm = TRUE),                                 # run-level sigma
    exceed_p = mean(abs(returns) > k * sig, na.rm = TRUE),                # P(|r| > k*sigma)
    VaR1     = -quantile(returns, 0.01, na.rm = TRUE),                    # left-tail VaR (1%)
    ES1      = -mean(returns[returns <= quantile(returns, 0.01, na.rm = TRUE)], na.rm = TRUE), # ES 1%
    .groups = "drop"
  )

# Simple linear regressions (dose–response on B&H share)
summary(lm(vol_run  ~ share.buy.hold, data = df_thr))
summary(lm(exceed_p ~ share.buy.hold, data = df_thr))

# Aggregate exceedance probability by B&H share, with 95% CI
df_thr_summary <- df_thr %>%
  group_by(share.buy.hold) %>%
  summarise(
    mean_exceed = mean(exceed_p, na.rm = TRUE),
    sd_exceed   = sd(exceed_p, na.rm = TRUE),
    n           = n(),
    .groups     = "drop"
  ) %>%
  mutate(
    se   = sd_exceed / sqrt(n),
    low  = mean_exceed - 1.96 * se,
    high = mean_exceed + 1.96 * se
  )

ggplot(
  df_thr_summary,
  aes(x = factor(share.buy.hold), y = mean_exceed, fill = factor(share.buy.hold))
) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2, linewidth = 0.7) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  guides(fill = "none") +
  labs(x = "B&H Share", y = paste0("Pr(|r| > ", k, "σ)")) +
  theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10))
  )

# =============================================================================
# 4) H4  Kurtosis and Distributional Shape
# =============================================================================

# 4.1  Excess kurtosis by run and B&H share
df_kurt <- df %>%
  group_by(share.buy.hold, X.run.number.) %>%
  summarise(kurt = kurtosis(returns, na.rm = TRUE), .groups = "drop")

# Monotonic association test (Spearman) between B&H share and kurtosis
cor.test(df_kurt$share.buy.hold, df_kurt$kurt, method = "spearman")

ggplot(df_kurt, aes(x = factor(share.buy.hold), y = kurt, fill = factor(share.buy.hold))) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 1) +
  guides(fill = "none") +
  labs(x = "B&H Share", y = "Excess kurtosis (per run)") +
  theme(
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10))
  )

# 4.2  QQ-plots of returns vs Normal, selected B&H shares
subset_q    <- c(0, 10, 20, 40)
subset_frac <- subset_q / 100

df %>%
  filter(share.buy.hold %in% subset_frac) %>%
  ggplot(aes(sample = returns,
             color = factor(share.buy.hold,
                            levels = subset_frac,
                            labels = paste0(subset_q, "% BH")),
             group = share.buy.hold)) +
  stat_qq(alpha = 0.8) +
  stat_qq_line() +
  facet_wrap(
    ~ factor(share.buy.hold,
             levels = subset_frac,
             labels = paste0(subset_q, "% BH")),
    scales = "free"
  ) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  guides(color = "none") +
  labs(x = "Normal Quantiles", y = "Return Quantiles") +
  theme(
    strip.text   = element_text(size = 14, face = "bold"),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10))
  )


# ---------------------------
# Helper per media e CI 95%
# ---------------------------
summ_ci <- function(x) {
  x <- x[is.finite(x)]
  n  <- length(x)
  m  <- mean(x)
  se <- sd(x)/sqrt(n)
  z  <- qnorm(0.975)
  tibble(mean = m, se = se, low = m - z*se, high = m + z*se, n = n)
}

# ---------------------------
# 1) Volatilità "end-of-run"
# ---------------------------
vol_end_by_run <- df %>%
  filter(X.step. == 3000) %>%
  group_by(share.buy.hold, X.run.number.) %>%
  summarise(vol_end = mean(meanVOl, na.rm = TRUE), .groups = "drop")

vol_tab <- vol_end_by_run %>%
  group_by(share.buy.hold) %>%
  summarise(summ_ci(vol_end), .groups = "drop") %>%
  rename(vol_mean = mean, vol_se = se, vol_low = low, vol_high = high, vol_n = n)

# ---------------------------
# 2) ACF lag 1 of |returns|
# ---------------------------
feat_H2 <- df %>%
  group_by(share.buy.hold, X.run.number.) %>%
  summarise(
    acf_vals = list(acf(abs(returns), plot = FALSE, lag.max = 5)$acf[2:6]),
    .groups = "drop"
  ) %>%
  unnest_wider(acf_vals, names_sep = "_lag")

acf_tab <- feat_H2 %>%
  group_by(share.buy.hold) %>%
  summarise(summ_ci(acf_vals_lag1), .groups = "drop") %>%
  rename(acf1_mean = mean, acf1_se = se, acf1_low = low, acf1_high = high, acf1_n = n)

# ---------------------------
# 3) Kurtosis of returns
# ---------------------------
kurt_by_run <- df %>%
  group_by(share.buy.hold, X.run.number.) %>%
  summarise(kurt = kurtosis(returns, na.rm = TRUE), .groups = "drop")

kurt_tab <- kurt_by_run %>%
  group_by(share.buy.hold) %>%
  summarise(summ_ci(kurt), .groups = "drop") %>%
  rename(kurt_mean = mean, kurt_se = se, kurt_low = low, kurt_high = high, kurt_n = n)

# ---------------------------
# 4) Join & format
# ---------------------------
summary_tab <- vol_tab %>%
  left_join(acf_tab,  by = "share.buy.hold") %>%
  left_join(kurt_tab, by = "share.buy.hold") %>%
  mutate(`B&H Share` = paste0(round(100*share.buy.hold), "%")) %>%
  select(`B&H Share`,
         vol_mean, vol_low, vol_high,
         acf1_mean, acf1_low, acf1_high,
         kurt_mean, kurt_low, kurt_high,
         vol_n, acf1_n, kurt_n)


# Stampa in console
kable(summary_tab_rounded, align = "c",
      col.names = c("B&H Share",
                    "Vol mean","Vol 95% low","Vol 95% high",
                    "|r| ACF(1) mean","ACF 95% low","ACF 95% high",
                    "Excess kurt mean","Kurt 95% low","Kurt 95% high",
                    "N_vol","N_acf","N_kurt"))

# Export
write_csv(summary_tab_compact, "summary_table_vol_acf_kurt.csv")

summary_tab_compact <- summary_tab %>%
  transmute(
    `B&H Share`,
    `Volatility` = sprintf("%.3f [%.3f; %.3f]", vol_mean, vol_low, vol_high),
    `ACF |r| (lag 1)` = sprintf("%.3f [%.3f; %.3f]", acf1_mean, acf1_low, acf1_high),
    `Excess kurtosis` = sprintf("%.3f [%.3f; %.3f]", kurt_mean, kurt_low, kurt_high),
    `N (runs)` = pmax(vol_n, acf1_n, kurt_n, na.rm = TRUE)
  )

