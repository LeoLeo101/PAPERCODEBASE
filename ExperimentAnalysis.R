# =============================================================================
# Setup
# =============================================================================

rm(list = ls())

# Core packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)
library(e1071)
library(broom)
library(gridExtra)


# Read data ---------------------------------------------------------------
dfraw <- read.csv("C:\\Users\\leoan\\OneDrive\\Desktop\\ArtificialFinancialMarketWithPassiveInvestors experiment-table.csv", skip = 6)

df <- dfraw %>%
  filter(X.step.>=100)

# Global plotting theme ---------------------------------------------------
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    )
)

# Helper for normal-theory CI used in stat_summary ------------------------
# Returns a data.frame with y, ymin, ymax like ggplot expects
mean_cl_normal <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  z <- qnorm(0.5 + conf/2)
  data.frame(y = m, ymin = m - z * se, ymax = m + z * se)
}

# =============================================================================
# 1) H1 — Volatility vs B&H share
# =============================================================================

# 1.1 — Dose–response at a fixed step (step = 3000)
# Aggregates the cumulative volatility measure at the end of the run
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

# 1.2 — Rolling volatility distribution by B&H share (boxplots + mean trace)
# For each run, compute a rolling mean of volatility over a 500-tick window
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

# 1.2 (alt view) — Spaghetti plot of runs + per-share average line
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

# Custom color palette per B&H level (levels must match factor labels)
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

# 1.3 — Linear dose–response: volatility-indicator ~ B&H share
m1 <- lm(volatility.indicator ~ share.buy.hold, data = df)
summary(m1)
broom::tidy(m1)
broom::glance(m1)

# =============================================================================
# 2) H2 — Volatility clustering: ACF of |returns|
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
# 3) H3 — Tail risk: exceedance probability and variance
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
# 4) H4 — Kurtosis and Distributional Shape
# =============================================================================

# 4.1 — Excess kurtosis by run and B&H share
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

# 4.2 — QQ-plots of returns vs Normal, selected B&H shares
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

