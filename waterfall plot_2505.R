

# ============================================================
# AUC confidence interval plot
# ============================================================

library(tidyverse)

auc_ci_data <- tibble(
  model = c(
    "LR-Chronic",
    "LR-Count",
    "LR-LCA",
    "XGB-Chronic",
    "XGB-Count",
    "XGB-LCA"
  ),
  auc = c(0.680, 0.682, 0.666, 0.679, 0.667, 0.658),
  ci_low = c(0.6324, 0.6355, 0.6201, 0.6326, 0.6214, 0.6101),
  ci_high = c(0.7236, 0.7244, 0.7085, 0.7233, 0.7118, 0.7031)
)

auc_ci_data <- auc_ci_data %>%
  mutate(
    model = factor(model, levels = rev(model))
  )

auc_ci_plot <- ggplot(auc_ci_data, aes(x = auc, y = model)) +
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high),
    height = 0.18,
    linewidth = 0.8
  ) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(
    limits = c(0.58, 0.75),
    breaks = seq(0.58, 0.75, by = 0.02)
  ) +
  labs(
    title = "AUC Values with Bootstrap-Based 95% Confidence Intervals",
    x = "AUC",
    y = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank()
  )

auc_ci_plot

ggsave(
  filename = "auc_confidence_intervals.png",
  plot = auc_ci_plot,
  width = 7,
  height = 4.5,
  dpi = 300
)