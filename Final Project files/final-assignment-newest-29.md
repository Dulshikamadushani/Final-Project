# 1. Introduction

This project presents a reproducible workflow for analyzing sediment
phosphorus fractions and selected sediment physicochemical properties
with depth. The analysis uses sediment core data and produces
manuscript-ready figures, summary tables, and a simple linear model to
evaluate the relationship between sediment depth and total phosphorus.

The workflow is designed to be fully reproducible using an R project
structure, relative file paths, clear data-cleaning steps, reusable
functions, and automatic output generation.

# 2. Objectives

The objectives of this project are to:

1.  Import and clean sediment chemistry data in a reproducible manner.
2.  Summarize variation in pH, electrical conductivity, nitrogen
    percentage, carbon percentage, and C/N ratio with sediment depth.
3.  Visualize the distribution of phosphorus fractions with depth.
4.  Quantify the percent contribution of each phosphorus fraction to
    total phosphorus.
5.  Demonstrate reproducible coding techniques using functions, loops,
    self-checks, and R Markdown.
6.  Apply a simple linear model to examine the relationship between
    sediment depth and total phosphorus.

# 3. Load packages

``` r
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(grid)
library(ggpubr)
library(ggpattern)
```

# 4. Read the CSV file

``` r
data <- read.csv(
  "Data file.csv",
  check.names = FALSE,
  na.strings = c("NA", "na", "")
)

colnames(data) <- trimws(colnames(data))


print(colnames(data))
```

    ##  [1] "Sample No."       "Sample depth"     "Reps"             "N [%]"           
    ##  [5] "C [%]"            "C/N  ratio"       "Soil pH"          "Soil EC / ?Scm-1"
    ##  [9] "NaCl IP mg/kg"    "NaCl TP mg/kg"    "NaCl Org P mg/kg" "NaBD IP mg/kg"   
    ## [13] "NaBD TP mg/kg"    "NaBD Org P mg/kg" "NaOH IP mg/kg"    "NaOH TP mg/kg"   
    ## [17] "NaOH Org P mg/kg" "HCl IP mg/kg"     "HCl TP mg/kg"     "HCl Org P mg/kg" 
    ## [21] "Residue mg/kg"

# 5. Creation of figures

## Figure 1: Physical characteristic variation of sediment with depth

``` r
data <- data %>%
  filter(if_any(everything(), ~ !is.na(.)))

# Fill depth values downward because only first replicate row may contain depth
data <- data %>%
  fill(`Sample depth`, .direction = "down")

# Convert depth to numeric
data <- data %>%
  mutate(`Sample depth` = as.numeric(`Sample depth`))


## Summarize data by depth
# pH and EC = single available value
# N, C, C/N = mean of 3 replicates

summary_data <- data %>%
  group_by(`Sample depth`) %>%
  summarise(
    pH = first(na.omit(`Soil pH`)),
    EC = first(na.omit(`Soil EC / ?Scm-1`)),
    N_mean = mean(`N [%]`, na.rm = TRUE),
    C_mean = mean(`C [%]`, na.rm = TRUE),
    CN_mean = mean(`C/N  ratio`, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(`Sample depth`)

# Check the summarized table
print(summary_data)
```

    ## # A tibble: 10 × 6
    ##    `Sample depth`    pH    EC N_mean C_mean CN_mean
    ##             <dbl> <dbl> <dbl>  <dbl>  <dbl>   <dbl>
    ##  1            0.5  5.42 18.0  0.322    2.01    6.25
    ##  2            1.5  5.52 16.9  0.305    1.82    5.99
    ##  3            2.5  5.47 15.1  0.126    1.48   11.8 
    ##  4            3.5  5.63 14.4  0.122    1.37   11.3 
    ##  5            4.5  5.6   9.87 0.116    1.34   11.6 
    ##  6            6.5  5.58  7.49 0.103    1.22   11.9 
    ##  7            9.5  5.64  7.36 0.101    1.20   11.8 
    ##  8           12.5  5.62  5.39 0.103    1.26   12.2 
    ##  9           15.5  5.49  2.35 0.105    1.38   13.1 
    ## 10           19.5  5.68  2.33 0.0984   2.63   26.7

``` r
str(summary_data)
```

    ## tibble [10 × 6] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample depth: num [1:10] 0.5 1.5 2.5 3.5 4.5 6.5 9.5 12.5 15.5 19.5
    ##  $ pH          : num [1:10] 5.42 5.52 5.47 5.63 5.6 5.58 5.64 5.62 5.49 5.68
    ##  $ EC          : num [1:10] 18.03 16.9 15.13 14.4 9.87 ...
    ##  $ N_mean      : num [1:10] 0.322 0.305 0.126 0.122 0.116 ...
    ##  $ C_mean      : num [1:10] 2.01 1.82 1.48 1.37 1.34 ...
    ##  $ CN_mean     : num [1:10] 6.25 5.99 11.77 11.25 11.6 ...

``` r
# Common theme

my_theme <- theme_classic() +
  theme(
    axis.title = element_text(size = 8, color = "black"),
    axis.text  = element_text(size = 8, color = "black"),
    
    plot.title = element_text(size = 10, face = "bold",
                              hjust = 0.5, lineheight = 0.95),
    
    axis.line = element_line(color = "black", linewidth = 0.7),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    
    plot.margin = margin(5, 5, 5, 5)
  )

# Common reversed depth axis
y_depth <- scale_y_reverse(
  limits = c(20, 0),
  breaks = seq(0, 20, 2),
  expand = c(0, 0)
)

# ================================
# a) pH variation with depth
# =============================
p1 <- ggplot(summary_data, aes(x = pH, y = `Sample depth`)) +
  geom_path(color = "black", linewidth = 0.7) +
  geom_point(color = "black", size = 1.2) +
  y_depth +
  scale_x_continuous(
    breaks = c(5, 5.5, 6),
    limits = c(5, 6)
  ) +
  labs(
    title = "a) pH variation with depth\n",
    x = "pH",
    y = "Sediment Depth (cm)"
  ) +
  my_theme

# ========================
# b) Electrical Conductivity variation with depth
# ===========================
p2 <- ggplot(summary_data, aes(x = EC, y = `Sample depth`)) +
  geom_path(color = "black", linewidth = 0.7) +
  geom_point(color = "black", size = 1.2) +
  y_depth +
  labs(
    title = "b) Electrical Conductivity\nvariation with depth",
    x = expression(paste("Electrical Conductivity (", mu, "S ", cm^-1, ")")),
    y = "Sediment Depth (cm)"
  ) +
  my_theme

# ================================
# c) Nitrogen percentage variation with depth
# ===================================
p3 <- ggplot(summary_data, aes(x = N_mean, y = `Sample depth`)) +
  geom_path(color = "black", linewidth = 0.7) +
  geom_point(color = "black", size = 1.2) +
  y_depth +
  labs(
    title = "c) Nitrogen percentage\nvariation with depth",
    x = "Nitrogen percentage (%)",
    y = "Sediment Depth (cm)"
  ) +
  my_theme

# ======================================
# d) Carbon percentage variation with depth
# ==========================================
p4 <- ggplot(summary_data, aes(x = C_mean, y = `Sample depth`)) +
  geom_path(color = "black", linewidth = 0.7) +
  geom_point(color = "black", size = 1.2) +
  y_depth +
  labs(
    title = "d) Carbon percentage\nvariation with depth",
    x = "Carbon percentage (%)",
    y = "Sediment Depth (cm)"
  ) +
  my_theme

# ======================================
# e) C/N variation with depth
# ==========================================
p5 <- ggplot(summary_data, aes(x = CN_mean, y = `Sample depth`)) +
  geom_path(color = "black", linewidth = 0.7) +
  geom_point(color = "black", size = 1.2) +
  y_depth +
  labs(
    title = "e) C/N variation with depth\n",
    x = "C/N ratio",
    y = "Sediment Depth (cm)"
  ) +
  my_theme

# ======================================
# Combine all five plots
# ==========================================
combined_plot <- ggarrange(
  p1, p2, p3, p4, p5,
  ncol = 3, nrow = 2
)
```

<img src="final-assignment-newest-29_files/figure-gfm/unnamed-chunk-4-1.png" alt="" style="display: block; margin: auto;" />

## Figure 2: Distribution of Pi and Po with depth

``` r
# read data
data <- read.csv(
  "Data file.csv",
  check.names = FALSE,
  na.strings = c("NA", "na", "")
)

names(data) <- trimws(names(data))

# Rename the needed columns 
data <- data %>%
  rename(
    `Sample depth` = `Sample depth`,
    `Loosely sorbed Pi (mg kg-1)` = `NaCl IP mg/kg`,
    `Loosely sorbed Po (mg kg-1)` = `NaCl Org P mg/kg`,
    `Loosely sorbed Pt (mg kg-1)` = `NaCl TP mg/kg`,
    `Redox sensitive Pi (mg kg-1)` = `NaBD IP mg/kg`,
    `Redox sensitive Po (mg kg-1)` = `NaBD Org P mg/kg`,
    `Redox sensitive Pt (mg kg-1)` = `NaBD TP mg/kg`,
    `Fe/Al Oxide Pi (mg kg-1)` = `NaOH IP mg/kg`,
    `Fe/Al Oxide Po (mg kg-1)` = `NaOH Org P mg/kg`,
    `Fe/Al Oxide Pt (mg kg-1)` = `NaOH TP mg/kg`,
    `Apatite bound Pi (mg kg-1)` = `HCl IP mg/kg`,
    `Apatite bound Po (mg kg-1)` = `HCl Org P mg/kg`,
    `Apatite bound Pt (mg kg-1)` = `HCl TP mg/kg`,
    `Residual P (mg kg-1)` = `Residue mg/kg`
  )

data <- data %>%
  fill(`Sample depth`, .direction = "down")

numeric_cols <- c(
  "Sample depth",
  "Loosely sorbed Pi (mg kg-1)",
  "Loosely sorbed Po (mg kg-1)",
  "Loosely sorbed Pt (mg kg-1)",
  "Redox sensitive Pi (mg kg-1)",
  "Redox sensitive Po (mg kg-1)",
  "Redox sensitive Pt (mg kg-1)",
  "Fe/Al Oxide Pi (mg kg-1)",
  "Fe/Al Oxide Po (mg kg-1)",
  "Fe/Al Oxide Pt (mg kg-1)",
  "Apatite bound Pi (mg kg-1)",
  "Apatite bound Po (mg kg-1)",
  "Apatite bound Pt (mg kg-1)",
  "Residual P (mg kg-1)"
)

data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)


# 2. Summary function
# Pt_mean = mean(Pi) + mean(Po)
# -------------------------
make_fraction_summary <- function(data_in, pi_col, po_col, fraction_name) {
  
  tmp <- data_in %>%
    transmute(
      `Sample depth`,
      Pi_value = .data[[pi_col]],
      Po_value = .data[[po_col]],
      Pt_rep   = Pi_value + Po_value
    )
  
  out <- tmp %>%
    group_by(`Sample depth`) %>%
    summarise(
      Pi_mean = mean(Pi_value, na.rm = TRUE),
      Po_mean = mean(Po_value, na.rm = TRUE),
      Pt_rep_sd = sd(Pt_rep, na.rm = TRUE),
      n_pt = sum(!is.na(Pt_rep)),
      .groups = "drop"
    ) %>%
    mutate(
      Pt_mean = Pi_mean + Po_mean,
      Pt_se = Pt_rep_sd / sqrt(n_pt)
    ) %>%
    arrange(`Sample depth`) %>%
    mutate(
      Fraction = fraction_name,
      y = rev(seq_len(n()))
    )
  
  return(out)
}

# 3. Plot creation
# -------------------------
plot_fraction <- function(summary_df, title_text, dark_col, light_col,
                          x_max, x_breaks) {
  
  bar_h <- 0.75
  
  pi_rect <- summary_df %>%
    mutate(
      xmin = 0,
      xmax = Pi_mean,
      ymin = y - bar_h / 2,
      ymax = y + bar_h / 2
    )
  
  po_rect <- summary_df %>%
    mutate(
      xmin = Pi_mean,
      xmax = Pi_mean + Po_mean,
      ymin = y - bar_h / 2,
      ymax = y + bar_h / 2
    )
  
  pt_rect <- summary_df %>%
    mutate(
      xmin = 0,
      xmax = Pt_mean,
      ymin = y - bar_h / 2,
      ymax = y + bar_h / 2
    )
  
  legend_df <- data.frame(
    x = c(1, 1, 1),
    y = c(1, 1, 1),
    grp = factor(c("Pt", "Pi", "Po"), levels = c("Pt", "Pi", "Po"))
  )
  
  ggplot() +
    geom_rect(
      data = pi_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = dark_col,
      color = NA
    ) +
    geom_rect(
      data = po_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = light_col,
      color = NA
    ) +
    geom_rect(
      data = pt_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    geom_errorbarh(
      data = summary_df,
      aes(
        y = y,
        xmin = pmax(Pt_mean - Pt_se, 0),
        xmax = Pt_mean + Pt_se
      ),
      height = 0.18,
      linewidth = 0.8,
      color = "black"
    ) +
    geom_point(
      data = legend_df,
      aes(x = x, y = y, fill = grp, shape = grp),
      size = 5,
      color = "black",
      stroke = 1,
      alpha = 0
    ) +
    scale_y_continuous(
      breaks = summary_df$y,
      labels = summary_df$`Sample depth`,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_x_continuous(
      limits = c(0, x_max),
      breaks = x_breaks,
      expand = c(0, 0.05)
    ) +
    scale_fill_manual(
      values = c(
        "Pt" = "white",
        "Pi" = dark_col,
        "Po" = light_col
      ),
      breaks = c("Pt", "Pi", "Po"),
      labels = c("Pt", "Pi", "Po"),
      name = NULL,
      guide = guide_legend(
        override.aes = list(
          shape = c(22, 22, 22),
          size = c(6, 6, 6),
          alpha = c(1, 1, 1),
          color = c("black", NA, NA),
          fill = c("white", dark_col, light_col),
          stroke = c(1.2, 0, 0)
        )
      )
    ) +
    scale_shape_manual(
      values = c("Pt" = 22, "Pi" = 22, "Po" = 22),
      guide = "none"
    ) +
    labs(
      title = title_text,
      x = expression(PO[4]^{"3-"}~"- P concentration (mg kg"^{-1}*")"),
      y = "Sediment Depth (cm)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 8, color = "black"),
      axis.text.y = element_text(size = 8, color = "black"),
      legend.text = element_text(size = 8),
      legend.position = c(0.78, 0.4),
      legend.key.size = unit(0.2, "cm"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      axis.line = element_blank(),
      plot.margin = margin(6, 10, 6, 10)
    )
}


# 4. Color selection
# -------------------------
purple_dark  <- "#7B1FA2"
purple_light <- "#C5ADD8"

red_dark     <- "#E52424"
red_light    <- "#DDA3A3"

green_dark   <- "#329632"
green_light  <- "#A8C6A8"

blue_dark    <- "#3774C2"
blue_light   <- "#9EB7D3"

xmax_loose   <- 20
breaks_loose <- seq(0, 20, by = 5)

xmax_redox   <- 150
breaks_redox <- seq(0, 150, by = 50)

xmax_feal    <- 900
breaks_feal  <- seq(0, 900, by = 150)

xmax_apatite <- 100
breaks_apatite <- seq(0, 100, by = 20)


# 5. loop
# -------------------------
fraction_specs <- list(
  list(
    title = "a) Loosely sorbed P",
    pi_col = "Loosely sorbed Pi (mg kg-1)",
    po_col = "Loosely sorbed Po (mg kg-1)",
    frac_name = "Loosely sorbed P",
    dark = purple_dark,
    light = purple_light,
    xmax = xmax_loose,
    breaks = breaks_loose
  ),
  list(
    title = "b) Redox sensitive P",
    pi_col = "Redox sensitive Pi (mg kg-1)",
    po_col = "Redox sensitive Po (mg kg-1)",
    frac_name = "Redox sensitive P",
    dark = green_dark,
    light = green_light,
    xmax = xmax_redox,
    breaks = breaks_redox
  ),
  list(
    title = "c) Fe/Al Oxide P",
    pi_col = "Fe/Al Oxide Pi (mg kg-1)",
    po_col = "Fe/Al Oxide Po (mg kg-1)",
    frac_name = "Fe/Al Oxide P",
    dark = red_dark,
    light = red_light,
    xmax = xmax_feal,
    breaks = breaks_feal
  ),
  list(
    title = "d) Apatite bound P",
    pi_col = "Apatite bound Pi (mg kg-1)",
    po_col = "Apatite bound Po (mg kg-1)",
    frac_name = "Apatite bound P",
    dark = blue_dark,
    light = blue_light,
    xmax = xmax_apatite,
    breaks = breaks_apatite
  )
)


# 6. Create plots with loop
# -------------------------
plot_list <- vector("list", length(fraction_specs))

for (i in seq_along(fraction_specs)) {
  spec <- fraction_specs[[i]]
  
  summary_df <- make_fraction_summary(
    data_in = data,
    pi_col = spec$pi_col,
    po_col = spec$po_col,
    fraction_name = spec$frac_name
  )
  
  plot_list[[i]] <- plot_fraction(
    summary_df = summary_df,
    title_text = spec$title,
    dark_col = spec$dark,
    light_col = spec$light,
    x_max = spec$xmax,
    x_breaks = spec$breaks
  )
}

# object names 
p1 <- plot_list[[1]]
p2 <- plot_list[[2]]
p3 <- plot_list[[3]]
p4 <- plot_list[[4]]


# 7. Combine
# -------------------------
final_plot <- ggarrange(
  plotlist = plot_list,
  ncol = 2, nrow = 2
)
```

<img src="final-assignment-newest-29_files/figure-gfm/unnamed-chunk-6-1.png" alt="" style="display: block; margin: auto;" />

## Figure 3: Percent distribution of P fractions with depth

``` r
# 1. Read data
data <- read.csv(
  "Data file.csv",
  check.names = FALSE,
  na.strings = c("NA", "na", "")
)

names(data) <- trimws(names(data))

# 2. Rename Columns
data <- data %>%
  rename(
    sample_no    = `Sample No.`,
    sample_depth = `Sample depth`,
    rep          = `Reps`,
    ph           = `Soil pH`,
    ec           = `Soil EC / ?Scm-1`,
    nacl_ip      = `NaCl IP mg/kg`,
    nacl_tp      = `NaCl TP mg/kg`,
    nacl_org     = `NaCl Org P mg/kg`,
    nabd_ip      = `NaBD IP mg/kg`,
    nabd_tp      = `NaBD TP mg/kg`,
    nabd_org     = `NaBD Org P mg/kg`,
    naoh_ip      = `NaOH IP mg/kg`,
    naoh_tp      = `NaOH TP mg/kg`,
    naoh_org     = `NaOH Org P mg/kg`,
    hcl_ip       = `HCl IP mg/kg`,
    hcl_tp       = `HCl TP mg/kg`,
    hcl_org      = `HCl Org P mg/kg`,
    residue      = `Residue mg/kg`
  )

# 3. Clean and Process Data
data_clean <- data %>%
  mutate(across(c(sample_no, sample_depth, ph, ec, contains("_"), residue), as.numeric)) %>%
  mutate(rep = as.character(rep)) %>%
  fill(sample_depth, .direction = "down")

# 4. Calculate Percentages
pct_rep <- data_clean %>%
  transmute(
    depth   = sample_depth,
    rep     = rep,
    nacl_tp = replace_na(nacl_tp, 0),
    nabd_tp = replace_na(nabd_tp, 0),
    naoh_tp = replace_na(naoh_tp, 0),
    hcl_tp  = replace_na(hcl_tp, 0),
    residue = replace_na(residue, 0)
  ) %>%
  mutate(
    total_p     = nacl_tp + nabd_tp + naoh_tp + hcl_tp + residue,
    nacl_pct    = (nacl_tp / total_p) * 100,
    nabd_pct    = (nabd_tp / total_p) * 100,
    naoh_pct    = (naoh_tp / total_p) * 100,
    hcl_pct     = (hcl_tp / total_p) * 100,
    residue_pct = (residue / total_p) * 100
  )

# 5. Renaming panel titles
pct_long <- pct_rep %>%
  select(depth, rep, contains("_pct")) %>%
  pivot_longer(
    cols      = contains("_pct"),
    names_to  = "fraction",
    values_to = "percent"
  ) %>%
  mutate(
    fraction = factor(
      fraction,
      levels = c("nacl_pct", "nabd_pct", "naoh_pct", "hcl_pct", "residue_pct"),
      labels = c(
        "Loosely sorbed Pt",
        "Redox sensitive Pt",
        "Fe/Al Oxide Pt",
        "Apatatite bound Pt",
        "Residue Pt"
      )
    )
  )

# 6. Summary Stat
sum_df <- pct_long %>%
  group_by(depth, fraction) %>%
  summarise(
    mean_percent = mean(percent, na.rm = TRUE),
    se_percent   = sd(percent, na.rm = TRUE) / sqrt(sum(!is.na(percent))),
    .groups      = "drop"
  ) %>%
  group_by(depth) %>%
  arrange(fraction, .by_group = TRUE) %>%
  mutate(xmax = cumsum(mean_percent)) %>%
  ungroup()

# 6_A. Calculate Total P (mean of replicates)
totalP_df <- data_clean %>%
  transmute(
    depth   = sample_depth,
    total_p = rowSums(across(c(nacl_tp, nabd_tp, naoh_tp, hcl_tp, residue)), na.rm = TRUE)
  ) %>%
  group_by(depth) %>%
  summarise(
    total_p_mean = mean(total_p, na.rm = TRUE),
    .groups      = "drop"
  )

# 7. Formatting Depth Labels
depth_order <- sort(unique(sum_df$depth))
depth_key <- data.frame(
  depth = depth_order,
  depth_label = as.character(depth_order)
)

sum_df <- sum_df %>%
  left_join(depth_key, by = "depth") %>%
  mutate(depth_f = factor(depth_label, levels = rev(depth_key$depth_label))) %>%
  left_join(totalP_df, by = "depth")

outline_df <- sum_df %>%
  distinct(depth_f) %>%
  mutate(total = 100)

# 7A. Labels for Total P
label_df <- sum_df %>%
  distinct(depth_f, total_p_mean) %>%
  mutate(
    x_label = 102,
    label   = sprintf("%.2f", total_p_mean)
  )

# 8. Color selection on plot
fill_cols <- c(
  "Loosely sorbed Pt"   = "#7B3294",
  "Redox sensitive Pt"  = "#50C878",
  "Fe/Al Oxide Pt"      = "#d95f02",
  "Apatatite bound Pt"  = "#3182BD",
  "Residue Pt"          = "#9E9E9E"
)

pattern_vals <- c(
  "Loosely sorbed Pt"   = "none",
  "Redox sensitive Pt"  = "stripe",
  "Fe/Al Oxide Pt"      = "crosshatch",
  "Apatatite bound Pt"  = "none",
  "Residue Pt"          = "circle"
)

pattern_angle_vals <- c(
  "Loosely sorbed Pt"   = 0,
  "Redox sensitive Pt"  = 45,
  "Fe/Al Oxide Pt"      = 45,
  "Apatatite bound Pt"  = 90,
  "Residue Pt"          = 45
)

# 9. Theme 
base_theme <- theme_bw(base_size = 20) +
  theme(
    panel.grid       = element_blank(),
    axis.title.x     = element_text(size = 12),
    axis.title.y     = element_text(size = 12),
    axis.text.x      = element_text(size = 10),
    axis.text.y      = element_text(size = 10),
    axis.ticks       = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.3, "cm"),
    axis.text        = element_text(colour = "black"),
    legend.title     = element_text(size = 12, colour = "black"),
    legend.text      = element_text(size = 10),
    legend.key.size  = unit(0.6, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.6, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.position  = c(1.55, 0.5),
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    plot.margin      = unit(c(0.5, 6, 0.5, 0.5), "cm"),
    aspect.ratio     = 1.2
  )

legend_override <- guide_legend(
  override.aes = list(
    pattern         = unname(pattern_vals),
    pattern_angle   = unname(pattern_angle_vals),
    pattern_fill    = rep("grey30", length(pattern_vals)),
    pattern_colour  = rep("grey30", length(pattern_vals)),
    pattern_density = rep(0.03, length(pattern_vals)),
    pattern_spacing = rep(0.02, length(pattern_vals)),
    colour          = rep("black", length(pattern_vals)),
    linewidth       = rep(0.6, length(pattern_vals))
  )
)

# 10. Create Plot
p <- ggplot(
  sum_df,
  aes(
    y = depth_f,
    x = mean_percent,
    fill = fraction,
    pattern = fraction,
    pattern_angle = fraction
  )
) +
  geom_col_pattern(
    position = position_stack(reverse = TRUE),
    colour = "black",
    linewidth = 0.6,
    width = 0.82,
    pattern_fill = "grey30",
    pattern_colour = "grey30",
    pattern_density = 0.03,
    pattern_spacing = 0.02,
    pattern_res = 600,
    pattern_key_scale_factor = 0.7
  ) +
  geom_col(
    data = outline_df,
    aes(x = total, y = depth_f),
    inherit.aes = FALSE,
    fill = NA,
    colour = "black",
    linewidth = 0.5,
    width = 0.82
  ) +
  annotate(
    "segment",
    x = seq(0, 100, 2),
    xend = seq(0, 100, 2),
    y = 0.5,
    yend = 0.35,
    colour = "black"
  ) +
  geom_text(
    data = label_df,
    aes(x = x_label, y = depth_f, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 4
  ) +
  annotate(
    "text",
    x = 102,
    y = length(unique(sum_df$depth_f)) + 0.8,
    label = "P[t] (mg/kg)",
    parse = TRUE,
    hjust = 0,
    size = 4
  ) +
  scale_fill_manual(
    values = fill_cols,
    name = "P fraction",
    labels = c(
      expression("Loosely sorbed " * P[t]),
      expression("Redox sensitive " * P[t]),
      expression("Fe/Al Oxide " * P[t]),
      expression("Apatatite bound " * P[t]),
      expression("Residue " * P[t])
    ),
    guide = legend_override
  ) +
  scale_pattern_manual(
    values = pattern_vals,
    guide = "none"
  ) +
  scale_pattern_angle_manual(
    values = pattern_angle_vals,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0.01, 0)
  ) +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  labs(
    x = expression(PO[4]^{"3-"} ~ - ~ P ~ fraction ~ percentage ~ "(%)"),
    y = "Sediment Depth (cm)"
  ) +
  base_theme

print(p)
```

<img src="final-assignment-newest-29_files/figure-gfm/unnamed-chunk-7-1.png" alt="" style="display: block; margin: auto;" />

## Figure 4: The relationship between sediment depth and mean total phosphorus

``` r
# Read data
data <- read.csv(
  "Data file.csv",
  check.names = FALSE,
  na.strings = c("NA", "na", "")
)

names(data) <- trimws(names(data))

# Linear model analysis
lm_total_p <- lm(total_p_mean ~ depth, data = totalP_df)

summary(lm_total_p)
```

    ## 
    ## Call:
    ## lm(formula = total_p_mean ~ depth, data = totalP_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -57.800 -34.698  -9.062  24.974  71.941 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  564.798     24.002  23.532 1.13e-08 ***
    ## depth        -13.619      2.462  -5.532 0.000553 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 47.54 on 8 degrees of freedom
    ## Multiple R-squared:  0.7928, Adjusted R-squared:  0.7669 
    ## F-statistic:  30.6 on 1 and 8 DF,  p-value: 0.0005526

``` r
# Plot the linear model
p_lm <- ggplot(totalP_df, aes(x = depth, y = total_p_mean)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  labs(
    title = "Relationship between sediment depth and mean total phosphorus",
    x = "Sediment Depth (cm)",
    y = "Mean Total P (mg/kg)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

p_lm
```

<img src="final-assignment-newest-29_files/figure-gfm/unnamed-chunk-8-1.png" alt="" style="display: block; margin: auto;" />

## Interpretation of the linear model

The linear model provides a simple statistical assessment of whether
mean total phosphorus changes systematically with sediment depth. This
model is included to demonstrate the use of linear modeling in a
reproducible workflow. Because sediment geochemical patterns may not
always follow a strictly linear relationship, the results should be
interpreted as an exploratory summary rather than a complete mechanistic
description.

# 6. Reproducibility features used in this project

This workflow includes several reproducibility features:

- Use of an R project structure
- Relative file paths
- R Markdown for transparent reporting
- Reusable functions for repeated tasks
- A loop for generating multiple phosphorus fraction plots
- Automatic saving of figures
- Export-ready outputs for GitHub submission

# 7. Conclusions

This project demonstrates a reproducible analytical workflow for
sediment phosphorus data. The workflow integrates data import,
validation, cleaning, summarization, visualization, and simple modeling
into a single structured R Markdown document. The figures and exported
outputs can be reproduced by any user with access to the project
repository and required packages.

# 8. GitHub
