library(yaml)
library(png)

# Read the YAML file
components <- read_yaml("indicator_calculation_components.yml")

# Indicator/component codes
indicator <- names(components)

# Extract component-level scores
scores <- sapply(
  components,
  function(x) {
    if (is.null(x$score)) NA_real_ else as.numeric(x$score)
  }
)

# Extract SDG number from indicator code
# e.g. 1-2-1 -> 1, 10-3-1 -> 10
sdg <- as.integer(sub("-.*$", "", indicator))

# Indicator-level dataset
indicator_scores <- data.frame(
  indicator = indicator,
  sdg = sdg,
  score = scores,
  row.names = NULL
)

# Mean score by SDG
sdg_scores <- aggregate(
  score ~ sdg,
  data = indicator_scores,
  FUN = mean,
  na.rm = TRUE
)

names(sdg_scores)[2] <- "mean_score"

# Number of scored indicators contributing to each SDG
n_indicators <- aggregate(
  score ~ sdg,
  data = indicator_scores,
  FUN = function(x) sum(!is.na(x))
)

names(n_indicators)[2] <- "n_indicators"

# Combine results and sort SDGs 1 -> 17
sdg_scores <- merge(sdg_scores, n_indicators, by = "sdg")
sdg_scores <- sdg_scores[order(sdg_scores$sdg), ]

print(sdg_scores)

# Graph

# Order SDGs from lowest to highest score
sdg_scores <- sdg_scores[order(sdg_scores$mean_score), ]

# Create SDG labels
sdg_scores$sdg_label <- paste0("SDG ", sdg_scores$sdg)

# Assign colours based on mean score
bar_colors <- ifelse(
  sdg_scores$mean_score == 5, "darkgreen",
  ifelse(
    sdg_scores$mean_score >= 2.5 & sdg_scores$mean_score < 5, "lightgreen",
    ifelse(
      sdg_scores$mean_score >= 0 & sdg_scores$mean_score < 2.5, "gold",
      ifelse(
        sdg_scores$mean_score >= -2.5 & sdg_scores$mean_score < 0, "orange",
        "red"
      )
    )
  )
)

# Shift scores so that -5 becomes 0
plot_scores <- sdg_scores$mean_score + 5

# Increase left margin to make room for y-axis labels
par(mar = c(5, 10, 4, 2) + 0.1)

# Get bar positions before plotting
bar_positions <- barplot(
  plot_scores,
  plot = FALSE
)

# Create bar chart
bp <- barplot(
  plot_scores,
#  names.arg = sdg_scores$sdg_label, # si on veut le retour des noms SDG
  names.arg = FALSE,
  col = bar_colors,
  border = NA,
  ylim = c(0, 10),
  xlim = c(min(bar_positions) - 0.40, max(bar_positions) + 0.40),
  yaxt = "n",
  # xlab = "SDG",
  ylab = "",
  main = "Canada's progress towards the Sustainable Development Goals from 2015 to 2027, by goal, Canadian Indicator Framework",
  las = 2
)

# Allow SDG icons to appear below the plotting area
par(xpd = NA)

# Add SDG icons below each bar
for (i in seq_along(bp)) {
  
  # Build icon filename based on SDG number
  icon_file <- file.path(
    "C:\\Users\\plamjer\\Documents\\cif-data-donnees-cic\\Outils\\SDG icones\\EN",
    sprintf("E_WEB_%02d.png", sdg_scores$sdg[i])
  )
  
  # Read icon
  icon <- readPNG(icon_file)
  
  # Add icon below corresponding bar
  rasterImage(
    icon,
    xleft   = bp[i] - 0.45,
    ybottom = -1.2,
    xright  = bp[i] + 0.45,
    ytop    = -0.3
  )
}

# Draw x-axis line at 0
abline(
  h = 0,
  col = "black",
  lwd = 1
)
# Draw y-axis with ticks at category boundaries
axis(
  side = 2,
  at = c(0, 2.5, 5, 7.5, 10),
  labels = FALSE
)

# Add classification labels
mtext(
  text = c(
    "Deterioration",
    "Limited progress",
    "Progress made but\nacceleration needed",
    "On track"
  ),
  side = 2,
  at = c(1.25, 3.75, 6.25, 8.75),
  line = 1.5,
  las = 2,
  cex = 0.75
)

# Target achieved corresponds specifically to 10
mtext(
  text = "Target achieved",
  side = 2,
  at = 10,
  line = 1.5,
  las = 2,
  cex = 0.75
)
# Add legend
#legend(
#  "topleft",
#  legend = c(
#    "Target achieved",
#    "On track",
#    "Progress made but acceleration needed",
#    "Limited progress",
#    "Deterioration"
#  ),
#  fill = c(
#    "darkgreen",
#    "lightgreen",
#    "gold",
#    "orange",
#    "red"
#  ),
#  border = NA,
#  bty = "n"
#)

# Add diagonal arrow showing increasing progress
arrows(
  x0 = min(bp),
  y0 = 0.5,
  x1 = max(bp),
  y1 = 9.5,
  length = 0.12,
  lwd = 2,
  col = "black"
)