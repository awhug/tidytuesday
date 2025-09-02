library(runner)
library(dplyr)
library(ggplot2)
library(VGAM)
library(ggtext)

billboard <- read.delim("data/230826_billboard.tsv", quote = "")
billboard <- janitor::clean_names(billboard)

fix_dates <- function(dates, date_format = "%d-%b-%y"){
    dates <- as.Date(dates, format = date_format)
    year <- lubridate::year(dates)
    bad_dates <- year > 2025
    year[bad_dates] <- year[bad_dates] - 100
    d_corrected <- as.Date(paste(year, format(dates, "%m"), format(dates, "%d"), sep="-"))
    return(d_corrected)
}
billboard$first_date <- fix_dates(billboard$date)

# Fix error in reporting
# Minnie Riperton date appears incorrect
date_err_ind <- with(billboard, which(artist == "Minnie Riperton"))
billboard$first_date[date_err_ind] <- "1975-04-05"

# Smooth has odd songwriter arrangement
writer_err_ind <- with(billboard, which(artist == "Santana ft. Rob Thomas"))
billboard$artist_is_a_songwriter[writer_err_ind] <- 1

# Work with more managable dataset
dat <- dplyr::select(.data = billboard,
    song, artist, first_date, artist_is_a_songwriter, artist_is_only_songwriter
    ) |>
    mutate(songwriter = as.factor(case_when(
        !artist_is_a_songwriter & !artist_is_only_songwriter ~ "none",
        artist_is_a_songwriter & artist_is_only_songwriter ~ "solo",
        artist_is_a_songwriter & !artist_is_only_songwriter ~ "multi"))
    ) |>
    arrange(first_date)

# Define even monthly grid
grid <- seq(min(dat$first_date), max(dat$first_date), by = "1 month")

# Calculate for each songwriter type
ma <- vector(mode = "list", length = 3)
for (i in c("none", "solo", "multi")){
    ma_grid <- runner::runner(
        x = dat$songwriter == i,
        idx = dat$first_date,
        at = grid,
        k = "6 months",
        f = function(x) mean(x, na.rm = TRUE)
    )
    ma[[i]] <- ma_grid
}

# Coerce into frame
prop_songwriters <- do.call(cbind, ma)

# Model using VGAM with splines
ps <- prop_songwriters
ps[which(ps == 0)] <- 1-0.999999
ps[which(ps == 1)] <- 0.999999

# Fit the model
ps_dat <- data.frame(ps, date = grid)
fit_v <- vgam(
    cbind(none, solo, multi) ~ s(date, df = 15), 
    family = dirichlet(), 
    data = ps_dat)

# Extract and format predictions
pred  <- predict(fit_v, type = "response")
colnames(pred) <- c("a_none", "b_solo", "c_multi")
smoothed_pred <- data.frame(pred, date = grid)
pred_long <- tidyr::pivot_longer(
    smoothed_pred,
    cols = -date,
    names_to = "type",
    values_to = "est"
)

# Check for key years
years <- lubridate::year(grid)
yearly_avg <- apply(prop_songwriters, 2, function(y) tapply(y, years, mean))
(top_years <- unique(years)[apply(yearly_avg, 2, which.max)])

# Plot it
x11()
ggplot(pred_long, aes(fill = type, y = est, x = date)) +
    geom_area(color = "white", size = 1.5) +
    theme_minimal() +
    scale_fill_manual(values = c("#E3C58C", "#55917F", "#264653")) +
    scale_x_date(
        breaks = seq(
            from = as.Date("1960-01-01"), 
            to = as.Date("2020-01-01"), 
            by = "10 years"
            ),
        labels = scales::date_format("%Y")
        ) +
    labs(
        title = "<span style = 'font-family:\"Typo Grotesk\"; font-size:40pt'>billboard #1, by committee</span><br>
        <span style = 'font-size:11pt'><br>Songwriting credits on hits have changed dramatically over the years. 
        The average proportion of songs hitting the top of the Billboard Hot 100 charts since 1958 by artists or 
        bands who **<span style = 'color:#BDA373;'>did not write the song</span>**, **<span style = 'color:#55917F;'>
        solely wrote the song</span>**, or **<span style = 'color:#264653;'>wrote the song collaboratively with 
        other songwriters</span>** is shown below.
        </span>",
        caption = "Proportions represent rolling averages over 6 month periods, smoothed using a Dirichlet Generalised Additive Model (GAM).<br>
        Data source: Billboard Hot 100 Number Ones Database, compiled by Chris Dalla Riva. Data visualisation: Angus Hughes, Github: @awhug"
    ) +
    theme(
        plot.title.position = "plot",
        plot.title = element_textbox_simple(
            size = 16,
            lineheight = 1.1,
            margin = margin(5, 60, 0, 40)
        ),
        plot.caption = element_textbox_simple(
            size = 8, colour = "#2525258e",
            margin = margin(15, 5, 0, 35)
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(
            vjust = 5, 
            size = 10, 
            colour = "black"),
        legend.position = "none",
        plot.background = element_rect(fill = "white")
    ) +
    geom_richtext(
        inherit.aes = FALSE,
        data = data.frame(
            x = as.Date("1970-01-01"),
            y = 0.85,
            label = "The 1960's saw the peak of Billboard #1<br>hits by artists/bands performing songs<br>**they themselves did not write.**"),
        aes(x = x, y = y, label = label),
        colour = "#252525",
        fill = NA, label.color = NA,
        size = 3.75, lineheight = 1.5
    ) +
    geom_richtext(
        inherit.aes = FALSE,
        data = data.frame(
            x = as.Date("1982-01-01"),
            y = 0.45,
            label = "By 1980 however, most<br>artists/bands were the **sole<br>songwriters** on their #1's."),
        aes(x = x, y = y, label = label),
        colour = "white",
        fill = NA, label.color = NA,
        size = 3.75, lineheight = 1.5
    ) +
    geom_richtext(
        inherit.aes = FALSE,
        data = data.frame(
            x = as.Date("2010-01-01"),
            y = 0.25,
            label = "From the turn of the millenium, artists/bands<br>increasingly branched out, **collaborating with other<br>(non-performing) songwriters** to write their #1s"),
        aes(x = x, y = y, label = label),
        colour = "white",
        fill = NA, label.color = NA,
        size = 3.75, lineheight = 1.5
    )

#dev.size("cm")

# Save it
ggsave(
    "billboard_100.svg",
    device = "svg",
    width = 23.1,
    height = 16.3,
    units = "cm",
    dpi = 320
)

# Raw verson (averaged by year) as sanity check
ps_dat |>
    tidyr::pivot_longer(
        cols = -date,
        names_to = "type",
        values_to = "est"
    ) |>
    mutate(
        year = lubridate::year(date),
        type = factor(type, levels = c("none", "solo", "multi"), ordered = TRUE)) |>
    summarise(est = mean(est), .by = c(year, type)) |>
    ggplot(aes(fill = type, y = est, x = year, color = type)) +
    #geom_bar(position = "dodge", stat = "identity", width = 50) +
    geom_area() +
    #geom_line() +
    theme_minimal()
