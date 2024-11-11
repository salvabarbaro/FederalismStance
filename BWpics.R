library(dplyr)
library(ggplot2)
library(ggdist)
library(ggrepel)
library(ggpubr)
library(rstatix)
library(ggpp)
library(reshape2)
library(ggthemes)
library(colorspace)
library(texreg)
library(sf)
library(rgdal)
library(geojsonsf)
#setwd("")
load("DATA/microdf.RData")
######## DATA  #############################################################
df.melt <- melt(df, id.vars = c("abbreviation", "fedatt.micro"))
df.short <- df %>% group_by(abbreviation) %>% filter(row_number(fedatt.micro) == 1) %>% 
  dplyr::select(., -c("fedatt.micro")) %>%
  rename(fedatt.mean = 'mean(fedatt.micro)')
nuts.df <- data.frame(NUTS_ID = c("DEA", "DEB", "DEC", "DED", "DEE", "DE3", 
                                  "DE1", "DE2", "DE4", "DE5", "DE6", "DE7", 
                                  "DE8", "DE9", "DEF", "DEG"),
                      abbreviation = c("NW", "RP", "SL", "SN", "ST", "BE", "BW", "BY", 
                                       "BR", "HB", "HH", "HE", "MV", "NI", "SH", "TH"))
bund_shp <- giscoR::gisco_get_nuts(country = "DEU", nuts_level = 1, resolution = 03) %>%
  left_join(x = ., y = nuts.df, by = "NUTS_ID")

############# KW/Wilcox/Dunn-Tests  ########################################
kw1 <- df %>% kruskal_test(fedatt.micro~party)
mw1 <- df %>% wilcox_test(fedatt.micro~party, alternative = "greater")
kw2 <- df %>%  kruskal_test(fedatt.micro~Prussian.char)
dt2<- df %>%  dunn_test(fedatt.micro~Prussian.char)
############################################################################
# FIG 1: Contributing and recipient states since 1950
#setwd("~/Documents/Research/Yardstick/")
LFAseit1950 <- read.csv("LFAseit1950.csv") %>% 
  mutate(across(where(is.integer), as.numeric))
t.df <- as.data.frame(t(LFAseit1950 %>% dplyr::select(., -c("Year")) ) ) %>% 
  setNames(., paste0("x",1950:2022) ) %>%
  mutate(abbreviation = row.names(.), .before = "x1950") %>% 
  mutate(Year_1950 = cut(x1950, 
                         breaks = sort(c(fivenum(x1950)[-3], 0)), 
                         labels = c("--", "-",  "+", "++"),
                         include.lowest = T, right = T)) %>%
  mutate(Year_1965 = cut(x1965, 
                         breaks = sort(c(fivenum(x1965)[-3], 0)), 
                         labels = c("--", "-",  "+", "++") ,
                         include.lowest = T, right = T)) %>%
  mutate(Year_1980 = cut(x1980, 
                         breaks = sort(c(fivenum(x1980)[-3], 0)), 
                         labels = c("--", "-",  "+", "++"),
                         include.lowest = T, right = T)) %>%
  mutate(Year_1995 = cut(x1995, 
                         breaks = sort(c(fivenum(x1995)[-3], 0)), 
                         labels = c("--", "-",  "+", "++"),
                         include.lowest = T, right = T)) %>%
  mutate(Year_2005 = cut(x2005, 
                         breaks = sort(c(fivenum(x2005)[-3], 0)), 
                         labels = c("--", "-", "+", "++"),
                         include.lowest = T, right = T)) %>%
  mutate(Year_2022 = cut(x2022, 
                         breaks = sort(c(fivenum(x2022)[-3], 0)), 
                         labels = c("--", "-", "+", "++"),
                         include.lowest = T, right = T) )
plot.data <- left_join(x = bund_shp, y = t.df, by = "abbreviation")
#
y.list <- list("Year_1950", "Year_1965", "Year_1980", "Year_1995", "Year_2005", "Year_2022")
#
lfaplot.fun <- function(anno) {
  ggplot(data = plot.data) +
    geom_sf(aes(fill = get(anno)), 
            color = "gray80", linewidth = 0.2) +
    theme_map() +
    scale_fill_grey(start = .6, end = 0.3, na.value = "white") + # Grayscale from light to dark
    theme_void() + 
    theme(legend.position = "left") +
    labs(
      # title = "Fiscal Equalisation", 
      subtitle = anno, 
      fill = "value"
    )
}
#
lfaplots <- lapply(y.list, lfaplot.fun)
lfaplots.pic <- ggarrange(plotlist = lfaplots, ncol = 3, nrow = 2, 
                          common.legend = TRUE, legend = "right")
ggsave("LFA1950BW.pdf", 
       plot = lfaplots.pic)
#lfaplots.pic
############################################################################
# FIG 2: Federalism Stance (Average)
plot.data <- left_join(x = bund_shp, y = df.short, by = "abbreviation")
fedatt.plot <- ggplot(data = plot.data %>% 
                        mutate(fedatt.mean = format(round(fedatt.mean, digits = 2), nsmall = 2))) +
  geom_sf(aes(fill = as.numeric(fedatt.mean)), color = "white", linewidth = 0.6) +
  geom_sf_label(aes(label = fedatt.mean)) +
  coord_sf(xlim = c(5.866, 15.038), ylim = c(47.27, 55.06)) +
  scale_fill_gradient2(low = "gray10", mid = "gray50", high = "gray90", 
                       midpoint = mean(plot.data$fedatt.mean, na.rm = TRUE), 
                       na.value = "white") +
  theme_void(base_size = 18) + 
  theme(legend.position = "right") +
  labs(title = "", fill = "Value")
ggsave("FedAttAvgBW.pdf", 
       plot = fedatt.plot)
############################################################################
# FIG 3: FKM 21 (Financial Strength in 2021)
f1 <- ggplot(data = plot.data) +
  geom_sf(aes(fill = FKM21), color = "white", linewidth = 0.6) +
  coord_sf(xlim = c(5.866, 15.038), ylim = c(47.27, 55.06)) +
  scale_fill_gradient2(low = "gray80", mid = "gray50", high = "gray20", 
                       midpoint = 100, na.value = "lightgray") + # Grayscale gradient
  theme_void(base_size = 18) + 
  theme(legend.position = "left") +
  labs(title = "", fill = "Index")
#
f2 <- ggplot(data = df.short, aes(y = FKM21, x = fedatt.mean)) +
  geom_point(color = "black") + # Set points to black
  geom_smooth(method = "lm", na.rm = TRUE, se = TRUE, color = "darkgray") + # Gray for trend line
  geom_label_repel(aes(label = abbreviation), fill = "lightgray", color = "black", size = 8) + # Labels in grayscale
  theme_gray(base_size = 22) +
  labs(title = "",
       x = "Avg. Federalism Stance", y = "Financial Strength in 2021 (Index)")
fkmpic <- ggarrange(f1, f2, nrow = 1, ncol = 2)
fkmpic
ggsave("fkm21BW.pdf", 
       plot = fkmpic, width = 16, height = 9)
############################################################################
# FIG 4: Federalism Stance by government-heading parties
pmicro.plot <- ggplot(data = df.melt %>% filter(., variable %in% c("party")) %>% filter(., is.na(value) == FALSE),
                      aes(x = value, y = fedatt.micro, colour = value)) +
  ylim(.9, 4.9) +
  geom_violin(fill = "gray75", col = "black") +
  geom_jitter(aes(colour = value), size = 6, shape = 4, width = 0.3, height = 0.1, col = "gray20") +
  annotate(geom = "table", x = .7, y = 4.8, label = list(mw1), 
           vjust = 1, hjust = 0, size = 6) +
  theme_gray(base_size = 22) + theme(legend.position = "none") +
  labs(y = "Federalism Stance", x = "")
ggsave("partymicroBW.pdf", 
       width = 16, height = 9, plot = pmicro.plot)
############################################################################
#FIG 5: Catholic Shares
c1 <- ggplot(data = df.short, aes(y = cath.share50, x = fedatt.mean)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  geom_smooth(method = "lm", na.rm = TRUE, se = T, col = "darkgray") +
  geom_label_repel(aes(label = abbreviation), 
                   fill = "lightgray", col = "black", size = 6) +
  theme_gray(base_size = 16) +
  labs(title = "Year: 1950",
       x = "Federalism Attitude", y = "Catholic population share 1950")
c2 <- ggplot(data = df.short, aes(y = cath.share21, x = fedatt.mean)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  geom_smooth(method = "lm", na.rm = TRUE, se = T, col = "darkgray") +
  geom_label_repel(aes(label = abbreviation), 
                   fill = "lightgray", col = "black", size = 6) +
  theme_gray(base_size = 16) +
  labs(title = "Year: 2021",
       x = "Federalism Attitude", y = "Catholic population share 2021")
#
c3 <- ggplot(data = plot.data) +
  geom_sf(aes(fill = cath.share50), , color="black", linewidth=0.6) +
  coord_sf(xlim = c(5.866, 15.038), ylim = c(47.27, 55.06)) +
  scale_fill_gradient2(low = "gray80", mid = "gray50", high = "gray20", 
                       midpoint = .4, 
                       na.value = "white") +
  theme_void(base_size = 18) + theme(legend.position = "left") +
  labs(title="Year: 1950", 
       fill = "Share") 
c4 <- ggplot(data = plot.data) +
  geom_sf(aes(fill = cath.share21), , color="black", linewidth=0.6) +
  coord_sf(xlim = c(5.866, 15.038), ylim = c(47.27, 55.06)) +
  scale_fill_gradient2(low = "gray80", mid = "gray50", high = "gray20", 
                       midpoint = max(plot.data$cath.share21, na.rm = T) / 2, 
                       na.value = "white") +
  theme_void(base_size = 18) + theme(legend.position = "left") +
  labs(title="Year: 2021", 
       fill = "Share") 
fourfig <- ggarrange(c3, c1, c4, c2, ncol = 2, nrow = 2)
fourfig
ggsave("cath_fourfigBW.pdf", 
       plot = fourfig, width = 12, height = 9)
############################################################################
#FIG 6:  Prussia ###########################################################
url <- "https://histogis.acdh.oeaw.ac.at/api/tempspatial/10084/?format=json"
prussia.sf <- geojson_sf(url)
rm(url)
prussia.plot <- ggplot(data = prussia.sf) +
  geom_sf() +
  geom_sf(data = bund_shp, aes(fill = NA), col = "black", fill = NA) +
  coord_sf() +
  theme_void()
ggsave("PrussiaBW.pdf", 
       plot = prussia.plot)
##############################################################################
# FIG 7: prussiamicro
prussiapic <- ggplot(data = df, 
                     aes(x = Prussian.char, y = fedatt.micro, 
                         colour = Prussian.char, group = Prussian.char)) +
  geom_violin(fill = "gray75", col = "black") +
  ylim(0.9, 4.7) +
  geom_jitter(width = 0.25, height = 0.15, col = "gray20", shape = 3, size = 6) +
  annotate(geom = "table", x = .4, y = 4.65, label = list(dt2), 
           vjust = 1, hjust = 0, size = 6) +
  theme_gray(base_size = 18) + theme(legend.position = "none") +
  labs(x = "Historically Prussian", y = "Federalism Stance")
ggsave("prussiamicroBW.pdf", 
       width = 16, height = 9, plot = prussiapic)
##########################################################################################

