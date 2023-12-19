library("ggplot2")
library("ggpubr")
library("ggrepel")
library("tidyverse")
library("vcfR")

#Number of SNPs, MNPs, INS, DELS, COMPLEX

slices <- c(1023,	36,	48,	4)
name <- c("SNPs","INS","DEL","COMPLEX")

num_variants <- data.frame(name,slices)
num_variants$percentage <- round(slices/sum(slices)*100, digits = 2)

df2 <- num_variants %>% 
  mutate(csum = rev(cumsum(rev(slices))), 
         pos = slices/2 + lead(csum, 1),
         pos = if_else(is.na(pos), slices/2, pos))

ggplot(num_variants, aes(x = "" , y = slices, fill = fct_inorder(name))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(percentage, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_pubclean()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

input_data <- read.vcfR("file.vcf")

