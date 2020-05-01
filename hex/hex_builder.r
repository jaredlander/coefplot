library(coefplot)
library(dplyr)
library(recipes)
library(hexSticker)

land <- readr::read_csv('https://www.jaredlander.com/data/manhattan_Train.csv')
land

train <- land %>% 
    mutate(Class=stringr::str_replace(Class, ' Apartment', '')) %>% 
    mutate(LandUse=stringr::str_replace(LandUse, ' Buildings', '')) %>% 
    filter(LotArea > 0) %>% 
    recipe(TotalValue ~ Class + LotArea + HistoricDistrict + NumFloors + BldgArea, data=.) %>% 
    step_other(all_nominal()) %>% 
    step_normalize(NumFloors, LotArea) %>%
    prep() %>% 
    juice()
mod2 <- lm(TotalValue ~ ., data=train)
interMult <- -1/5
the_coefs <- coefplot(mod2, sort='magnitude', innerCI=3, outerCI=6, plot=FALSE) %>% 
    mutate_at(.vars=c('Value', 'HighInner', 'LowInner', 'HighOuter', 'LowOuter'), .funs=~.x/100) %>% 
    mutate(Coefficient=stringr::str_replace(Coefficient, 'Class', '')) %>% 
    mutate(Coefficient=stringr::str_replace(Coefficient, 'Yes', '')) %>% 
    mutate_at(.vars=c('Value', 'HighInner', 'LowInner', 'HighOuter', 'LowOuter'), 
              .funs=~if_else(Coefficient=='(Intercept)', .x*interMult, .x)) %>% 
    mutate_at(.vars=c('Value', 'HighInner', 'LowInner', 'HighOuter', 'LowOuter'), 
              .funs=~if_else(Coefficient=='Single Family', .x*-1, .x)) %>% 
    mutate_at(.vars=c('Value', 'HighInner', 'LowInner', 'HighOuter', 'LowOuter'), 
              .funs=~if_else(Coefficient=='LotArea', .x-5000, .x)) %>% 
    arrange(Value) %>% 
    mutate(Coefficient=factor(Coefficient, levels=Coefficient))

coefColor <- 'blue'
the_plot <- ggplot(the_coefs, aes(x=Value, y=Coefficient)) + 
    geom_errorbarh(aes(xmin=LowOuter, xmax=HighOuter), height=0, color=coefColor, size=0.1) + 
    # geom_errorbarh(aes(xmin=LowInner, xmax=HighInner), height=0, color=coefColor, size=2) + 
    geom_vline(xintercept=0, linetype=2, color='grey') + 
    geom_point(color=coefColor, size=1) + 
    # scale_x_continuous(labels=scales::dollar) + 
    # theme_classic() + 
    labs(y=NULL, x=NULL) + 
    theme(panel.grid=element_blank()) +
    theme_transparent()
the_plot
sticker(the_plot, package="coefplot", 
        p_size=20, p_color=coefColor, p_y=1.52,
        s_x=0.93, s_y=.82, s_width=1.3, s_height=1,
        h_fill="white", white_around_sticker=FALSE, h_color='grey',
        filename="inst/figures/coefplot_v2.png", spotlight=FALSE)

# sticker(the_plot, package="coefplot", 
#         p_size=20, p_color=coefColor,
#         s_x=0.89, s_y=.75, s_width=1.3, s_height=1,
#         filename="inst/figures/coefplot_v2.png", spotlight=FALSE, h_fill="#40a0db")

