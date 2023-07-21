# author: Danielle Navarro
# date: 2020-06-23

# packages
library(ggplot2)
library(dplyr)
library(scico)
library(here)

rectgenerate <- function(seed) {
  
  set.seed(seed)
  ver <- 1
  fname <- here("images", paste0("rectgenerate_", ver, "_", seed, ".png"))
  
  n <- round(rnorm(1, mean = 100000, sd = 10000)) # number of consequential regions to sample
  r <- 7.5   # range parameter for the plots
  
  n_train <- 3
  r_width <- 2
  
  cx <- runif(1, min = .2, max = 1.5)
  cy <- runif(1, min = .2, max = 1.5)
  
  pal <- sample(scico_palette_names(), 1)
  bg <- sample(scico(n = 100, palette = pal), 1)
  
  train <- tibble(
    x = runif(n_train, min = -r_width/2, max = r_width/2),
    y = runif(n_train, min = -r_width/2, max = r_width/2)
  )
  
  # shepard model -----------------------------------------------------------
  
  train_with <- function(hypotheses, train) {
    n <- nrow(train)
    for(i in 1:n) {
      hypotheses <- hypotheses %>% 
        filter( # under weak sampling, Bayesian updating is simply falsification/filtering
          x_min < train$x[i], x_max > train$x[i], 
          y_min < train$y[i], y_max > train$y[i]
        )
    }
    return(hypotheses)
  }
  
  
  # construct posterior hypothesis space
  hypotheses <- tibble(
    mid_x = runif(n, min = -r, max = r), # prior location parameters are treated as arbitrary
    mid_y = runif(n, min = -r, max = r),
    len_x = rgamma(n, rate = cx, shape = 1),
    len_y = rgamma(n, rate = cy, shape = 1)
  ) %>%
    mutate( # reparameterize as the edges of the regions
      x_min = mid_x - len_x / 2, 
      x_max = mid_x + len_x / 2,
      y_min = mid_y - len_y / 2,
      y_max = mid_y + len_y / 2
    ) %>%
    train_with(train) %>%
    filter( # for visual nicety, use the Navarro et al 2012 "bounded" model
      x_min > -r, x_max < r,
      y_min > -r, y_max < r
    )
  
  
  # convenience function to compute the generalization probability given a 
  # stimulus locations and vectors 
  within_prob <- function(x, xmin, xmax) {
    within <- function(x) {
      mean(x < xmax & x > xmin)
    }
    unlist(lapply(x, within))
  }
  
  # construct generalization gradients
  generalise <- tibble(
    x = seq(-r, r, length.out = 1000),
    y = seq(-r, r, length.out = 1000),
    px = within_prob(x, hypotheses$x_min, hypotheses$x_max),
    py = within_prob(y, hypotheses$y_min, hypotheses$y_max)
  )
  
  
  # draw plot ---------------------------------------------------------------
  
  
  # the central plot showing the regions...
  pic <- ggplot(
    data = hypotheses, 
    mapping = aes(xmin = x_min, ymin = y_min, 
                  xmax = x_max, ymax = y_max)
  ) + 
    geom_rect(aes(fill = len_x + len_y), 
              alpha = .15, color = "white", 
              size = .1, show.legend = FALSE) + 
    
    geom_rect(fill = NA, color = paste0(bg, "22"), 
              size = .1, show.legend = FALSE) + 
    
    # stylistic
    theme_void() +
    theme(plot.background = element_rect(fill = bg, colour = bg)) + 
    scale_x_continuous(NULL, labels = NULL, limits = c(-r, r)) + 
    scale_y_continuous(NULL, labels = NULL, limits = c(-r, r)) + 
    scico::scale_fill_scico(palette = pal) +
    coord_cartesian(clip = "off")
  
  
  # save to file
  ggsave(
    filename = fname, 
    plot = pic, 
    height = 6,
    width = 6,
    dpi = 5000/6
  )
}
