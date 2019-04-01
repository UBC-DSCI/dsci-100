library(tidyverse)
library(Rtsne)

pokemon <- read_csv('data/pokemon.csv') %>%
    select(Speed, Defense)






generate_random_clusters <- function(df, num_clusters, num_iters){
    
    num_seed <- round(runif(1, min = 1, max = 1e6))
    
    set.seed(num_seed)
    
    
    kmeans_results <- df %>%
        kmeans(centers = num_clusters, iter.max = num_iters)
    
    df$cluster <- as.factor(kmeans_results$cluster)
    
    cluster_viz <- ggplot(df, aes_string(x = 'Speed', y = 'Defense', colour = 'cluster')) +
        geom_point(alpha = 0.5) +
        labs(x = "Speed",
             y = "Defense") +
        theme(legend.position = "none")
    return(cluster_viz)
}

plot_list <- list()

for (i in 1:6){
    plot_list[[i]] <- generate_random_clusters(pokemon, 4, 1)
}

library(cowplot)
plot_matrix <- do.call(plot_grid, plot_list)

cowplot::ggsave(plot_matrix, filename = 'imgs/multiple_initializations.png', 
                units = 'in', width = 8, height = 4)