partition_euforest <- function(euforest_rep, rep, env_file, min_occ_prop, k_sblock, k_random) {

    conflict_prefer("select", "dplyr")
    conflict_prefer("filter", "dplyr")
                                      
    data <- euforest_rep$data %>% filter(.part == rep) %>%
        ungroup() %>% select(-.part)
    
    env <- rast(env_file)
    
    set.seed(1996)
    data_part_sblock <- NA
    while(is.na(data_part_sblock[1])){
        data_part_sblock <- part_sblock(data = data,
                                 env_layer = env,
                                 pr_ab = "pr_ab",
                                 x = "x",
                                 y = "y",
                                 min_res_mult = 25,
                                 max_res_mult = 300,
                                 num_grids = 30,
                                 prop = 0.1,
                                 min_occ = dim(data)[1]*min_occ_prop,
                                 n_part = k_sblock)
        k_sblock = k_sblock-1 # decrease number of blocks until there is a solution.
    }

    set.seed(1996)    
    data_part_random <- part_random(data = data,
                        pr_ab = "pr_ab",
                        method = c(method = "kfold", folds = k_random)) 

    data_out <- data_part_sblock$part %>%
        rename(sblock.part = .part) %>%
        left_join(data_part_random, by = c("x", "y", "pr_ab")) %>%
        rename(random.part = .part)  

    ## block_layer <- get_block(env_layer = env, best_grid = data_part$grid)
    ## cl <- c("#64146D", "#9E2962", "#F47C15", "#FCFFA4")
    ## plot(block_layer, col=cl, legend=FALSE, axes=FALSE)
    ## points(data[,c("x", "y")])

    return(list(data = data_out,
                sblock_cell_size = data_part_sblock$best_part_info$cell_size,
                sblock_folds = max(data_out$sblock.part),
                species = euforest_rep$species,
                rep = rep))
}
