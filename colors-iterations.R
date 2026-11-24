
good1 <- sample(hcl.colors(8, "Dark 3", alpha = 1))
good2 <- sample(hcl.colors(8, "Dark 3", alpha = 1))
good3 <- sample(hcl.colors(8, "Dark 3", alpha = 1))
good4 <- sample(hcl.colors(8, "Dark 3", alpha = 1))
good5 <- sample(hcl.colors(8, "Dark 3", alpha = 1))
good6 <- sample(hcl.colors(8, "Dark 3", alpha = 1))

palette_list <- list(good1, good2, good3, good4, good5, good6)

plot_list <- map(palette_list, function(pal) {
  pp +
    scale_color_manual(
      name = "",
      values = pal
    )
})

names(plot_list) <- paste0("pp", 1:6)

list2env(plot_list, envir = .GlobalEnv)

combined_plot <- plot_list$pp1 + plot_list$pp2 + plot_list$pp3 +
  plot_list$pp4 + plot_list$pp5 + plot_list$pp6 +
  plot_layout(ncol = 3)  # Adjust layout as needed

combined_plot 

keeper1 <- plot_list$pp3

keeper2 <- plot_list$pp1

keeper3 <- plot_list$pp2

keeper4 <- plot_list$pp2

keeper5 <- plot_list$pp6

keeper6 <- plot_list$pp2

keeper7 <- plot_list$pp6

keeper8 <- plot_list$pp1

combined_plot <- keeper1 + keeper2 + keeper3 +
  keeper4 + keeper5 + keeper6 + keeper7 + keeper8 +
  plot_layout(ncol = 4)

keeper1 + keeper6 + plot_layout(ncol = 2)

keeper6

pp2

euh <- ggplot_build(pp2)

fillrecup <- unique(euh$data[[1]]["colour"])[,1]

fillfixed <- c("#F1421C", fillrecup[2:8])