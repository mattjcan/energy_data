# EXPORT --------------------------------------------------------------------

png("images/p_delta_ap.png", width = 6, height = 3, units = "in", res = 300)
p_delta_ap
dev.off() 

png("images/p_delta.png", width = 6, height = 3, units = "in", res = 300)
p_delta
dev.off() 

png("images/p_g_supply.png", width = 6, height = 3, units = "in", res = 300)
p_g_supply
dev.off() 

png("images/p_g_demand.png", width = 6, height = 3, units = "in", res = 300)
p_g_demand
dev.off() 

png("images/p_g_fuel.png", width = 6, height = 3, units = "in", res = 300)
p_g_fuel
dev.off() 

png("images/p_g_gen.png", width = 6, height = 3, units = "in", res = 300)
p_g_gen
dev.off() 

png("images/p_g_fuel.png", width = 6, height = 3, units = "in", res = 300)
p_g_fuel
dev.off() 

png("images/p_r_demand_ap.png", width = 6, height = 3, units = "in", res = 300)
p_r_demand$`Asia Pacific`
dev.off() 

png("images/p_r_gen_ap.png", width = 6, height = 3, units = "in", res = 300)
p_r_gen$`Asia Pacific`
dev.off() 

png("images/p_r_supply_ap.png", width = 6, height = 3, units = "in", res = 300)
p_r_supply$`Asia Pacific`
dev.off() 

png("images/p_g_india.png", width = 6, height = 3, units = "in", res = 300)
grid.arrange(p_g_india, p_delta_india, ncol = 2)
dev.off() 

png("images/pov.png", width = 6, height = 3, units = "in", res = 300)
grid.arrange(p_r_gen$`Asia Pacific` + theme(plot.title = element_text(size = 8), plot.subtitle = element_text(size = 7), plot.caption = element_text(size = 7)), p_pov_ap + theme(plot.title = element_text(size = 8), plot.subtitle = element_text(size = 7), plot.caption = element_text(size = 7)), ncol = 2)
dev.off() 

png("images/p_hist_coal.png", width = 6, height = 3, units = "in", res = 300)
p_hist_coal
dev.off() 

png("images/p_coal_prices.png", width = 6, height = 3, units = "in", res = 300)
p_coal_prices
dev.off() 
