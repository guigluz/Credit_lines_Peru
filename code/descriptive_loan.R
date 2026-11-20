# =========================================================
# Initial analysis of the loan data
#
# sample of 100 firms, from 2010-09 to 2023-12 (160 months)
# =========================================================

library(arrow)
library(tidyverse)
library(data.table)

sample = read_parquet("data/raw/base_muestra_seudonimizada.parquet")

setDT(sample)
glimpse(sample)

sample[, cantidad_empresas:= as.integer(cantidad_empresas)]

# Plot number of banks

sample[fecha_reporte == as.Date("2023-12-31"), .(cantidad_empresas=mean(cantidad_empresas)), by = identificador] %>%
  ggplot(aes(x = cantidad_empresas)) +
  geom_histogram() +
  labs(title = "Dist of the number of banks per firm in the sample", 
       x = "Number of banks", y = "Count",
       subtitle = "2023-12-31")  

# Plot number of products

sample[fecha_reporte == as.Date("2023-12-31"), .(n_products=uniqueN(producto)), by = identificador] %>%
  ggplot(aes(x = n_products)) +
  geom_histogram() +
  labs(title = "Dist of the number of products per firm in the sample", 
       x = "Number of products", y = "Count",
       subtitle = "2023-12-31")  

sample[ , .(n_products=uniqueN(producto)), by = identificador] %>%
  ggplot(aes(x = n_products)) +
  geom_histogram() +
  labs(title = "Dist of the number of products per firm in the sample", 
       x = "Number of products", y = "Count",
       subtitle = "Over entire sample")  

# Products with non-zero linea_no_utilizada:
# tarjeta_credito, prestamos, descuentos, credito_otorgado_no_desembolsado, 
# cartas_credito, cuenta_corriente, avales_fianzas, mutilineas, otras_lineas

sample[no_utilizado_total!=0, .(producto)] %>% unique()

# Plot used vs unused
sample[, .(used = sum(credito_utilizado), unused = sum(no_utilizado_total)), by = fecha_reporte] %>%
  pivot_longer(cols = c(used, unused), names_to = "type", values_to = "amount") %>%
  ggplot(aes(x = fecha_reporte, y = amount, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Used vs Unused credit", 
       x = NULL, y = "Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


