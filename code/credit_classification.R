# Credit classification 
# Summary statistics that are useful for understanding the implications and limitations of our choices on how we classify credit lines

library(tidyverse)
library(data.table)
library(arrow)
library(fixest)

new_sample = read_parquet('data/raw/muestra_anonima_base2015_v2.parquet')

setDT(new_sample)

# Reporting break in 2018-06-30
new_sample[, before_2018:=ifelse(fecha_reporte<ymd("2018-06-30"), 1, 0)]

# Used vs unused credit
new_sample[fecha_reporte == ymd("2015-12-31"), .(balance = sum(credito_vigente), unused = sum(linea_no_utilizada))] /10^6

# Big Four - Time series of unused credit 
new_sample[entidad<10, .(unused = sum(linea_no_utilizada)), by = .(fecha_reporte, entidad)] %>% 
ggplot(aes(x=fecha_reporte, y=unused/10^6, color=as.factor(entidad))) + 
  geom_line() + geom_point() +
  labs(y="Unused (millions)", x=NULL, color="Entidad") +
  theme_bw()

  # "Otras lineas" was introduced in 2018-06-30
new_sample[entidad==1, .(unused = sum(linea_no_utilizada)), by = .(fecha_reporte, producto)] %>% 
  ggplot(aes(x=fecha_reporte, y=unused/10^6, color=producto)) + 
  geom_line() + geom_point() +
  labs(y="Unused (millions)", x=NULL, color="Product", title = "BCP - Unused credit by product") +
  theme_bw()

# What are the relevant products?
new_sample[fecha_reporte==ymd("2015-12-31"), .(balance = sum(credito_vigente), unused = sum(linea_no_utilizada)), by = producto] %>% 
  mutate(balance = round(prop.table(balance)*100, 2), unused = round(prop.table(unused)*100, 2)) %>%
  arrange(-balance) 

# Can we separate the products into revolving and non-revolving?
new_sample[fecha_reporte==ymd("2015-12-31"), .(balance = sum(credito_vigente), unused = sum(linea_no_utilizada)), by = .(producto, revolvencia)] %>% 
  mutate(balance = round(prop.table(balance)*100, 2), unused = round(prop.table(unused)*100,2)) %>%
  arrange(producto, revolvencia) %>% write_csv('output/tables/product_table.csv')

# Which products ever have unused lines?
new_sample[, .(unused = sum(linea_no_utilizada)), by = .(producto)] %>% 
  filter(unused > 0) 

  # Growth right arround the reporting break
new_sample[fecha_reporte %in% ymd(c("2018-05-31", "2018-06-30")) , .(unused = sum(linea_no_utilizada)), by = .(producto, before_2018)] %>% 
  filter(unused > 0) %>% 
  pivot_wider(names_from = "before_2018", names_prefix = "before_2018_", values_from = "unused" ) %>%
  mutate_if(is.numeric, function(x){x/10^3})

# See the evolution of prestamos for a single firm-bank 
new_sample[identificador == "EMP_004D06CEF2C3A57B15DCD5F396066B5D" & producto=="prestamos" & entidad==6, 
           .(balance = sum(credito_vigente), 
             unused = sum(linea_no_utilizada)), 
           by = .(fecha_reporte, revolvencia)] %>%
  gather(key = "type", value = "amount", balance, unused) %>%
  mutate(category = paste0(type, "_", revolvencia)) %>%
  ggplot(aes(x=fecha_reporte, y=amount/10^3, fill=as.factor(category))) + 
  geom_area(position = "stack") +
  labs(y="$ (millions)", x=NULL, fill="Used_Revolver") +
  theme_bw()

# Results from the product-based classification
CREDIT_LINE_PRODUCTS <- c("tarjeta_credito", "cuenta_corriente", "cartas_credito",
                          "avales_fianzas", "multilinea", "otras_lineas")
new_sample[, credit_line := producto %in% CREDIT_LINE_PRODUCTS]
new_sample[fecha_reporte==ymd("2015-12-31"), .(balance = sum(credito_vigente), unused = sum(linea_no_utilizada)), by = .(credit_line)] 


