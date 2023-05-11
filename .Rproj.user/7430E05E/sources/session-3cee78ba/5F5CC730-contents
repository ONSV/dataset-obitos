library(tidyverse)
library(geobr)
library(sf)

# Dados -------------------------------------------------------------------

datasus_url <- 
    "https://github.com/pabsantos/datasus-rda/raw/main/rda/datasus-sim-2022.rda"

load(url(datasus_url))

pop_municipios <- read_csv("data/pop_mun_ibge.csv")

geo_municipios <- read_municipality()

# Processamento -----------------------------------------------------------

obitos_municipios <- datasus_sim |> 
    filter(ano_ocorrencia > 2010) |> 
    group_by(cod_municipio, ano_ocorrencia) |> 
    summarise(obitos = n()) |> 
    mutate(ano_ocorrencia = as.character(ano_ocorrencia))

tabela_municipios <- pop_municipios |> 
    separate(Município, sep = " \\(", into = c("nome_municipio", "UF")) |> 
    mutate(
        cod_municipio = str_sub(cod_muni, 1, 6),
        uf = str_remove(UF, "\\)"),
        regiao = case_match(
            str_sub(cod_municipio, 1, 1),
            "1" ~ "Norte",
            "2" ~ "Nordeste",
            "3" ~ "Sudeste",
            "4" ~ "Sul", 
            "5" ~ "Centro-Oeste"
        )
    ) |> 
    pivot_longer(
        `2011`:`2021`,
        names_to = "ano_ocorrencia",
        values_to = "populacao"
    ) |> 
    select(cod_municipio, uf, nome_municipio, ano_ocorrencia, populacao)

tab_final <- tabela_municipios |> 
    left_join(obitos_municipios, by = c("cod_municipio", "ano_ocorrencia")) |> 
    replace_na(list(obitos = 0)) |> 
    mutate(taxa_pop = obitos / populacao * 100000)

geo_final <- geo_municipios |> 
    mutate(cod_municipio = str_sub(code_muni, 1, 6)) |> 
    select(cod_municipio) |> 
    left_join(tab_final, by = "cod_municipio")


# Pontos ------------------------------------------------------------------

geo_municipios_ponto <- geo_final |> 
    st_make_valid() |> 
    st_centroid()

# Export ------------------------------------------------------------------

save(geo_municipios, file = "data/geo_municipios_poly.rda")
save(geo_municipios_ponto, file = "data/geo_municipios_pontos.rda")
