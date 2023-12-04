library(tidyverse)

# Download data and tidy it ----

variable_ids <- list(
  generico = "40171",
  especifico = "40172",
  modalidad = "40173",
  departamento = "40514",
  provincia = "40515",
  distrito = "40516"
) |>
  unlist() |>
  paste0(collapse = ",")

datacrim_url <- "https://datacrim.inei.gob.pe/csv_controller/index"

# Esto toma alrededor de 20 segundos
datacrim_delitos_pnp <- httr2::request(datacrim_url) |>
  httr2::req_url_query(desde="tematico", id = variable_ids) |>
  httr2::req_perform() |>
  httr2::resp_body_string() |>
  read_csv()

data_tidied <- datacrim_delitos_pnp |>
  janitor::clean_names() |>
  rename(
    periodo_anual = periodo,
    n_faltas = numero_faltas,
    departamento = departamento_del_hecho,
    provincia = provincia_del_hecho
  ) |>
  separate_wider_delim(
    cols = distrito_del_hecho,
    delim = " ",
    names = c("ubigeo", "distrito"),
    too_many = "merge"
  ) |>
  relocate(ubigeo, departamento, provincia, distrito, .after = periodo_anual) |>
  relocate(n_faltas, .after = everything())

write_rds(data_tidied, "data/datacrim_denuncias_lugar_hecho.rds", compress = "xz")

# Typify manually ----

recuento_generico_especifico <- data_tidied |> count(generica, especifica, sort = TRUE)
recuento_esp_modalidad <- data_tidied |> count(especifica, modalidad, sort = TRUE)

delitos_tipificados <- data_tidied |>
  mutate(
      tipo_crimen_organizado = case_when(
      str_detect(modalidad, "HOMICIDIO CALIFICADO") ~ "HOMICIDIO CALIFICADO",
      str_detect(modalidad, "SICARIATO") ~ "SICARIATO",
      str_detect(modalidad, "SECUESTRO") ~ "SECUESTRO",
      str_detect(modalidad, "ROBO AGRAVADO") ~ "ROBO AGRAVADO",
      especifica == "EXTORSION" ~ "EXTORSION",
      str_detect(modalidad, "USURPACION") ~ "USURPACION",
      especifica == "DELITOS INFORMATICOS" ~ "DELITOS INFORMATICOS",
      especifica == "DELITO MONETARIO" ~ "DELITOS MONETARIOS",
      str_detect(modalidad, "FABRICACION, SUMINISTRO O TENENCIA DE MATERIALES PELIGROSOS") ~ "FABRICACION, SUMINISTRO O TENENCIA DE MATERIALES PELIGROSOS",
      str_detect(especifica, "LEY DE REPRESION DE TID") ~ "TRAFICO ILICITO DE DROGAS",
      str_detect(modalidad, "MICROCOMERCIALIZACION") ~ "MICROCOMERCIALIZACION O MICROPRODUCCION",
      str_detect(modalidad, "MINERIA ILEGAL") ~ "MINERIA ILEGAL",
      str_detect(modalidad, "TRAFICO ILEGAL DE PRODUCTOS FORESTALES") ~ "TRAFICO ILEGAL DE PRODUCTOS FORESTALES",
      # especifica == "VIOLACION DEL SECRETO DE LAS COMUNICACIONES" ~ "VIOLACION DEL SECRETO DE LAS COMUNICACIONES",
      str_detect(generica, "DELITOS CONTRA LA ADMINISTRACION PUBLICA") ~ "DELITOS CONTRA LA ADMINISTRACION PUBLICA",
      .default = NA
    )
  )

write_rds(delitos_tipificados, "data/datacrim_denuncias_tipificado_crimen_organisado.rds", compress = "xz")
