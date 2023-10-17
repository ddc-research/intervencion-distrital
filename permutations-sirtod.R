library(tidyverse)
library(perusirtod)
library(future)
library(furrr)
library(perupobreza2018)

plan(multisession)

sirtod_base <- sirtod_indicadores |> 
	filter(str_detect(indicador, "consumo de drogas")) |> 
	select(codigoIndicador) |> 
	left_join(sirtod_valores) |> 
	select(-codigoIndicador) |> 
	rename(periodo_anual = año, int_consumo_drogas = dato) |> 
	filter(periodo_anual %in% 2019:2021) |> 
	filter(str_starts(ubigeo, "1501")) |>
	mutate(int_consumo_drogas = as.integer(int_consumo_drogas)) |> 
	filter(n() == 3, .by = ubigeo)

ubigeos_unique <- unique(sirtod_base$ubigeo)

combs <- gtools::combinations(n = length(ubigeos_unique), r = 3, v = ubigeos_unique) |> 
	as_tibble() |> 
	rowwise() |> 
	mutate(triplets = list(c_across(c(V1, V2, V3)))) |> 
	ungroup()

get_score_order <- function(triplets, get_data = FALSE) {
	sirtod_ejemplo <- sirtod_base |> 
		filter(ubigeo %in% triplets)
	
	tabla_ejemplo <- sirtod_ejemplo |> 
		left_join(pobreza2018, by = "ubigeo") |> 
		select(ubigeo, periodo_anual, int_consumo_drogas, pob_2020) |> 
		mutate(tasa_int_consumo_drogas = (int_consumo_drogas/pob_2020 * 10^4) |> round(1)) |> 
		select(ubigeo, periodo_anual, tasa_int_consumo_drogas)
	
	tabla_ejemplo_agregada <- tabla_ejemplo |> 
		summarise(
			.by = ubigeo,
			mas_antigua = first(tasa_int_consumo_drogas, order_by = periodo_anual),
			mas_reciente = last(tasa_int_consumo_drogas, order_by = periodo_anual),
			cambio = mas_reciente - mas_antigua,
			porc_variacion = cambio/mas_antigua*100
		)
	
	puntaje_sin_normalizar <- tabla_ejemplo_agregada |> 
		select(ubigeo, mas_reciente, porc_variacion) |> 
		mutate(puntaje = (0.5*mas_reciente) + (0.5*porc_variacion)) |> 
		arrange(desc(puntaje))
	
	puntaje_normalizado <- tabla_ejemplo_agregada |> 
		mutate(
			norm_reciente = scales::rescale(mas_reciente),
			norm_variacion = scales::rescale(porc_variacion),
			puntaje = (0.5*norm_reciente) + (0.5*norm_variacion)
		) |> 
		select(ubigeo, norm_reciente, norm_variacion, puntaje) |> 
		arrange(desc(puntaje))
	
	result_1 <- puntaje_sin_normalizar$ubigeo[3]
	result_2 <- puntaje_normalizado$ubigeo[3]
	
	if (get_data) return(list(sin_normalizacion = puntaje_sin_normalizar, con_normalizacion = puntaje_normalizado))
	
	return(result_1 != result_2)
}

combinations_results <- combs$triplets |> 
	future_map(safely(get_score_order), .progress = TRUE)

results <- combs |> 
	mutate(result = map_lgl(combinations_results, "result")) |> 
	select(-triplets)

results |> 
	filter(result) |> 
	rowwise() |> 
	filter(! any(c_across(V1:V3) %in% c("150101", "150102", "150114", "150104")))

get_score_order(c("150103", "150105", "150113"), get_data = TRUE)
get_score_order(c("150103", "150105", "150120"), get_data = TRUE)
