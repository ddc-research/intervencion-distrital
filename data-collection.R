library(tidyverse)
library(perusirtod) # calderonsamuel/perusirtod
library(perumapas) # calderonsamuel/perumapas
library(perupobreza2018)
library(sf)

selected_ubigeos <- sirtod_lugares |> 
	filter(provincia == "LIMA" | provincia == "CALLAO") |> 
	pull(ubigeo)

subset_indicadores <- sirtod_indicadores |> 
	filter(codigoIndicador %in% 516522:516529)

poblacion <- pobreza2018 |> 
	select(ubigeo, pob_2020)

data_intervenciones <- sirtod_valores |> 
	inner_join(subset_indicadores) |> 
	filter(ubigeo %in% selected_ubigeos) |> 
	left_join(poblacion) |> 
	filter(between(año, 2016, 2021)) |> 
	mutate(tasa = dato/pob_2020 * 10^4)

calcular_abreviatura_distrito <- function(x) {
	abr <- map_chr(x, ~{
		.x |> 
			str_remove(" DEL?") |> 
			str_split_1(" ") |> 
			str_extract("^.") |> 
			paste0(collapse = "")
	})
	
	if_else(str_length(abr) == 1, str_extract(x, "^..."), abr)
}

calcular_sumaria <- function(.data, wt_roc = 0.6) {
	.data |> 
		arrange(codigoIndicador, año, ubigeo) |> 
		summarise(
			.by = c(codigoIndicador, ubigeo),
			first_value = first(tasa),
			last_value = last(tasa),
			mean = mean(tasa),
			median = median(tasa)
		) |>
		mutate(
			absolut_change = last_value - first_value,
			rate_of_change = absolut_change / first_value,
			norm_roc = scales::rescale(rate_of_change),
			norm_last = scales::rescale(last_value),
			composite_score = (wt_roc * norm_roc) + ((1-wt_roc) * norm_last)
		) 
}

plot_distrital_puntaje_compuesto <- function(.data) {
	
	nombre_indicador <- unique(.data$indicador)
	
	.data |>
		ggplot() +
		geom_sf(
			aes(fill = composite_score, geometry = geometry),
			linetype = 2,
			color = "grey60"
		) +
		geom_point(
			aes(geometry = geometry),	
			stat = "sf_coordinates",
			size = 0.3,
			color = "grey20",
			alpha = 0.6
		) +
		ggrepel::geom_text_repel(
			aes(label = abr, geometry = geometry), 
			stat = "sf_coordinates",
			size = 2.3,
			min.segment.length = unit(0.1, "lines"),
			color = "grey20",
			fontface = "bold",
			seed = 1
		) +
		scale_fill_viridis_c(
			direction = -1, 
			option = "F", 
			begin = 0.4
		) +
		labs(
			title = nombre_indicador,
			subtitle = "Puntaje de priorización de intervención*",
			caption = "* Puntaje compuesto calculado en base a tasa de cambio 2015-2021 y valor más reciente"
		) +
		theme_light() +
		theme(
			legend.position = "bottom",
			legend.title = element_blank(),
			legend.key.width = unit(1.3, "cm"),
			legend.key.height = unit(0.2, "cm"),
			axis.text = element_blank(),
			axis.title = element_blank(),
			plot.title = element_text(size = 10, face = "bold"),
			plot.subtitle = element_text(size = 8),
			plot.caption = element_text(size = 8)
		)
}

plot_distrital_indicador_sirtod <- function(indicador_elegido, wt_roc = 0.5) {
	sumaria_intervenciones <- data_intervenciones |> 
		filter(codigoIndicador == indicador_elegido) |> 
		calcular_sumaria(wt_roc = 0.5) |> 
		left_join(sirtod_lugares) |> 
		mutate(abr = calcular_abreviatura_distrito(distrito))
	
	# View(sumaria_intervenciones)
	
	mapa_distrital |> 
		filter(ubigeo %in% selected_ubigeos) |> 
		left_join(sumaria_intervenciones) |> 
		left_join(sirtod_indicadores) |> 
		plot_distrital_puntaje_compuesto()
}

make_plot_and_save <- function(indicador_elegido) {
	plot_object <- plot_distrital_indicador_sirtod(indicador_elegido)
	
	nombre_indicador <- sirtod_indicadores |> 
		filter(codigoIndicador == indicador_elegido) |> 
		pull(indicador)
	
	ggsave(
		filename = paste0(indicador_elegido, "-", nombre_indicador, ".png"),
		plot = plot_object,
		path = "maps-out",
		width = 6,
		height = 9,
		dpi = 300
	)
}

516522:516529 |>
	as.character() |> 
	walk(make_plot_and_save)
