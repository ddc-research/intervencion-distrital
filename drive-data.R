library(googledrive)

drive_auth()

drive_find(n_max = 100, )

diagnostico_cultivos <- drive_get(id = "1grXd8445eTpaIAW3NM6vd8WN0VcVbM0l")

diagnostico_data <- drive_ls(path = diagnostico_cultivos)

diagnostico_data |> 
	drive_ls()


# ----

library(httr2)

# test_resp <- request("https://datacrim.inei.gob.pe/csv_controller/index?desde=tematico&id=40514,40515,40516") |> 
test_resp <- request("https://datacrim.inei.gob.pe/csv_controller/index?desde=tematico&id=40173,40514,40515,40516") |> 
	req_perform()

test_resp_str <- test_resp |> 
	resp_body_string()

test_resp_str |> 
	readr::read_csv()

test_resp_str |> 
	stringr::str_sub(end = 250)
