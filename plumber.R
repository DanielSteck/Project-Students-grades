# Generated by the vetiver package; edit with care

library(pins)
library(plumber)
library(rapidoc)
library(vetiver)
b <- board_folder(path = "C:/Users/Daniel/AppData/Local/Temp/Rtmp8udSvw/pins-9101bffdca")
v <- vetiver_pin_read(b, "Prediction_of_exam_score_model_r", version = "20230420T161308Z-87efe")

#* @plumber
function(pr) {
    pr %>% vetiver_api(v)
}
