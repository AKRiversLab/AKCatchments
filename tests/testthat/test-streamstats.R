# ak_streamstats() against canned answers from the three StreamStats services
canned <- function(url) {
  if (grepl("pourpoint", url)) return(list(couldSnap = TRUE, output = list(coordinates = list(-149.23, 61.71))))
  ring <- list(list(-149.30, 61.70), list(-149.20, 61.70), list(-149.20, 61.75), list(-149.30, 61.75), list(-149.30, 61.70))
  list(bcrequest = list(wsresp = list(featurecollection = list(list(
    list(name = "globalwatershedpoint", feature = list(features = list())),
    list(name = "globalwatershed", feature = list(features = list(list(
      type = "Feature", properties = list(GlobalWshd = 1L), geometry = list(type = "Polygon", coordinates = list(ring)))))))))))
}

test_that("a basin comes back with its snapped point and, if asked, its characteristics", {
  local_mocked_bindings(
    ak_json = function(url, query = list(), simplify = TRUE) canned(url),
    ak_perform = function(req, path = NULL) httr2::response(body = charToRaw(
      '{"parameters": [{"code": "DRNAREA", "name": "Drainage Area", "unit": "square miles", "value": 12.5},
                       {"code": "PRECIP", "name": "Mean Annual Precipitation", "unit": "inches", "value": -999}]}')))
  ss <- ak_streamstats(61.71, -149.24, site_code = "X", characteristics = TRUE)
  expect_equal(c(ss$ss_lon, ss$ss_lat), c(-149.23, 61.71))
  expect_gt(ss$ss_area_km2, 0)
  bc <- ss$ss_characteristics[[1]]
  expect_equal(bc$code, c("DRNAREA", "PRECIP"))
  expect_equal(bc$value, c(12.5, NA))
  expect_false("ss_characteristics" %in% names(ak_streamstats(61.71, -149.24)))
})

test_that("a point StreamStats can't use is NULL; a server error stops", {
  answer <- function(status) function(url, query = list(), simplify = TRUE) {
    stop(structure(class = c(paste0("httr2_http_", status), "httr2_http", "httr2_error", "rlang_error", "error", "condition"),
                   list(message = paste("HTTP", status), status = status, call = NULL)))
  }
  local_mocked_bindings(ak_json = answer(400))
  expect_message(expect_null(ak_streamstats(64, -147)), "could not use the point")
  local_mocked_bindings(ak_json = answer(503))
  expect_error(ak_streamstats(64, -147), "HTTP 503")
})
