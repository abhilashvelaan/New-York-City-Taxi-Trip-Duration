library(data.table)

#load data usind fread
train = fread("E:\\ATI\\@class\\AB\\@prac\\NYC TAXI\\Data\\train.csv", stringsAsFactors = T)
test = fread("E:\\ATI\\@class\\AB\\@prac\\NYC TAXI\\Data\\test.csv", stringsAsFactors = T)

model_train = train
model_test = test

model_train$pickup_datetime = ymd_hms(as.character(model_train$pickup_datetime),
                                      tz = "America/New_York")
model_test$pickup_datetime = ymd_hms(as.character(model_test$pickup_datetime),
                                     tz = "America/New_York")

model_train$date = as.Date(model_train$pickup_datetime)
model_test$date = as.Date(model_test$pickup_datetime)

model_train$day_pickup = lubridate::wday(model_train$pickup_datetime, label = TRUE, abbr = FALSE)
model_test$day_pickup = lubridate::wday(model_test$pickup_datetime, label = TRUE, abbr = FALSE)

model_train$hour_pickup = hour(model_train$pickup_datetime)
model_test$hour_pickup = hour(model_test$pickup_datetime)

model_train$month_pickup = lubridate::month(model_train$pickup_datetime, label = T, abbr = F)
model_test$month_pickup = lubridate::month(model_test$pickup_datetime, label = T, abbr = F)
model_train$month_pickup = droplevels(model_train$month_pickup)
model_test$month_pickup = droplevels(model_test$month_pickup)

library(geosphere)
model_train$est_dist = distVincentyEllipsoid(p1 = matrix(c(model_train$pickup_longitude,model_train$pickup_latitude), ncol = 2),
                                             p2 = matrix(c(model_train$dropoff_longitude, model_train$dropoff_latitude),ncol = 2)) / 1609.344
model_test$est_dist = distVincentyEllipsoid(p1 = matrix(c(model_test$pickup_longitude,model_test$pickup_latitude), ncol = 2),
                                            p2 = matrix(c(model_test$dropoff_longitude, model_test$dropoff_latitude),ncol = 2)) / 1609.344

model_train$bearing = bearing(p1 = matrix(c(model_train$pickup_longitude,model_train$pickup_latitude), ncol = 2),
                              p2 = matrix(c(model_train$dropoff_longitude, model_train$dropoff_latitude),ncol = 2))
model_test$bearing = bearing(p1 = matrix(c(model_test$pickup_longitude,model_test$pickup_latitude), ncol = 2),
                             p2 = matrix(c(model_test$dropoff_longitude, model_test$dropoff_latitude),ncol = 2))

model_train$Fbearing = finalBearing(p1 = matrix(c(model_train$pickup_longitude,model_train$pickup_latitude), ncol = 2),
                                    p2 = matrix(c(model_train$dropoff_longitude, model_train$dropoff_latitude),ncol = 2))
model_test$Fbearing = finalBearing(p1 = matrix(c(model_test$pickup_longitude,model_test$pickup_latitude), ncol = 2),
                                   p2 = matrix(c(model_test$dropoff_longitude, model_test$dropoff_latitude),ncol = 2))

#airport
jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

pick_coord <- train %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- train %>%
  select(dropoff_longitude, dropoff_latitude)

model_train$jfk_dist_pick <- distVincentyEllipsoid(pick_coord, jfk_coord)
model_train$jfk_dist_drop <- distVincentyEllipsoid(drop_coord, jfk_coord)
model_train$lg_dist_pick <- distVincentyEllipsoid(pick_coord, la_guardia_coord)
model_train$lg_dist_drop <- distVincentyEllipsoid(drop_coord, la_guardia_coord)

model_train <- model_train %>%
  mutate(speed = est_dist/trip_duration*3.6,
         jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
         lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
         blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
  )

pick_coord1 <- test %>%
  select(pickup_longitude, pickup_latitude)
drop_coord1 <- test %>%
  select(dropoff_longitude, dropoff_latitude)

model_test$jfk_dist_pick <- distVincentyEllipsoid(pick_coord1, jfk_coord)
model_test$jfk_dist_drop <- distVincentyEllipsoid(drop_coord1, jfk_coord)
model_test$lg_dist_pick <- distVincentyEllipsoid(pick_coord1, la_guardia_coord)
model_test$lg_dist_drop <- distVincentyEllipsoid(drop_coord1, la_guardia_coord)

model_test <- model_test %>%
  mutate(jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
         lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
         blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
  )

#data cleaning
#model_train <- model_train %>%
 # filter(trip_duration < 22*3600,
  #       est_dist > 0 | (near(est_dist, 0) & trip_duration < 60),
   #      jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5,
    #     trip_duration > 10,
     #    speed < 100)

##external data
# load weather data 
weather = fread("E:\\ATI\\@class\\AB\\@prac\\NYC TAXI\\EXTERNAL DATA\\WEATHER DATA\\weather_data_nyc_centralpark_2016.xls")

#fastest_route data
train_1 = fread("E:\\ATI\\@class\\AB\\@prac\\NYC TAXI\\EXTERNAL DATA\\OSCARLEO-OSRM\\fastest_routes_train_part_1.csv")
train_2 = fread("E:\\ATI\\@class\\AB\\@prac\\NYC TAXI\\EXTERNAL DATA\\OSCARLEO-OSRM\\fastest_routes_train_part_2.csv")
test_1 = fread("E:\\ATI\\@class\\AB\\@prac\\NYC TAXI\\EXTERNAL DATA\\OSCARLEO-OSRM\\fastest_routes_test.csv")

library(tidyverse)
library(stringr)
fastest_route = bind_rows(train_1,train_2,test_1)

foo2 = fastest_route %>% 
  select(id, total_distance, total_travel_time, number_of_steps, step_direction,
         step_maneuvers) %>% 
  mutate(fastest_speed = total_distance / total_travel_time * 3.6,
         left_turns = str_count(step_direction, "left"),
         right_turns = str_count(step_direction, "right"),
         turns = str_count(step_maneuvers, "turn")
  ) %>%
  select(-step_direction, -step_maneuvers)

model_train <- left_join(model_train, foo2, by = "id")

model_test <- left_join(model_test, foo2, by = "id")

#JOIN WEATHER DATA WITH TRAIN AND TEST
library(lubridate)
weather <- weather %>%
  mutate(date = dmy(date),
         rain = as.numeric(ifelse(precipitation == "T", "0.01", precipitation)),
         s_fall = as.numeric(ifelse(`snow fall` == "T", "0.01", `snow fall`)),
         s_depth = as.numeric(ifelse(`snow depth` == "T", "0.01", `snow depth`)),
         all_precip = s_fall + rain,
         has_snow = (s_fall > 0) | (s_depth > 0),
         has_rain = rain > 0,
         max_temp = `maximum temerature`,
         min_temp = `minimum temperature`)

foo1 <- weather %>%
  select(date, rain, s_fall, all_precip, has_snow, has_rain, s_depth, max_temp, min_temp)

model_train <- left_join(model_train, foo1, by = "date")
model_test <- left_join(model_test, foo1, by = "date")

#PCA datetime
library(tidyverse)
train_pickup = model_train %>% select(pickup_latitude, pickup_longitude)
names(train_pickup) = c("lat", "lon")

train_dropoff = model_train %>% select(dropoff_latitude, dropoff_longitude)
names(train_dropoff) = c("lat", "lon")

test_pickup = model_test %>% select(pickup_latitude, pickup_longitude)
names(test_pickup) = c("lat", "lon")

test_dropoff = model_test %>% select(dropoff_latitude, dropoff_longitude)
names(test_dropoff) = c("lat", "lon")

PStrain_pickup = model_train %>% select(pickup_latitude, pickup_longitude)
names(train_pickup) = c("lat", "lon")

train_dropoff = model_train %>% select(dropoff_latitude, dropoff_longitude)
names(train_dropoff) = c("lat", "lon")

PCAdata = rbind(train_pickup, train_dropoff, test_pickup, test_dropoff)
pca = prcomp(PCAdata, scale. = T)

train_pick_pca = predict(pca, newdata = train_pickup)
train_pick_pca = as.data.frame(train_pick_pca)

model_train$pickup_PCA1 = train_pick_pca$PC1
model_train$pickup_PCA2 = train_pick_pca$PC2

train_drop_pca = predict(pca, newdata = train_dropoff)
train_drop_pca = as.data.frame(train_drop_pca)

model_train$drop_PCA1 = train_drop_pca$PC1
model_train$drop_PCA2 = train_drop_pca$PC2

test_pick_pca = predict(pca, newdata = test_pickup)
test_pick_pca = as.data.frame(test_pick_pca)

model_test$pickup_PCA1 = test_pick_pca$PC1
model_test$pickup_PCA2 = test_pick_pca$PC2

test_drop_pca = predict(pca, newdata = test_dropoff)
test_drop_pca = as.data.frame(test_drop_pca)

model_test$drop_PCA1 = test_drop_pca$PC1
model_test$drop_PCA2 = test_drop_pca$PC2

model_train = droplevels(model_train)
model_test = droplevels(model_test)

model_train$store_and_fwd_flag = as.integer(model_train$store_and_fwd_flag)
model_test$store_and_fwd_flag = as.integer(model_test$store_and_fwd_flag)

model_train$day_pickup = as.integer(model_train$day_pickup)
model_test$day_pickup = as.integer(model_test$day_pickup)

model_train$month_pickup = as.integer(model_train$month_pickup)
model_test$month_pickup = as.integer(model_test$month_pickup)

model_train = model_train %>% mutate(trip_duration = log(trip_duration + 1))

model_train = model_train[,-c(1,3,4,6,7,8,9,23)]
model_test = model_test[,-c(1,3,5,6,7,8)]
model_train = model_train[,-5]
model_test = model_test[,-4]
#model_train = model_train[complete.cases(model_train),]
#model_test = model_test[complete.cases(model_test),]

#model_test$jfk_trip = as.integer(model_test$jfk_trip)
#model_test$lg_trip = as.integer(model_test$lg_trip)
#model_test$blizzard = as.integer(model_test$blizzard)
#model_test$has_rain = as.integer(model_test$has_rain)
#model_test$has_snow = as.integer(model_test$has_snow)

#model_train$jfk_trip = as.integer(model_train$jfk_trip)
#model_train$lg_trip = as.integer(model_train$lg_trip)
#model_train$blizzard = as.integer(model_train$blizzard)
#model_train$has_rain = as.integer(model_train$has_rain)
#model_train$has_snow = as.integer(model_train$has_snow)

#XGB
#partition
library(caret)
set.seed(4321)
trainIndex = createDataPartition(model_train$trip_duration, p=0.8, list = F, times = 1)

training = model_train[trainIndex,]
valid = model_train[-trainIndex,]


library(xgboost)
foo = training %>% select(-trip_duration) 
bar = valid %>% select(-trip_duration)

dtrain = xgb.DMatrix(as.matrix(foo), label = training$trip_duration)
dvalid = xgb.DMatrix(as.matrix(bar), label = valid$trip_duration)
dtest = xgb.DMatrix(as.matrix(model_test))

xgb_params = list(colsample_bytree = 0.6, #variables per tree #0.7
                  subsample = 0.6, #data subset per tree #0.7
                  booster = "gbtree",
                  max_depth = 6, #tree levels #5
                  eta = 0.1, #shrinkage #0.3
                  eval_metric = "rmse",
                  objective ="reg:linear",
                  seed = 4321)

watchlist = list(train = dtrain, valid = dvalid)

set.seed(4321)
gb_dt = xgb.train(params = xgb_params,
                  data = dtrain,
                  print_every_n = 50,
                  watchlist = watchlist,
                  nrounds = 1300)

xgb_imp_freq = xgb.importance(feature_names = colnames(dtrain), model = gb_dt)
xgb.plot.importance(xgb_imp_freq)

predictions = predict(gb_dt, newdata = dtest)
predictions = exp(predictions)

sub_xgb = data.frame(id = test$id, trip_duration = predictions)
write.csv(sub_xgb, file = "E:\\ATI\\@class\\AB\\@prac\\NYC TAXI\\submissions\\GBM\\submission_xgb_150917_5.csv",row.names = F)
