require("htmltab")
require("ggplot2")
require("data.table")
require("stringr")
require("lubridate")

#Extract data
data <- as.data.table(htmltab::htmltab("https://en.wikipedia.org/wiki/Women%27s_100_metres_world_record_progression", which = 2)) %>%
		.[, Time := as.numeric(str_replace(Time, "\\(\\d+\\D+\\)", ""))] %>%
		.[, Date := as.Date(str_trim(Date), "%B %d, %Y")] %>%
		.[, Legend := "Observed Values"]

#Model
linear_model <- lm(Time ~ Date, data = data)

#Years 2023 to 2030
future_Date <- as.data.table(seq(ymd('2023-01-01'),ymd('2030-01-01'), by = '1 year')) %>%
				.[, Date := as.Date(V1)] %>%
				.[, V1 := NULL]

#Prediction
predictions <- as.data.table(predict(linear_model, newdata = future_Date)) %>%
				.[, Date := as.Date(future_Date$Date)] %>%
				.[, Legend := "Fitted Values"] 

names(predictions)[1] <- "Time"

#Plot frame
plot_frame <- rbind(data[, c("Time", "Date", "Legend"), with = FALSE],
					predictions[, c("Time", "Date", "Legend"), with = FALSE])

figure <- ggplot(data = plot_frame, aes(x = Date, y = Time, color = Legend))+ 
			geom_point() +
			geom_abline(slope = linear_model$coefficients[2], intercept = linear_model$coefficients[1], 
						color = "blue", linetype = "dashed") +
			ggtitle("Predicted times for women's 100 metres world record") +
			ylab("Time (seconds)") +
			xlab("Year")

figure
