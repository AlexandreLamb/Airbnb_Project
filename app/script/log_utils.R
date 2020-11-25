library(log4r)

my_logfile = "log/logfile.txt"

my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE,
                            layout = default_log_layout())

my_logger <- log4r::logger(threshold = "INFO",
                appenders= my_file_appender)

log4r_info <- function(msg) {
  log4r::info(my_logger, msg)
}

log4r_error <- function(msg) {
  log4r::error(my_logger, msg)
}

log4r_debug <- function(msg) {
  log4r::debug(my_logger, msg)
}
