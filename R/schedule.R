schedule_cron <- function(script_path = "R/get_messages.R", hour = 10) {
  cron_line <- sprintf("0 %d * * * Rscript %s >> data/log.txt 2>&1", hour, normalizePath(script_path))
  cron_file <- tempfile()
  
  existing <- tryCatch(system("crontab -l", intern = TRUE), error = function(e) character())
  if (any(grepl(script_path, existing))) {
    message("Сron-задача уже существует.")
    return(invisible())
  }
  
  writeLines(c(existing, cron_line), cron_file)
  system(sprintf("crontab %s", cron_file))
  message("Сron-задача добавлена для ежедневного запуска в ", hour, ":00")
}

schedule_windows_task <- function(script_path = "R/get_messages.R", hour = 10) {
  # Используем допустимое значение winslash = "/"
  script_abs_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
  rscript_path <- normalizePath(file.path(R.home("bin"), "Rscript.exe"), winslash = "/", mustWork = TRUE)
  task_name <- "TelegramMessageFetcher"
  
  cmd <- sprintf(
    'schtasks /Create /F /SC DAILY /TN "%s" /TR "\\"%s\\" \\"%s\\"" /ST %02d:00',
    task_name, rscript_path, script_abs_path, hour
  )
  
  system(cmd)
  message("Задача добавлена в Планировщик Windows: ", task_name)
}


setup_schedule <- function(hour = 10) {
  if (.Platform$OS.type == "windows") {
    schedule_windows_task(hour = hour)
  } else {
    schedule_cron(hour = hour)
  }
}

# Вызов
setup_schedule(hour = 21)
