library(reticulate)
library(jsonlite)

# 1. Настройка окружения с автоматической установкой зависимостей
setup_environment <- function() {
  # Устанавливаем необходимые Python-пакеты
  required_packages <- c("telethon", "pytz")
  for (pkg in required_packages) {
    tryCatch({
      import(pkg)
    }, error = function(e) {
      message(sprintf("Установка %s...", pkg))
      py_install(pkg, pip = TRUE)
    })
  }
  
  list(
    telethon = import("telethon"),
    asyncio = import("asyncio"),
    datetime = import("datetime"),
    pytz = import("pytz")
  )
}

# 2. Конфигурация
get_config <- function() {
  list(
    api_id = 22471500,
    api_hash = "15728315b8e1de07c28e23bad5fa6619",
    channel = "bbbreaking",
    hours_back = 24,
    output_file = "telegram_messages.json",
    request_limit = 10,
    chunk_size = 100
  )
}

# 3. Основная функция с улучшенной обработкой дат
get_telegram_messages <- function() {
  # Инициализация
  modules <- setup_environment()
  config <- get_config()
  
  # Создаем Python скрипт с обработкой временных зон
  py_code <- sprintf('
from telethon.sync import TelegramClient
from datetime import datetime, timedelta
import asyncio
import pytz

async def fetch_messages():
    client = TelegramClient(
        "r_session_%d",
        %d,
        "%s",
        device_model="iPhone 15 Pro",
        system_version="17.4.1",
        app_version="10.2.5"
    )
    
    try:
        await client.start()
        channel = await client.get_entity("%s")
        
        # Устанавливаем временную зону UTC
        tz = pytz.UTC
        end_date = datetime.now(tz)
        start_date = end_date - timedelta(hours=%d)
        all_messages = []
        offset_date = end_date
        
        while True:
            messages = await client.get_messages(
                channel,
                limit=%d,
                offset_date=offset_date
            )
            
            if not messages:
                break
                
            # Фильтруем сообщения по дате
            filtered = [
                msg for msg in messages 
                if msg.date.replace(tzinfo=tz) >= start_date
            ]
            all_messages.extend(filtered)
            
            # Проверяем, вышли ли за временной диапазон
            if messages[-1].date.replace(tzinfo=tz) < start_date:
                break
                
            offset_date = messages[-1].date
            print(f"Собрано {len(all_messages)} сообщений", end="\\r")
            
            # Искусственная задержка между запросами
            await asyncio.sleep(%f)
            
        return [{
            "id": msg.id,
            "date": msg.date.replace(tzinfo=tz).isoformat(),
            "text": msg.text or "",
            "media": bool(msg.media)
        } for msg in all_messages]
        
    finally:
        await client.disconnect()
', 
                     as.integer(Sys.time()), config$api_id, config$api_hash, 
                     config$channel, config$hours_back, config$chunk_size,
                     (60/config$request_limit) + 0.5)  # Задержка с небольшим рандомным смещением
  
  # Выполняем Python код
  py_run_string(py_code)
  messages <- tryCatch({
    modules$asyncio$run(py$fetch_messages())
  }, error = function(e) {
    message("Ошибка выполнения:")
    print(py_last_error())
    return(NULL)
  })
  
  # Сохранение результатов
  if (!is.null(messages) && length(messages) > 0) {
    write_json(messages, config$output_file, auto_unbox = TRUE, pretty = TRUE)
    message(sprintf("\nУспешно собрано %d сообщений за последние %d часов", 
                    length(messages), config$hours_back))
    message(sprintf("Диапазон дат: от %s до %s", 
                    messages[[1]]$date, messages[[length(messages)]]$date))
    message(sprintf("Данные сохранены в файл: %s", config$output_file))
  } else {
    message("Не удалось получить сообщения. Проверьте:")
    message("- Доступ к каналу @", config$channel)
    message("- Авторизацию в Telegram")
    message("- Интернет-соединение")
  }
}

# Запуск
get_telegram_messages()