install.packages("reticulate") 
library(reticulate)
library(jsonlite)


# 1. Настройка окружения
setup_environment <- function() {
  # Используем py_require вместо py_install для существующего окружения
  tryCatch({
    py_require("telethon", pip = TRUE)
    py_require("pytz", pip = TRUE)
  }, error = function(e) {
    message("Ошибка установки пакетов: ", e$message)
  })
  
  list(
    telethon = import("telethon"),
    asyncio = import("asyncio"),
    datetime = import("datetime"),
    pytz = import("pytz"),
    os = import("os")
  )
}

# 2. Конфигурация
get_config <- function() {
  list(
    api_id = ***,
    api_hash = "***",
    channel = "Cbpub",
    hours_back = 504,
    output_file = "telegram_messages.json",
    request_limit = 10,
    chunk_size = 100,
    session_name = "r_session_1745403555"
  )
}

# 3. Основная функция с исправленной инициализацией клиента
get_telegram_messages <- function() {
  modules <- setup_environment()
  config <- get_config()
  
  py_code <- sprintf('
from telethon.sync import TelegramClient
from datetime import datetime, timedelta
import asyncio
import pytz
import os

async def fetch_messages():
    session_file = "%s.session"
    new_session = not os.path.exists(session_file)
    
    # Всегда инициализируем клиент с api_id и api_hash
    client = TelegramClient(
        "%s",
        %d,
        "%s",
        device_model="iPhone 15 Pro",
        system_version="17.4.1",
        app_version="10.2.5"
    )
    
    try:
        await client.connect()
        
        if not await client.is_user_authorized():
            if new_session:
                print("Пожалуйста, авторизуйтесь...")
                await client.start()
            else:
                raise Exception("Сессия существует, но не авторизована")
        
        channel = await client.get_entity("%s")
        
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
                
            filtered = [
                msg for msg in messages 
                if msg.date.replace(tzinfo=tz) >= start_date
            ]
            all_messages.extend(filtered)
            
            if messages[-1].date.replace(tzinfo=tz) < start_date:
                break
                
            offset_date = messages[-1].date
            print(f"Собрано {len(all_messages)} сообщений", end="\\r")
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
                     config$session_name, config$session_name, config$api_id, config$api_hash,
                     config$channel, config$hours_back, config$chunk_size, 
                     (60/config$request_limit) + 0.5)
  
  py_run_string(py_code)
  messages <- tryCatch({
    modules$asyncio$run(py$fetch_messages())
  }, error = function(e) {
    message("Ошибка выполнения:")
    print(py_last_error())
    return(NULL)
  })
  
  if (!is.null(messages) && length(messages) > 0) {
    write_json(messages, config$output_file, auto_unbox = TRUE, pretty = TRUE)
    message(sprintf("\nУспешно собрано %d сообщений", length(messages)))
    message(sprintf("Диапазон дат: от %s до %s", 
                    messages[[1]]$date, messages[[length(messages)]]$date))
  } else {
    message("Не удалось получить сообщения. Проверьте:")
    if (!modules$os$path$exists(paste0(config$session_name, ".session"))) {
      message("- Сессия не существует, возможно нужно авторизоваться")
    } else {
      message("- Проблемы с подключением или доступом к каналу")
    }
  }
}

# Запуск
get_telegram_messages()