# **MessageAlchemyLLM** – Shiny-приложение для анализа новостей Telegram-каналов 
*Автоматическая аналитика, дайджесты и RSS-ленты.*

> **TL;DR**  
> Сбросьте экспорт Telegram-сообщений в `data/`, выполните `Rscript build.R`,  
> затем `shiny::runApp("app")` – и получите интерактивные графики, сводки и `feed.xml`.

---

## ⚡️ Почему это круто

* **Sentiment-анализ** – определяем настроение каждого поста (👍 / 😐 / 👎).  
* **Активность по дням** – видно, когда канал «взрывается», а когда молчит.  
* **Одним предложением в день** – LLM-модель собирает ежедневные дайджесты.  
* **RSS-экспорт** – читайте сводки там, где привыкли: в любом RSS-ридере.  
* **100 % офлайн** – LLM работает через **Ollama** (частные данные ≠ в облаке).  

---

## 🚀 Быстрый старт

1. **Клонируйте репозиторий**
   ```bash
   git clone https://github.com/xKINGofFIREx/MessageAlchemyLLM.git
   cd MessageAlchemyLLM
   ```

2. **Установите зависимости**
   ```r
   Rscript requirements.R
   ```
   Или откройте файл `requirements.R` в RStudio и запустите вручную.

3. **Запустите Ollama (если ещё не запущен)**
   ```bash
   ollama serve &
   ```

4. **Подготовьте данные Telegram**  
   Скопируйте `telegram_messages_*.json` в папку `data/`.

5. **Постройте отчёт**
   ```r
   Rscript build.R
   ```
   Этот шаг выполнит ETL, тональность, дайджесты и создаст `feed.xml`.

6. **Запустите веб-интерфейс**
   ```r
   shiny::runApp("app")
   ```
   Или откройте `app/app.R` в RStudio и нажмите **Run App**.

После запуска откроется браузер с графиками активности, распределением тональностей и текстовыми дайджестами по дням.  
Файл `data/feed.xml` готов к чтению в любом RSS-ридере.

---

## 🛠️ Требования

- **R ≥ 4.0** (рекомендуется использовать RStudio)
- **Ollama** с загруженной моделью (например, `qwen:7b`, `mistral`, `deepseek` и др.)
- **CRAN-пакеты**:
  ```r
  install.packages(c("httr", "dplyr", "lubridate", "stringr", 
                     "jsonlite", "shiny", "xml2"))
  ```
  Установятся автоматически при запуске `requirements.R`.

**Альтернатива:**  
Можно использовать API Hugging Face вместо Ollama.  
Для этого:
- Пропишите `HF_TOKEN=your_token` в `.Renviron`
- Отредактируйте `R/summarization.R` под Hugging Face API

---

## 🧬 Стек и архитектура

```plaintext
MessageAlchemyLLM/
├── build.R               # Сценарий полной сборки (ETL + анализ + RSS)
├── app/                  # Shiny-приложение
│   └── app.R             # Интерфейс + серверная логика
├── R/                    # Модули анализа
│   ├── etl.R             # Загрузка и очистка Telegram JSON
│   ├── summarization.R   # Вызов LLM и генерация summary
│   └── rss_generator.R   # Генерация RSS-ленты
├── data/                 # 📥 вход / 📤 выход
│   ├── telegram_messages_*.json
│   ├── messages.rds
│   ├── summaries.rds
│   └── feed.xml
├── requirements.R        # Установка всех зависимостей
└── README.md             # Документация (этот файл)
```

> **Поток данных:**  
> `*.json` → `etl.R` → `summarization.R` → `messages.rds` + `summaries.rds`  
> → `rss_generator.R` → `feed.xml` → визуализация через `app.R`

---

## 📜 Лицензия

**MIT License** © 2025 [xKINGofFIREx](https://github.com/xKINGofFIREx)  
Все упомянутые торговые марки и логотипы принадлежат их законным владельцам.
