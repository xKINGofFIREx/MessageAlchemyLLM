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

```bash
# 1. Клонируйте репозиторий
git clone https://github.com/xKINGofFIREx/MessageAlchemyLLM.git
cd MessageAlchemyLLM

# 2. Установите зависимости
Rscript requirements.R        # или откройте файл в RStudio

# 3. Запустите Ollama (если ещё не)
ollama serve &

# 4. Подготовьте данные Telegram
#    Сохраните *.json-экспорт (telegram_messages_*.json) в data/

# 5. Постройте отчёт
Rscript build.R               # ETL → анализ → дайджесты → feed.xml

# 6. Откройте веб-интерфейс
R -e "shiny::runApp('app')"   # или Run App в RStudio

После запуска откроется браузер с графиками активности, распределением тональностей и текстовыми дайджестами по дням.
Файл data/feed.xml готов к чтению в любом RSS-ридере.

🛠️ Требования
R ≥ 4.0 и (желательно) RStudio

Ollama с загруженной моделью, умеющей кратко суммировать на русском

Пакеты CRAN: httr, dplyr, lubridate, stringr, jsonlite, shiny, xml2

Установятся автоматически из requirements.R.

Альтернатива: вместо Ollama можно подключить любую LLM по API Hugging Face –
пропишите HF_TOKEN в .Renviron и адаптируйте вызовы в R/summarization.R.

🧬 Стек и архитектура

MessageAlchemyLLM/
├── build.R               # Полный циклический сборщик анализов
├── app/                  # Shiny-приложение
│   └── app.R
├── R/                    # Логика проекта на R
│   ├── etl.R             # Чтение и очистка Telegram JSON
│   ├── summarization.R   # LLM: summary + sentiment
│   └── rss_generator.R   # Экспорт результатов в RSS
├── data/                 # 📥 вход / 📤 выход
│   ├── telegram_messages_*.json
│   ├── messages.rds
│   ├── summaries.rds
│   └── feed.xml
├── requirements.R        # Установка пакетов
└── README.md             # (этот файл)

Поток данных: *.json → ETL → sentiment & summary → messages.rds
→ RSS-генерация → feed.xml → Shiny-UI в браузере.

📜 License
MIT © 2025 xKINGofFIREx
Упомянутые торговые марки принадлежат их владельцам.

