# ============================================================
# TELEMETRY CONFIGURATION FOR MICROBESTUDIO
# Opt-in, consent-gated, minimal usage analytics
# ============================================================
#
# Telemetry is OFF by default. A server operator enables it by setting
# the environment variable MICROBESTUDIO_TELEMETRY=1. Even then, nothing
# is recorded for a session until the user clicks "I Agree" in the
# consent dialog.
#
# Data collected (only when enabled AND consented):
# - Random session ID (UUID)
# - Tabs viewed and tools used (names only)
# - Session start/end and duration
# - Browser and OS family (parsed from the user agent; the raw
#   user-agent string is NOT stored)
#
# Data NOT collected:
# - IP addresses (full or truncated) or any geolocation; no request
#   is made to third-party services
# - Uploaded data, input values, parameters or analysis results
# - Names, emails or credentials
#
# Storage: SQLite file whose path is MICROBESTUDIO_TELEMETRY_DB
# (default "data/telemetry.sqlite"). The file is git-ignored; never
# commit it.
#
# ============================================================

library(shiny.telemetry)
library(uuid)
library(DBI)
library(RSQLite)

# ============================================================
# CONFIGURATION
# ============================================================
telemetry_enabled <- function() {
  tolower(Sys.getenv("MICROBESTUDIO_TELEMETRY", "")) %in% c("1", "true", "yes")
}

telemetry_db_path <- function() {
  Sys.getenv("MICROBESTUDIO_TELEMETRY_DB", "data/telemetry.sqlite")
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================
# INITIALIZE TELEMETRY WITH MULTI-USER SUPPORT
# ============================================================
initialize_telemetry <- function() {
  # Opt-in: return NULL (telemetry fully disabled) unless the operator enabled it
  if (!telemetry_enabled()) {
    return(NULL)
  }

  db_path <- telemetry_db_path()
  if (!dir.exists(dirname(db_path))) {
    dir.create(dirname(db_path), recursive = TRUE)
  }

  # Create storage object
  data_storage <- shiny.telemetry::DataStorageSQLite$new(db_path = db_path)

  # Configure database for multi-user access
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Enable Write-Ahead Logging (WAL) for better concurrent read/write
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")

  # Set busy timeout to handle concurrent writes (5 seconds)
  DBI::dbExecute(con, "PRAGMA busy_timeout=5000;")

  # Enable foreign keys
  DBI::dbExecute(con, "PRAGMA foreign_keys=ON;")

  # Create custom tables for enhanced metrics
  create_custom_tables(con)

  DBI::dbDisconnect(con)

  # Create telemetry object with privacy-safe settings
  telemetry <- shiny.telemetry::Telemetry$new(
    data_storage = data_storage
  )

  return(telemetry)
}

# ============================================================
# CUSTOM DATABASE TABLES FOR ENHANCED METRICS
# ============================================================
create_custom_tables <- function(con) {
  # User sessions table with location and metadata
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_sessions (
      session_id TEXT PRIMARY KEY,
      session_uuid TEXT UNIQUE NOT NULL,
      session_start TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      session_end TIMESTAMP,
      session_duration_seconds INTEGER,
      browser TEXT,
      os TEXT,
      screen_resolution TEXT
    );
  ")

  # Create index for faster queries
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_session_start
    ON user_sessions(session_start);
  ")


  # Page views / tab navigation table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS page_views (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id TEXT NOT NULL,
      tab_name TEXT NOT NULL,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (session_id) REFERENCES user_sessions(session_id)
    );
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_page_views_session
    ON page_views(session_id);
  ")

  # Tool usage table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tool_usage (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id TEXT NOT NULL,
      tool_name TEXT NOT NULL,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (session_id) REFERENCES user_sessions(session_id)
    );
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_tool_usage_session
    ON tool_usage(session_id);
  ")
}

# ============================================================
# SESSION MANAGEMENT FUNCTIONS
# ============================================================
start_telemetry_session <- function(telemetry, session) {
  # Called only after the user has given consent, and only when enabled
  if (is.null(telemetry)) return(invisible(NULL))

  session_uuid <- uuid::UUIDgenerate()
  session_id <- session$token

  # Browser/OS family only; the raw user-agent string is not stored
  browser_info <- parse_user_agent(session$request$HTTP_USER_AGENT %||% "Unknown")

  con <- DBI::dbConnect(RSQLite::SQLite(), telemetry_db_path())
  tryCatch({
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO user_sessions
      (session_id, session_uuid, session_start, browser, os)
      VALUES (?, ?, datetime('now'), ?, ?);
    ", params = list(session_id, session_uuid, browser_info$browser, browser_info$os))
  }, error = function(e) {
    warning("Failed to store session metadata: ", e$message)
  })
  DBI::dbDisconnect(con)

  telemetry$start_session(
    track_inputs = FALSE,  # Don't track inputs (prevents PII)
    track_values = FALSE   # Don't track values (prevents user data)
  )

  session$userData$session_uuid <- session_uuid
  session$userData$session_start <- Sys.time()

  telemetry$log_custom_event("session_start", list(session_uuid = session_uuid))

  invisible(session_uuid)
}

# End session and calculate duration
end_telemetry_session <- function(telemetry, session) {
  if (is.null(telemetry)) return(invisible(NULL))
  session_id <- session$token
  session_uuid <- session$userData$session_uuid
  session_start <- session$userData$session_start

  if (!is.null(session_start)) {
    duration_seconds <- as.numeric(difftime(Sys.time(), session_start, units = "secs"))

    # Update session end time and duration
    con <- DBI::dbConnect(RSQLite::SQLite(), telemetry_db_path())

    tryCatch({
      DBI::dbExecute(con, "
        UPDATE user_sessions
        SET session_end = datetime('now'),
            session_duration_seconds = ?
        WHERE session_id = ?;
      ", params = list(duration_seconds, session_id))
    }, error = function(e) {
      warning("Failed to update session end: ", e$message)
    })

    DBI::dbDisconnect(con)

    # Log session end event
    telemetry$log_custom_event("session_end", list(
      session_uuid = session_uuid,
      duration_seconds = round(duration_seconds)
    ))
  }
}

# ============================================================
# ENHANCED EVENT LOGGING
# ============================================================
log_tab_view <- function(telemetry, session, tab_name) {
  # No-op unless telemetry is enabled and this session consented
  if (is.null(telemetry) || is.null(session$userData$session_uuid)) return(invisible(NULL))
  session_id <- session$token
  session_uuid <- session$userData$session_uuid

  # Log to custom table
  con <- DBI::dbConnect(RSQLite::SQLite(), telemetry_db_path())

  tryCatch({
    DBI::dbExecute(con, "
      INSERT INTO page_views (session_id, tab_name, timestamp)
      VALUES (?, ?, datetime('now'));
    ", params = list(session_id, tab_name))
  }, error = function(e) {
    warning("Failed to log page view: ", e$message)
  })

  DBI::dbDisconnect(con)

  # Also log to shiny.telemetry for compatibility
  telemetry$log_custom_event("tab_view", list(
    session_uuid = session_uuid,
    tab = tab_name
  ))
}

log_tool_usage <- function(telemetry, session, tool_name) {
  # No-op unless telemetry is enabled and this session consented
  if (is.null(telemetry) || is.null(session$userData$session_uuid)) return(invisible(NULL))
  session_id <- session$token
  session_uuid <- session$userData$session_uuid

  # Log to custom table
  con <- DBI::dbConnect(RSQLite::SQLite(), telemetry_db_path())

  tryCatch({
    DBI::dbExecute(con, "
      INSERT INTO tool_usage (session_id, tool_name, timestamp)
      VALUES (?, ?, datetime('now'));
    ", params = list(session_id, tool_name))
  }, error = function(e) {
    warning("Failed to log tool usage: ", e$message)
  })

  DBI::dbDisconnect(con)

  # Also log to shiny.telemetry for compatibility
  telemetry$log_custom_event("tool_used", list(
    session_uuid = session_uuid,
    tool = tool_name
  ))
}

# ============================================================
# USER AGENT PARSING (Browser/OS Detection)
# ============================================================
parse_user_agent <- function(user_agent) {
  browser <- "Unknown"
  os <- "Unknown"

  if (is.null(user_agent) || user_agent == "") {
    return(list(browser = browser, os = os))
  }

  # Detect browser
  if (grepl("Edge", user_agent)) {
    browser <- "Edge"
  } else if (grepl("Chrome", user_agent)) {
    browser <- "Chrome"
  } else if (grepl("Safari", user_agent) && !grepl("Chrome", user_agent)) {
    browser <- "Safari"
  } else if (grepl("Firefox", user_agent)) {
    browser <- "Firefox"
  } else if (grepl("MSIE|Trident", user_agent)) {
    browser <- "Internet Explorer"
  }

  # Detect OS
  if (grepl("Windows", user_agent)) {
    os <- "Windows"
  } else if (grepl("Mac OS X", user_agent)) {
    os <- "macOS"
  } else if (grepl("Linux", user_agent)) {
    os <- "Linux"
  } else if (grepl("Android", user_agent)) {
    os <- "Android"
  } else if (grepl("iOS|iPhone|iPad", user_agent)) {
    os <- "iOS"
  }

  return(list(browser = browser, os = os))
}

# ============================================================
# ANALYTICS QUERY FUNCTIONS
# ============================================================

# Get total unique users (sessions)
get_total_users <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), telemetry_db_path())
  count <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT session_uuid) as total FROM user_sessions;")
  DBI::dbDisconnect(con)
  return(count$total)
}

# Get most used tools
get_most_used_tools <- function(limit = 10) {
  con <- DBI::dbConnect(RSQLite::SQLite(), telemetry_db_path())
  result <- DBI::dbGetQuery(con, sprintf("
    SELECT tool_name, COUNT(*) as usage_count
    FROM tool_usage
    GROUP BY tool_name
    ORDER BY usage_count DESC
    LIMIT %d;
  ", limit))
  DBI::dbDisconnect(con)
  return(result)
}

# Get most visited tabs
get_most_visited_tabs <- function(limit = 10) {
  con <- DBI::dbConnect(RSQLite::SQLite(), telemetry_db_path())
  result <- DBI::dbGetQuery(con, sprintf("
    SELECT tab_name, COUNT(*) as view_count
    FROM page_views
    GROUP BY tab_name
    ORDER BY view_count DESC
    LIMIT %d;
  ", limit))
  DBI::dbDisconnect(con)
  return(result)
}

# Get average session duration
get_average_session_duration <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), telemetry_db_path())
  result <- DBI::dbGetQuery(con, "
    SELECT AVG(session_duration_seconds) as avg_duration_seconds
    FROM user_sessions
    WHERE session_duration_seconds IS NOT NULL;
  ")
  DBI::dbDisconnect(con)
  return(result$avg_duration_seconds)
}

# Export analytics summary
export_analytics_summary <- function() {
  list(
    total_users = get_total_users(),
    most_used_tools = get_most_used_tools(),
    most_visited_tabs = get_most_visited_tabs(),
    avg_session_duration_minutes = round(get_average_session_duration() / 60, 2)
  )
}
