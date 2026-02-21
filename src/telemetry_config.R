# ============================================================
# TELEMETRY CONFIGURATION FOR MICROBESTUDIO
# Privacy-Compliant Analytics for Multi-User Sessions
# ============================================================
#
# Data Collected:
# - Anonymous session IDs (UUID)
# - Geographic location (country/region from IP, anonymized)
# - App usage patterns (tabs viewed, tools used)
# - Session duration and timestamps
# - User counts (unique sessions)
#
# Data NOT Collected:
# - Personal Identifiable Information (PII)
# - User uploaded data or file contents
# - Input values or form data
# - Full IP addresses (only country/region)
# - Email, names, or any user credentials
#
# ============================================================

library(shiny.telemetry)
library(uuid)
library(DBI)
library(RSQLite)

# ============================================================
# ANONYMOUS IP GEOLOCATION (Privacy-Safe)
# ============================================================
# Uses ipapi.co free API to get country/region from IP
# IP address is NOT stored, only the location
get_user_location <- function(session) {
  tryCatch({
    # Get client IP from session
    client_ip <- session$request$REMOTE_ADDR

    # Don't geolocate local IPs
    if (is.null(client_ip) || client_ip %in% c("127.0.0.1", "::1", "localhost")) {
      return(list(
        country = "Local",
        region = "Development",
        city = "Local",
        ip_anonymized = "127.0.0.x"
      ))
    }

    # Anonymize IP (remove last octet for IPv4, last 4 groups for IPv6)
    ip_parts <- strsplit(client_ip, "\\.")[[1]]
    if (length(ip_parts) == 4) {
      ip_anonymized <- paste(ip_parts[1:3], collapse = ".")
      ip_anonymized <- paste0(ip_anonymized, ".x")
    } else {
      # IPv6 - keep first 4 groups only
      ip_parts_v6 <- strsplit(client_ip, ":")[[1]]
      ip_anonymized <- paste(ip_parts_v6[1:min(4, length(ip_parts_v6))], collapse = ":")
      ip_anonymized <- paste0(ip_anonymized, ":x:x:x:x")
    }

    # Fetch location from ipapi.co (free, no API key needed)
    # Rate limit: 30,000 requests/month
    response <- httr::GET(
      paste0("https://ipapi.co/", client_ip, "/json/"),
      httr::timeout(3)
    )

    if (httr::status_code(response) == 200) {
      location_data <- httr::content(response, as = "parsed")

      return(list(
        country = location_data$country_name %||% "Unknown",
        region = location_data$region %||% "Unknown",
        city = location_data$city %||% "Unknown",
        ip_anonymized = ip_anonymized
      ))
    } else {
      return(list(
        country = "Unknown",
        region = "Unknown",
        city = "Unknown",
        ip_anonymized = ip_anonymized
      ))
    }
  }, error = function(e) {
    warning("Failed to get location: ", e$message)
    return(list(
      country = "Unknown",
      region = "Unknown",
      city = "Unknown",
      ip_anonymized = "Unknown"
    ))
  })
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================
# INITIALIZE TELEMETRY WITH MULTI-USER SUPPORT
# ============================================================
initialize_telemetry <- function() {
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data", recursive = TRUE)
  }

  # Initialize SQLite storage with WAL mode for better concurrency
  db_path <- "data/telemetry.sqlite"

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
      country TEXT,
      region TEXT,
      city TEXT,
      ip_anonymized TEXT,
      user_agent TEXT,
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

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_country
    ON user_sessions(country);
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
  # Generate unique anonymous session ID
  session_uuid <- uuid::UUIDgenerate()
  session_id <- session$token

  # Get user location (anonymized)
  location <- get_user_location(session)

  # Get browser/OS info from user agent
  user_agent <- session$request$HTTP_USER_AGENT %||% "Unknown"
  browser_info <- parse_user_agent(user_agent)

  # Store session metadata in custom table
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")

  tryCatch({
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO user_sessions
      (session_id, session_uuid, session_start, country, region, city,
       ip_anonymized, user_agent, browser, os)
      VALUES (?, ?, datetime('now'), ?, ?, ?, ?, ?, ?, ?);
    ", params = list(
      session_id,
      session_uuid,
      location$country,
      location$region,
      location$city,
      location$ip_anonymized,
      user_agent,
      browser_info$browser,
      browser_info$os
    ))
  }, error = function(e) {
    warning("Failed to store session metadata: ", e$message)
  })

  DBI::dbDisconnect(con)

  # Start shiny.telemetry session (with privacy settings)
  telemetry$start_session(
    track_inputs = FALSE,  # Don't track inputs (prevents PII)
    track_values = FALSE   # Don't track values (prevents user data)
  )

  # Store session UUID in session object for later use
  session$userData$session_uuid <- session_uuid
  session$userData$session_start <- Sys.time()

  # Log session start event
  telemetry$log_custom_event("session_start", list(
    session_uuid = session_uuid,
    country = location$country,
    region = location$region
  ))

  return(session_uuid)
}

# End session and calculate duration
end_telemetry_session <- function(telemetry, session) {
  session_id <- session$token
  session_uuid <- session$userData$session_uuid
  session_start <- session$userData$session_start

  if (!is.null(session_start)) {
    duration_seconds <- as.numeric(difftime(Sys.time(), session_start, units = "secs"))

    # Update session end time and duration
    con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")

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
  session_id <- session$token
  session_uuid <- session$userData$session_uuid

  # Log to custom table
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")

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
  session_id <- session$token
  session_uuid <- session$userData$session_uuid

  # Log to custom table
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")

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
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")
  count <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT session_uuid) as total FROM user_sessions;")
  DBI::dbDisconnect(con)
  return(count$total)
}

# Get users by country
get_users_by_country <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")
  result <- DBI::dbGetQuery(con, "
    SELECT country, COUNT(DISTINCT session_uuid) as users
    FROM user_sessions
    WHERE country != 'Unknown'
    GROUP BY country
    ORDER BY users DESC;
  ")
  DBI::dbDisconnect(con)
  return(result)
}

# Get most used tools
get_most_used_tools <- function(limit = 10) {
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")
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
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")
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
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/telemetry.sqlite")
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
    users_by_country = get_users_by_country(),
    most_used_tools = get_most_used_tools(),
    most_visited_tabs = get_most_visited_tabs(),
    avg_session_duration_minutes = round(get_average_session_duration() / 60, 2)
  )
}
