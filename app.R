library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(htmltools)

setwd("C:/Users/andre/OneDrive - Otterbein University/Zoo Comittee/Map For Hall/shiny")

# Load your dataset
data <- read_csv("internships_data.csv", show_col_types = FALSE)

map_data <- data %>%
  mutate(
    clean_filename = gsub("^www[/\\\\]", "", img_filename),
    clean_filename = gsub("\\\\", "/", clean_filename),
    
    img_src = if_else(
      !is.na(clean_filename) & clean_filename != "",
      paste0("www/", clean_filename),
      ""
    ),
    
    # Clean popup containing ONLY your graphic
    popup = paste0(
      "<div style='margin: -14px -20px; padding: 0; line-height: 0; text-align: center;'>",
      "<img src='", img_src, "' style='width: 320px; height: auto; max-width: 80vw; border-radius: 4px; display: block;' />",
      "</div>"
    )
  )

# Custom pin icons (Navy star)
pin_icons <- awesomeIcons(
  icon = 'star',
  iconColor = '#ffffff',
  library = 'fa',
  markerColor = 'navy'
)

# Custom CSS for Navy & Gold Cluster Badges
custom_style_css <- tags$style(HTML("
  .marker-cluster-small, 
  .marker-cluster-medium, 
  .marker-cluster-large {
    background-color: rgba(197, 160, 89, 0.35) !important;
    border-radius: 50% !important;
  }
  
  .marker-cluster div {
    background-color: #1e3a8a !important;
    color: #ffffff !important;
    font-family: system-ui, -apple-system, sans-serif !important;
    font-weight: 700 !important;
    font-size: 13px !important;
    border: 2px solid #C5A059 !important;
    box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3) !important;
  }

  .marker-cluster:hover {
    transform: scale(1.1);
    transition: transform 0.2s ease-in-out;
  }
"))

# Idle Rotation Script with Adjustable Zoom Distance
idle_js <- "
function(el, x) {
  var map = this;
  var idleTime = 0;
  var IDLE_LIMIT = 120;     // 2 minutes idle threshold
  var CYCLE_SPEED = 6000;   // 6 seconds per location
  var TARGET_ZOOM = 8;      // <-- CHANGE THIS VALUE (e.g., 6 = further out, 10 = closer in)
  
  var slideshowTimer = null;
  var isIdle = false;
  var currentIdx = 0;
  var clusterLayer = null;

  map.eachLayer(function(layer) {
    if (layer instanceof L.MarkerClusterGroup) {
      clusterLayer = layer;
    }
  });

  function resetIdleTimer() {
    idleTime = 0;
    if (isIdle) {
      isIdle = false;
      if (slideshowTimer) clearInterval(slideshowTimer);
      map.closePopup();
      map.setView([40.125, -82.937], 6); // Default wide view on user tap
    }
  }

  function startSlideshow() {
    if (!clusterLayer || isIdle) return;
    var markers = clusterLayer.getLayers();
    if (markers.length === 0) return;
    
    isIdle = true;

    function showNextPin() {
      if (!isIdle) return;
      var m = markers[currentIdx];
      if (m) {
        clusterLayer.zoomToShowLayer(m, function() {
          if (isIdle) {
            // Smoothly adjust zoom to target distance before popping up
            map.flyTo(m.getLatLng(), TARGET_ZOOM, { duration: 1.2 });
            setTimeout(function() {
              if (isIdle) {
                m.openPopup();
              }
            }, 800);
          }
        });
      }
      currentIdx = (currentIdx + 1) % markers.length;
    }

    showNextPin();
    slideshowTimer = setInterval(showNextPin, CYCLE_SPEED);
  }

  setInterval(function() {
    idleTime++;
    if (idleTime >= IDLE_LIMIT && !isIdle) {
      startSlideshow();
    }
  }, 1000);

  var events = ['mousemove', 'mousedown', 'touchstart', 'click', 'keydown', 'scroll'];
  events.forEach(function(evt) {
    window.addEventListener(evt, resetIdleTimer, true);
    el.addEventListener(evt, resetIdleTimer, true);
  });
}
"

# Build Leaflet map
map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addAwesomeMarkers(
    lng = ~lng,
    lat = ~lat,
    popup = ~popup,
    icon = pin_icons,
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = FALSE,
      zoomToBoundsOnClick = TRUE
    )
  ) %>%
  onRender(idle_js)

# Attach CSS styling to map
map_styled <- prependContent(map, custom_style_css)

# Save HTML
saveWidget(map_styled, "index.html", selfcontained = TRUE)