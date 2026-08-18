library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(sf)
library(rnaturalearth)

setwd("C:/Users/andre/OneDrive - Otterbein University/Zoo Comittee/Map For Hall/shiny")

# Load dataset
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
    
    popup = paste0(
      "<div style='margin: -14px -20px; padding: 0; line-height: 0; text-align: center;'>",
      "<img src='", img_src, "' style='width: 320px; height: auto; max-width: 80vw; border-radius: 4px; display: block;' />",
      "</div>"
    )
  )

# Convert map points to spatial sf object
map_sf <- st_as_sf(map_data, coords = c("lng", "lat"), crs = 4326)

# Disable s2 engine for flat geometry calculations
sf_use_s2(FALSE)

# Download admin-1 boundaries (includes both US States and Canadian Provinces)
subregions <- ne_download(scale = 50, type = "states", category = "cultural", returnclass = "sf") %>%
  filter(admin %in% c("United States of America", "Canada")) %>%
  select(region_name = name) %>%
  st_make_valid()

# Load World Countries (excluding US and Canada so we highlight individual provinces/states)
world_rest <- ne_countries(scale = 50, returnclass = "sf") %>%
  filter(!admin %in% c("United States of America", "Canada")) %>%
  select(region_name = name) %>%
  st_make_valid()

# Combine US States, Canadian Provinces, and Other Countries into one spatial layer
all_regions <- rbind(subregions, world_rest)

# Find the single nearest state, province, or country for each internship pin
nearest_indices <- st_nearest_feature(map_sf, all_regions)

# Calculate exact distance from pin to nearest region boundary
distances <- st_distance(map_sf, all_regions[nearest_indices, ], by_element = TRUE)

# Filter: Only highlight regions within 50 km (~31 miles) of an internship pin
max_dist_meters <- 50000
valid_matches <- nearest_indices[as.numeric(distances) <= max_dist_meters]

regions_with_pins <- all_regions[unique(valid_matches), ]

# Re-enable s2 engine
sf_use_s2(TRUE)

# Custom Otterbein Cardinal Red SVG Pin with White Star & Tan Border
otterbein_red_pin <- makeIcon(
  iconUrl = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 36' width='30' height='42'><path fill='%23A6192E' stroke='%23C5A059' stroke-width='1.5' d='M12 0C5.37 0 0 5.37 0 12c0 9 12 24 12 24s12-15 12-24c0-6.63-5.37-12-12-12z'/><path fill='%23FFFFFF' d='M12 5.5l1.8 3.6 4 .6-2.9 2.8.7 4-3.6-1.9-3.6 1.9.7-4-2.9-2.8 4-.6z'/></svg>",
  iconWidth = 30, iconHeight = 42,
  iconAnchorX = 15, iconAnchorY = 42,
  popupAnchorX = 0, popupAnchorY = -38
)

# Custom CSS for Otterbein Red Clusters & Sidebar Buttons
custom_style_css <- tags$style(HTML("
  .bookmark-bar {
    position: absolute;
    top: 20px;
    right: 20px;
    z-index: 1000;
    display: flex;
    flex-direction: column;
    gap: 12px;
    font-family: system-ui, -apple-system, sans-serif;
  }

  .bookmark-btn {
    background-color: #1e3a8a;
    color: #ffffff !important;
    text-decoration: none;
    padding: 12px 20px;
    border-radius: 10px;
    font-size: 15px;
    font-weight: 700;
    border: 3px solid #C5A059;
    box-shadow: 0 6px 16px rgba(0, 0, 0, 0.35);
    transition: all 0.2s ease-in-out;
    display: flex;
    align-items: center;
    gap: 10px;
    cursor: pointer;
  }

  .bookmark-btn:hover {
    background-color: #172554;
    transform: scale(1.04);
  }

  .marker-cluster-small, .marker-cluster-medium, .marker-cluster-large {
    background-color: rgba(166, 25, 46, 0.35) !important;
    border-radius: 50% !important;
  }
  
  .marker-cluster div {
    background-color: #A6192E !important;
    color: #ffffff !important;
    font-family: system-ui, -apple-system, sans-serif !important;
    font-weight: 700 !important;
    font-size: 13px !important;
    border: 2px solid #C5A059 !important;
    box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3) !important;
  }
"))

sidebar_html <- tags$div(
  class = "bookmark-bar",
  tags$button(
    class = "bookmark-btn",
    onclick = "openKioskWindow('https://forms.cloud.microsoft/Pages/ResponsePage.aspx?id=Q2rBH138y0udAwT5a01pALSDb2mRnB9BoQ1g1tPOgdtUMUlNNkVXUVpDMEkwOEIxVlhBUElCNllYMy4u')",
    HTML("📝 Submit your internship info here")
  ),
  tags$button(
    class = "bookmark-btn",
    onclick = "openKioskWindow('https://www.aza.org/Jobs')",
    HTML("💼 AZA Job Board")
  ),
  tags$button(
    class = "bookmark-btn",
    onclick = "openKioskWindow('https://jobs.rwfm.tamu.edu/')",
    HTML("💼 A&M Job Board")
  ),
  tags$button(
    class = "bookmark-btn",
    onclick = "openKioskWindow('https://otterbein.catalog.acalog.com/index.php')",
    HTML("📚 Otterbein Course Catalog")
  ),
  tags$button(
    class = "bookmark-btn",
    onclick = "openKioskWindow('https://otterbein.catalog.acalog.com/content.php?catoid=44&navoid=4272')",
    HTML("🦁 Majors Requirements")
  )
)

idle_js <- "
function(el, x) {
  var map = this;
  var idleTime = 0;
  var IDLE_LIMIT = 120;     // 2 minutes
  var CYCLE_SPEED = 6000;   // 6 seconds
  var TARGET_ZOOM = 8;
  
  var slideshowTimer = null;
  var isIdle = false;
  var currentIdx = 0;
  var clusterLayer = null;
  var activeChildWindow = null;

  map.eachLayer(function(layer) {
    if (layer instanceof L.MarkerClusterGroup) {
      clusterLayer = layer;
    }
  });

  window.openKioskWindow = function(url) {
    resetIdleTimer();
    var width = window.innerWidth * 0.9;
    var height = window.innerHeight * 0.85;
    var left = (window.innerWidth - width) / 2;
    var top = (window.innerHeight - height) / 2;
    
    activeChildWindow = window.open(
      url, 
      '_blank', 
      'width=' + width + ',height=' + height + ',top=' + top + ',left=' + left + ',toolbar=no,location=no,status=no,menubar=no,scrollbars=yes,resizable=yes'
    );
  };

  function resetIdleTimer() {
    idleTime = 0;
    if (isIdle) {
      isIdle = false;
      if (slideshowTimer) clearInterval(slideshowTimer);
      if (activeChildWindow && !activeChildWindow.closed) {
        activeChildWindow.close();
      }
      map.closePopup();
      map.setView([40.125, -82.937], 6);
    }
  }

  function startSlideshow() {
    if (!clusterLayer || isIdle) return;
    var markers = clusterLayer.getLayers();
    if (markers.length === 0) return;
    
    isIdle = true;
    
    if (activeChildWindow && !activeChildWindow.closed) {
      activeChildWindow.close();
    }

    function showNextPin() {
      if (!isIdle) return;
      var m = markers[currentIdx];
      if (m) {
        clusterLayer.zoomToShowLayer(m, function() {
          if (isIdle) {
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

# Build Leaflet Map
map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Highlight states, provinces, and countries containing pins
  addPolygons(
    data = regions_with_pins,
    fillColor = "#C5A059",
    fillOpacity = 0.4,
    color = "#A6192E",
    weight = 1.5,
    stroke = TRUE,
    smoothFactor = 0.5,
    options = pathOptions(interactive = FALSE)
  ) %>%
  # Pin Markers & Clustering
  addMarkers(
    lng = ~lng,
    lat = ~lat,
    popup = ~popup,
    icon = otterbein_red_pin,
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = FALSE,
      zoomToBoundsOnClick = TRUE,
      spiderfyDistanceMultiplier = 2.0,
      maxClusterRadius = 40
    )
  ) %>%
  onRender(idle_js)

# Combine and export to HTML
map_styled <- prependContent(map, custom_style_css, sidebar_html)
saveWidget(map_styled, "index.html", selfcontained = TRUE)