# =============================================================================
## ultima propuesta,
# MAPA INTERACTIVO - ASISTENCIA TÉCNICA P504B PUNO ENA 2024
#1 Asesor del establecimiento comercial autorizado
#2 Médico veterinario
#3 Ingeniero zootecnista
#4 Personal de SENASA
#5 El mismo productor/a
#6 Técnico agropecuario
#7 Otro
# =============================================================================

library(leaflet)
library(htmlwidgets)
library(dplyr)
library(haven)

# =============================================================================
# COORDENADAS DE DISTRITOS Y PROVINCIAS DE PUNO
# =============================================================================

obtener_coordenadas_distritos_puno <- function() {
  data.frame(
    codigo = c(
      "210101", "210102", "210103", "210104", "210105", "210106", "210107", 
      "210108", "210109", "210110", "210111", "210112", "210113", "210114", "210115",
      "210201", "210202", "210203", "210204", "210205", "210206", "210207", 
      "210208", "210209", "210210", "210211", "210212", "210213", "210214", "210215",
      "210301", "210302", "210303", "210304", "210305", "210306", "210307", 
      "210308", "210309", "210310",
      "210401", "210402", "210403", "210404", "210405", "210406", "210407",
      "210501", "210502", "210503", "210504", "210505",
      "210601", "210602", "210603", "210604", "210605", "210606", "210607", "210608",
      "210701", "210702", "210703", "210704", "210705", "210706", "210707", 
      "210708", "210709", "210710",
      "210801", "210802", "210803", "210804", "210805", "210806", "210807", 
      "210808", "210809",
      "210901", "210902", "210903", "210904",
      "211001", "211002", "211003", "211004", "211005",
      "211101", "211102", "211103", "211104",
      "211201", "211202", "211203", "211204", "211205", "211206", "211207", 
      "211208", "211209", "211210",
      "211301", "211302", "211303", "211304", "211305", "211306", "211307", "211308"
    ),
    
    nombre_distrito = c(
      "PUNO", "ACORA", "AMANTANI", "ATUNCOLLA", "CAPACHICA", "CHUCUITO", "COATA", 
      "HUATA", "MAÑAZO", "PAUCARCOLLA", "PICHACANI", "PLATERIA", "SAN ANTONIO", 
      "TIQUILLACA", "VILQUE",
      "AZANGARO", "ACHAYA", "ARAPA", "ASILLO", "CAMINACA", "CHUPA", "JOSE DOMINGO CHOQUEHUANCA",
      "MUÑANI", "POTONI", "SAMAN", "SAN ANTON", "SAN JOSE", "SAN JUAN DE SALINAS", 
      "SANTIAGO DE PUPUJA", "TIRAPATA",
      "MACUSANI", "AJOYANI", "AYAPATA", "COASA", "CORANI", "CRUCERO", "ITUATA",
      "OLLACHEA", "SAN GABAN", "USICAYOS",
      "JULI", "DESAGUADERO", "HUACULLANI", "KELLUYO", "PISACOMA", "POMATA", "ZEPITA",
      "ILAVE", "CAPAZO", "PILCUYO", "SANTA ROSA", "CONDURIRI",
      "HUANCANE", "COJATA", "HUATASANI", "INCHUPALLA", "PUSI", "ROSASPATA", "TARACO", "VILQUE CHICO",
      "LAMPA", "CABANILLA", "CALAPUJA", "NICASIO", "OCUVIRI", "PALCA", "PARATIA", 
      "PUCARA", "SANTA LUCIA", "VILAVILA",
      "AYAVIRI", "ANTAUTA", "CUPI", "LLALLI", "MACARI", "NUÑOA", "ORURILLO", "SANTA ROSA", "UMACHIRI",
      "MOHO", "CONIMA", "HUAYRAPATA", "TILALI",
      "PUTINA", "ANANEA", "PEDRO VILCA APAZA", "QUILCAPUNCU", "SINA",
      "JULIACA", "CABANA", "CABANILLAS", "CARACOTO",
      "SANDIA", "CUYOCUYO", "LIMBANI", "PATAMBUCO", "PHARA", "QUIACA", "SAN JUAN DEL ORO",
      "YANAHUAYA", "ALTO INAMBARI", "SAN PEDRO DE PUTINA PUNCO",
      "YUNGUYO", "ANAPIA", "COPANI", "CUTURAPI", "OLLARAYA", "TINICACHI", "UNICACHI", "DESAGUADERO"
    ),
    
    nombre_provincia = c(
      rep("PUNO", 15), rep("AZANGARO", 15), rep("CARABAYA", 10), rep("CHUCUITO", 7),
      rep("EL COLLAO", 5), rep("HUANCANE", 8), rep("LAMPA", 10), rep("MELGAR", 9),
      rep("MOHO", 4), rep("SAN ANTONIO DE PUTINA", 5), rep("SAN ROMAN", 4),
      rep("SANDIA", 10), rep("YUNGUYO", 8)
    ),
    
    lat = c(
      -15.8402, -15.9667, -15.6667, -15.7500, -15.6333, -16.0500, -15.6167, 
      -15.5333, -15.7833, -15.7667, -16.0667, -16.0167, -15.9833, -15.8167, -15.8500,
      -14.9067, -14.8500, -15.1333, -14.6833, -14.9500, -15.2833, -14.7333,
      -14.7833, -15.0500, -15.1167, -14.9833, -14.8833, -15.3167, -14.7667, -15.2500,
      -14.0667, -13.8333, -13.6167, -14.1667, -13.8667, -14.3667, -13.9833,
      -13.6833, -13.5333, -14.2833,
      -16.2167, -16.5667, -16.6333, -16.4000, -16.8833, -16.2667, -16.4833,
      -16.0833, -17.1833, -16.0333, -16.2333, -16.6167,
      -15.2000, -15.0333, -15.0667, -15.1333, -15.4167, -15.0833, -15.3000, -15.1667,
      -15.3667, -15.6333, -15.3167, -15.4833, -15.6167, -15.2500, -15.5167,
      -15.0833, -15.6833, -15.1500,
      -14.8833, -14.8000, -14.9333, -14.8167, -14.7167, -14.4833, -14.7500, -14.6167, -14.8333,
      -15.3833, -15.5833, -15.4500, -15.2833,
      -14.9000, -14.6833, -14.8667, -15.0167, -14.7000,
      -15.5000, -15.6167, -15.5833, -15.4167,
      -14.2833, -14.4833, -14.0833, -14.1333, -13.9167, -14.5667, -14.1667,
      -14.2167, -13.8000, -14.0167,
      -16.2500, -16.2167, -16.1833, -16.1167, -16.0833, -16.2833, -16.1500, -16.5667
    ),
    
    lng = c(
      -70.0199, -69.7667, -69.7000, -70.0333, -69.8667, -69.8833, -70.0833,
      -69.9000, -70.3167, -70.1000, -69.7333, -69.7833, -69.8167, -70.1333, -70.0667,
      -70.1833, -70.0167, -70.1167, -69.4833, -70.0500, -69.9500, -69.3167,
      -69.9167, -69.8333, -69.7833, -69.2333, -69.3833, -69.6667, -69.5167, -69.8833,
      -70.4167, -70.2833, -70.5833, -70.1333, -70.6333, -70.0167, -70.3833,
      -70.7833, -70.3833, -70.0833,
      -69.4667, -69.0333, -69.1833, -69.2833, -69.8500, -69.2833, -69.1167,
      -69.6333, -69.7167, -69.7833, -69.5167, -69.3833,
      -69.7833, -69.5667, -69.8167, -69.9333, -69.8833, -69.6833, -69.9833, -69.6167,
      -70.3667, -70.3833, -70.1167, -70.4333, -70.6500, -70.6833, -70.1833,
      -70.3167, -70.6167, -70.2833,
      -70.5833, -70.6500, -70.3167, -70.8000, -70.5500, -70.6333, -70.5000, -70.7833, -70.6167,
      -69.5000, -69.1167, -69.4333, -69.6333,
      -69.8667, -69.5333, -69.9500, -69.7833, -69.4833,
      -70.1333, -70.2833, -70.3500, -70.0833,
      -69.4500, -69.1833, -69.7333, -69.5833, -69.5167, -69.0333, -69.2833,
      -69.4833, -69.2167, -69.3333,
      -69.0833, -69.0167, -69.2333, -69.1833, -69.0833, -69.0333, -69.1000, -69.0333
    )
  )
}

obtener_coordenadas_provincias <- function() {
  data.frame(
    nombre_provincia = c("PUNO", "AZANGARO", "CARABAYA", "CHUCUITO", "EL COLLAO", "HUANCANE", "LAMPA", "MELGAR", "MOHO", "SAN ANTONIO DE PUTINA", "SAN ROMAN", "SANDIA", "YUNGUYO"),
    lat = c(-15.8402, -14.9067, -14.0667, -16.2167, -16.0833, -15.2000, -15.3667, -14.8833, -15.3833, -14.9000, -15.5000, -14.2833, -16.2500),
    lng = c(-70.0199, -70.1833, -70.4167, -69.4667, -69.6333, -69.7833, -70.3667, -70.5833, -69.5000, -69.8667, -70.1333, -69.4500, -69.0833)
  )
}

# =============================================================================
# CARGAR Y PROCESAR DATOS P504B
# =============================================================================

cargar_datos_p504b_2024 <- function(ruta_archivo = "D:/decimo 10/Estadistica Espacial/2024/2024 - DESCOMPRIMIDO/973-Modulo1905_vacunas/13_CAP500AB.sav") {
  
  data <- read_sav(ruta_archivo)
  
  puno_data_clean <- data %>%
    filter(NOMBREDD == "PUNO") %>%
    select(ANIO, CCDD, NOMBREDD, CCPP, NOMBREPV, CCDI, NOMBREDI, P504B) %>%
    filter(!is.na(P504B)) %>%
    mutate(
      codigo_distrito = paste0(CCDD, CCPP, CCDI),
      asistencia_codigo = as.numeric(P504B),
      distrito_limpio = trimws(toupper(NOMBREDI)),
      provincia_limpio = trimws(toupper(NOMBREPV))
    )
  
  mapeo_asistencia <- data.frame(
    codigo = 1:7,
    tipo_asistencia = c(
      "Asesor del establecimiento comercial autorizado",
      "Médico veterinario",
      "Ingeniero zootecnista",
      "Personal de SENASA",
      "El mismo productor/a",
      "Técnico agropecuario",
      "Otro"
    ),
    color = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7", "#9B59B6", "#95A5A6"),
    categoria = c("Comercial", "Profesional", "Profesional", "Público", "Propio", "Técnico", "Otros")
  )
  
  datos_resumen <- puno_data_clean %>%
    group_by(distrito_limpio, provincia_limpio, asistencia_codigo) %>%
    summarise(cantidad = n(), .groups = 'drop') %>%
    left_join(mapeo_asistencia, by = c("asistencia_codigo" = "codigo")) %>%
    group_by(distrito_limpio, provincia_limpio) %>%
    mutate(
      total_distrito = sum(cantidad),
      porcentaje = round(cantidad/total_distrito * 100, 1)
    ) %>%
    ungroup()
  
  return(list(
    datos = datos_resumen,
    mapeo_asistencia = mapeo_asistencia,
    datos_raw = puno_data_clean
  ))
}

# =============================================================================
# CREAR MAPA INTERACTIVO
# =============================================================================

crear_mapa_asistencia_ena <- function(ruta_archivo = NULL) {
  
  distritos_coords <- obtener_coordenadas_distritos_puno()
  provincias_coords <- obtener_coordenadas_provincias()
  
  if(is.null(ruta_archivo)) {
    ruta_archivo <- "D:/decimo 10/Estadistica Espacial/2024/2024 - DESCOMPRIMIDO/973-Modulo1905_vacunas/13_CAP500AB.sav"
  }
  
  datos_sistema <- cargar_datos_p504b_2024(ruta_archivo)
  datos_asistencia <- datos_sistema$datos
  mapeo_asistencia <- datos_sistema$mapeo_asistencia
  
  # Datos por distrito
  mapa_datos_distrito <- merge(
    distritos_coords, 
    datos_asistencia %>% 
      group_by(distrito_limpio, provincia_limpio) %>% 
      summarise(total = sum(cantidad), .groups = 'drop'),
    by.x = "nombre_distrito", 
    by.y = "distrito_limpio",
    all.x = TRUE
  ) %>%
    mutate(total = ifelse(is.na(total), 0, total))
  
  # Datos por provincia
  mapa_datos_provincia <- merge(
    provincias_coords,
    datos_asistencia %>% 
      group_by(provincia_limpio) %>% 
      summarise(total = sum(cantidad), .groups = 'drop'),
    by.x = "nombre_provincia",
    by.y = "provincia_limpio",
    all.x = TRUE
  ) %>%
    mutate(total = ifelse(is.na(total), 0, total))
  
  # Crear popups para distritos
  crear_popup_distrito <- function(distrito_name) {
    datos_dist <- datos_asistencia[datos_asistencia$distrito_limpio == distrito_name, ]
    if(nrow(datos_dist) == 0) {
      return(paste0("<b>DISTRITO: ", distrito_name, "</b><br/>Sin datos disponibles"))
    }
    
    provincia <- unique(datos_dist$provincia_limpio)[1]
    popup_html <- paste0("<b>DISTRITO: ", distrito_name, "</b><br/>",
                         "<b>PROVINCIA: ", provincia, "</b><br/><hr>")
    
    for(i in 1:nrow(datos_dist)) {
      if (datos_dist$cantidad[i] > 0) {
        popup_html <- paste0(popup_html,
                             "<b>", datos_dist$tipo_asistencia[i], "</b>: ", 
                             datos_dist$cantidad[i], " (", datos_dist$porcentaje[i], "%)<br/>")
      }
    }
    popup_html <- paste0(popup_html, "<hr><b>Total:</b> ", sum(datos_dist$cantidad))
    return(popup_html)
  }
  
  # Crear popups para provincias
  crear_popup_provincia <- function(provincia_name) {
    datos_prov <- datos_asistencia[datos_asistencia$provincia_limpio == provincia_name, ]
    if(nrow(datos_prov) == 0) {
      return(paste0("<b>PROVINCIA: ", provincia_name, "</b><br/>Sin datos disponibles"))
    }
    
    popup_html <- paste0("<b>PROVINCIA: ", provincia_name, "</b><br/><hr>")
    
    resumen_prov <- datos_prov %>%
      group_by(tipo_asistencia) %>%
      summarise(cantidad = sum(cantidad), .groups = 'drop') %>%
      mutate(porcentaje = round(cantidad/sum(cantidad) * 100, 1))
    
    for(i in 1:nrow(resumen_prov)) {
      popup_html <- paste0(popup_html,
                           "<b>", resumen_prov$tipo_asistencia[i], "</b>: ", 
                           resumen_prov$cantidad[i], " (", resumen_prov$porcentaje[i], "%)<br/>")
    }
    popup_html <- paste0(popup_html, "<hr><b>Total:</b> ", sum(resumen_prov$cantidad))
    return(popup_html)
  }
  
  # Crear mapa base
  mapa <- leaflet() %>%
    addTiles(group = "Mapa Base") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
    setView(lng = -70.0199, lat = -15.2, zoom = 8)
  
  # Añadir círculos totales por distrito
  mapa <- mapa %>%
    addCircleMarkers(
      data = mapa_datos_distrito,
      lng = ~lng, 
      lat = ~lat,
      radius = ~pmax(sqrt(total) * 0.8, 3),
      color = "#2c3e50",
      fillColor = "#e74c3c",
      fillOpacity = 0.7,
      popup = ~sapply(nombre_distrito, crear_popup_distrito),
      label = ~paste(nombre_distrito, "-", total, "casos"),
      group = "Totales por Distrito"
    )
  
  # Añadir círculos totales por provincia
  mapa <- mapa %>%
    addCircleMarkers(
      data = mapa_datos_provincia,
      lng = ~lng, 
      lat = ~lat,
      radius = ~pmax(sqrt(total) * 1.2, 5),
      color = "#34495e",
      fillColor = "#f39c12",
      fillOpacity = 0.8,
      popup = ~sapply(nombre_provincia, crear_popup_provincia),
      label = ~paste("PROVINCIA:", nombre_provincia, "-", total, "casos"),
      group = "Totales por Provincia"
    )
  
  # Añadir círculos por tipo de asistencia
  for(i in 1:nrow(mapeo_asistencia)) {
    tipo <- mapeo_asistencia$tipo_asistencia[i]
    color <- mapeo_asistencia$color[i]
    
    datos_tipo_i <- merge(
      datos_asistencia[datos_asistencia$tipo_asistencia == tipo & datos_asistencia$cantidad > 0, ],
      distritos_coords,
      by.x = "distrito_limpio", by.y = "nombre_distrito"
    )
    
    if (nrow(datos_tipo_i) > 0) {
      mapa <- mapa %>%
        addCircleMarkers(
          data = datos_tipo_i,
          lng = ~lng + runif(nrow(datos_tipo_i), -0.02, 0.02),
          lat = ~lat + runif(nrow(datos_tipo_i), -0.02, 0.02),
          radius = ~pmax(sqrt(cantidad) * 1.5, 2),
          color = color,
          fillColor = color,
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0("<b>DISTRITO: ", distrito_limpio, "</b><br/>",
                          "<b>PROVINCIA: ", provincia_limpio, "</b><br/>",
                          "<b>", tipo_asistencia, "</b>: ", cantidad, " casos (", porcentaje, "%)"),
          label = ~paste(tipo_asistencia, ":", cantidad, "casos"),
          group = tipo
        )
    }
  }
  
  # Control de capas
  grupos_overlay <- c("Totales por Distrito", "Totales por Provincia", mapeo_asistencia$tipo_asistencia)
  
  mapa <- mapa %>%
    addLayersControl(
      position = "topleft",
      baseGroups = c("Mapa Base", "Satélite"),
      overlayGroups = grupos_overlay,
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    showGroup("Totales por Distrito") %>%
    hideGroup("Totales por Provincia") %>%
    hideGroup(mapeo_asistencia$tipo_asistencia[4:7])
  
  # Leyenda
  mapa <- mapa %>%
    addLegend(
      position = "bottomright",
      colors = mapeo_asistencia$color,
      labels = mapeo_asistencia$tipo_asistencia,
      title = "Tipos de Asistencia Técnica (P504B)",
      opacity = 0.8
    )
  
  # Información estadística
  total_registros <- sum(datos_asistencia$cantidad)
  distritos_con_datos <- length(unique(datos_asistencia$distrito_limpio))
  provincias_con_datos <- length(unique(datos_asistencia$provincia_limpio))
  
  stats_por_categoria <- datos_asistencia %>%
    group_by(categoria) %>%
    summarise(total = sum(cantidad), .groups = 'drop') %>%
    mutate(porcentaje = round(total/total_registros * 100, 1))
  
  stats_html <- paste0(
    "<b>Por categoría:</b><br/>",
    paste(apply(stats_por_categoria, 1, function(x) 
      paste0("• ", x[1], ": ", x[2], " (", x[3], "%)")), collapse = "<br/>")
  )
  
  mapa <- mapa %>%
    addControl(
      html = paste0(
        "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.3); max-width: 220px;'>",
        "<h4>ENA 2024 - PUNO</h4>",
        "<h5>Asistencia Técnica (P504B)</h5>",
        "<b>Total registros:</b> ", format(total_registros, big.mark = ","), "<br/>",
        "<b>Distritos:</b> ", distritos_con_datos, "/", nrow(distritos_coords), "<br/>",
        "<b>Provincias:</b> ", provincias_con_datos, "/13<br/><br/>",
        stats_html, "<br/><br/>",
        "<small><b>Fuente:</b> ENA 2024<br/>Módulo 1905 - Cap. 500AB</small>",
        "</div>"
      ),
      position = "bottomleft"
    )
  
  return(mapa)
}

# =============================================================================
# EJECUTAR
# =============================================================================

mapa_final <- crear_mapa_asistencia_ena()
print(mapa_final)
saveWidget(mapa_final, file = "001mapa_asistencia_tecnica_puno_ena2024.html", selfcontained = TRUE)
