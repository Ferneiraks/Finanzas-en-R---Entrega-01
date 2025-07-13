ProyectoAnalisisInversionesReportGenerator <- function() {
  
  # Datos del proyecto
  project_data <- list(
    nombre = "Proyecto Análisis de Inversiones",
    subtitle = "Proyecto del Curso - Finanzas en R",
    autor = "Fernando Neira - Compañía de Seguros",
    objetivo_principal = "Optimizar decisiones de inversión mediante análisis automatizado comparativo con TPM",
    alcance_instrumentos = 50,
    precision_objetivo = 100
  )
  
  # Función para crear tabla de stakeholders (todas frecuencias semanales)
  create_stakeholders_table <- function() {
    stakeholders <- data.frame(
      Stakeholder = c(
        "Gerencia de Inversiones",
        "Analistas Financieros", 
        "Comité de Inversiones",
        "CMF"
      ),
      Impacto = c(
        "Decisiones estratégicas de inversión",
        "Evaluación diaria de oportunidades",
        "Reportes ejecutivos y seguimiento",
        "Supervisión regulatoria y cumplimiento"
      ),
      Frecuencia = c(
        "Semanal",
        "Semanal", 
        "Semanal",
        "Semanal"
      ),
      Modulos_Utilizados = c(
        "Alertas, Dashboard Principal",
        "Análisis Comparativo, Diferencias Temporales",
        "Reportes Automáticos, Métricas Avanzadas",
        "Extracción Directa, Validación Regulatoria"
      ),
      stringsAsFactors = FALSE
    )
    return(stakeholders)
  }
  
  # Función para crear tabla de análisis de riesgos
  create_risk_analysis_table <- function() {
    risks <- data.frame(
      Riesgo = c(
        "Inconsistencias en datos CMF",
        "Cambios en estructura web CMF", 
        "Latencia en actualización TPM",
        "Calidad de datos históricos",
        "Cambios regulatorios",
        "Rendimiento del sistema"
      ),
      Probabilidad = c(
        "Alta",
        "Media",
        "Media", 
        "Alta",
        "Media",
        "Baja"
      ),
      Impacto = c(
        "Alto",
        "Muy Alto",
        "Alto",
        "Alto", 
        "Muy Alto",
        "Medio"
      ),
      Estrategia_Mitigacion = c(
        "Módulo diferencias temporales automático",
        "Múltiples métodos de extracción alternativos",
        "Cache inteligente con sistema de alertas",
        "Validación cruzada automática continua",
        "Monitoreo normativo y adaptación rápida", 
        "Optimización código y paralelización"
      ),
      stringsAsFactors = FALSE
    )
    return(risks)
  }
  
  # Función para crear tabla de módulos CMF (todas frecuencias semanales)
  create_cmf_modules_table <- function() {
    modules <- data.frame(
      Modulo = c(
        "Extracción Directa CMF",
        "Monitoreo Diferencias Temporales",
        "Validación Regulatoria",
        "Sistema de Alertas"
      ),
      Funcion_Principal = c(
        "Obtener datos financieros actualizados",
        "Detectar correcciones posteriores en datos",
        "Verificar cumplimiento límites inversión",
        "Notificar inconsistencias y errores"
      ),
      Fuente_Datos = c(
        "Portal Oficial CMF",
        "Base Histórica CMF",
        "Normativa Actualizada CMF",
        "Comparación Cruzada CMF"
      ),
      Frecuencia_Ejecucion = c(
        "Semanal",
        "Semanal",
        "Semanal",
        "Semanal"
      ),
      stringsAsFactors = FALSE
    )
    return(modules)
  }
  
  # Función para crear tabla de métodos de análisis
  create_analysis_methods_table <- function() {
    methods <- data.frame(
      Metodologia = c(
        "Análisis Fundamental",
        "Análisis Técnico",
        "Análisis Cuantitativo",
        
        "Análisis de Regímenes de Mercado"
      ),
      Descripcion = c(
        "Evaluación del valor intrínseco de activos",
        "Análisis de patrones en precios históricos",
        "Modelos matemáticos y estadísticos",
        "Identificación de estados del mercado"
      ),
      Ventajas_Principales = c(
        "Visión fundamental a largo plazo",
        "Señales precisas de entrada y salida",
        "Objetividad y escalabilidad",
        "Adaptabilidad a cambios de mercado"
      ),
      Aplicacion_Practica = c(
        "Evaluación acciones y bonos",
        "Trading de instrumentos líquidos",
        "Optimización portafolios complejos",
        "Asignación táctica dinámica"
      ),
      stringsAsFactors = FALSE
    )
    return(methods)
  }
  
  # Función para generar reporte completo en PDF
  generate_complete_report <- function(output_dir = "./") {
    
    # Verificar que existe la imagen del Gantt
    gantt_image_path <- "Gantt_fernando_Neira.PNG"
    if (!file.exists(gantt_image_path)) {
      warning("Imagen del Gantt no encontrada: ", gantt_image_path)
      gantt_image_path <- ""
    }
    
    # Verificar que existe la imagen del flujo de proceso
    flujo_image_path <- "Flujo_proceso_base.png"
    if (!file.exists(flujo_image_path)) {
      warning("Imagen del flujo de proceso no encontrada: ", flujo_image_path)
      flujo_image_path <- ""
    }
    
    # Crear datos de las tablas
    stakeholders_data <- create_stakeholders_table()
    risk_data <- create_risk_analysis_table()
    cmf_modules_data <- create_cmf_modules_table()
    analysis_methods_data <- create_analysis_methods_table()
    
    # Crear contenido del reporte
    report_content <- paste0(
      "---\n",
      "title: '", project_data$nombre, "'\n",
      "subtitle: '", project_data$subtitle, "'\n", 
      "author: '", project_data$autor, "'\n",
      "date: '", format(Sys.Date(), "%d de %B de %Y"), "'\n",
      "output:\n",
      "  pdf_document:\n",
      "    toc: true\n",
      "    toc_depth: 3\n",
      "    number_sections: true\n",
      "    fig_caption: true\n",
      "    latex_engine: xelatex\n",
      "header-includes:\n",
      "  - \\usepackage[spanish]{babel}\n",
      "  - \\usepackage{float}\n",
      "  - \\floatplacement{figure}{H}\n",
      "  - \\usepackage{longtable}\n",
      "  - \\usepackage{booktabs}\n",
      "---\n\n",
      
      "```{r setup, include=FALSE}\n",
      "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)\n",
      "library(kableExtra)\n",
      "library(dplyr)\n",
      "```\n\n",
      
      "# Resumen Ejecutivo\n\n",
      "El ", project_data$nombre, " desarrolla una **solución automatizada integral** para optimizar las decisiones de inversión de una compañía de seguros. El sistema implementa análisis comparativo continuo con la **Tasa de Política Monetaria (TPM)** como benchmark principal, incorporando extracción directa desde el portal CMF y un módulo especializado de monitoreo de diferencias temporales.\n\n",
      
      "## Objetivo Principal\n\n",
      project_data$objetivo_principal, " mediante la implementación de un sistema robusto que integra múltiples metodologías de análisis financiero y garantiza el cumplimiento regulatorio con la CMF.\n\n",
      
      "## Características Técnicas Destacadas\n\n",
      "- **Extracción Directa CMF**: Obtención automática de datos financieros actualizados\n",
      "- **Monitoreo Diferencias Temporales**: Detección de correcciones posteriores en datos\n",
      "- **Comparación Automática TPM**: Benchmark continuo con tasa de política monetaria\n", 
      "- **Análisis Multimetodológico**: Implementación de diferentes metodologías complementarias\n",
      "- **Sistema de Alertas**: Notificación automática de inconsistencias\n",
      "- **Dashboard Interactivo**: Visualización en tiempo real\n",
      "- **Cumplimiento Regulatorio**: Adherencia automática a normativas CMF\n",
      "- **Alcance**: ", project_data$alcance_instrumentos, " instrumentos financieros\n",
      "- **Precisión Objetivo**: ", project_data$precision_objetivo, "%\n\n",
      
      "\\newpage\n\n",
      
      "# Cronograma y Fases del Proyecto\n\n",
      "El proyecto se estructura en **8 fases secuenciales** que culminan con un sistema operativo completo:\n\n"
    )
    
    # Agregar imagen del flujo de proceso si existe
    if (flujo_image_path != "") {
      report_content <- paste0(report_content,
                               "## Flujo de Proceso del Sistema\n\n",
                               "![Flujo de Proceso Base](", flujo_image_path, ")\n\n"
      )
    }
    
    # Continuar con las fases
    report_content <- paste0(report_content,
                             "## Fases de Desarrollo\n\n",
                             "**PASO 1: Definición de Requerimientos**\n",
                             "- Análisis de necesidades de inversión de la compañía de seguros\n",
                             "- Configuración del entorno R para el desarrollo\n",
                             "- Investigación de fuentes de datos de la CMF\n\n",
                             
                             "**PASO 2: Extracción de Datos**\n",
                             "- Extracción directa desde el portal CMF en tiempo real\n",
                             "- Obtención de datos TPM del Banco Central\n",
                             "- Implementación del sistema de monitoreo de diferencias temporales\n\n",
                             
                             "**PASO 3: Validación y Procesamiento**\n",
                             "- Validación y limpieza automática de datos\n",
                             "- Detección de diferencias temporales en los datos CMF\n",
                             "- Control de calidad y consistencia\n\n",
                             
                             "**PASO 4: Análisis Financiero**\n",
                             "- Cálculos de rentabilidad básicos\n",
                             "- Implementación de métricas avanzadas (VaR, CVaR)\n",
                             "- Análisis de correlaciones y comparación con TPM\n\n",
                             
                             "**PASO 5: Análisis Alternativo**\n",
                             "- Implementación de múltiples metodologías de análisis\n",
                             "- Análisis fundamental, técnico y cuantitativo\n",
                             "- Simulaciones Monte Carlo para evaluación de riesgos\n\n",
                             
                             "**PASO 6: Visualización y Dashboard**\n",
                             "- Desarrollo de dashboard interactivo\n",
                             "- Sistema de alertas para inconsistencias temporales\n",
                             "- Gráficos en tiempo real y reportes automáticos\n\n",
                             
                             "**PASO 7: Automatización de Reportes**\n",
                             "- Generación automática de reportes PDF\n",
                             "- Creación de diagramas de Gantt\n",
                             "- Análisis de riesgos y reportes ejecutivos\n\n",
                             
                             "**PASO 8: Despliegue y Monitoreo**\n",
                             "- Documentación técnica completa\n",
                             "- Manual de usuario para stakeholders\n",
                             "- Sistema de monitoreo continuo y plan de despliegue\n\n",
                             
                             "El flujo culmina con un **sistema operativo completo** que optimiza las decisiones de inversión mediante comparación automática con la TPM y cumple con las regulaciones de la CMF.\n\n"
    )
    
    # Agregar imagen del Gantt si existe
    if (gantt_image_path != "") {
      report_content <- paste0(report_content,
                               "## Diagrama de Gantt del Proyecto\n\n",
                               "![Cronograma del Proyecto](", gantt_image_path, ")\n\n",
                               "\\newpage\n\n"
      )
    }
    
    # Continuar con el contenido del reporte
    report_content <- paste0(report_content,
                             "# Stakeholders e Impactos\n\n",
                             "El proyecto involucra múltiples stakeholders con impactos específicos y frecuencia de interacción **semanal** para garantizar un seguimiento efectivo:\n\n",
                             
                             "```{r stakeholders-table}\n",
                             "stakeholders_data <- data.frame(\n",
                             "  Stakeholder = c('Gerencia de Inversiones', 'Analistas Financieros', 'Comité de Inversiones', 'CMF'),\n",
                             "  Impacto = c('Decisiones estratégicas de inversión', 'Evaluación diaria de oportunidades', 'Reportes ejecutivos y seguimiento', 'Supervisión regulatoria y cumplimiento'),\n",
                             "  Frecuencia = c('Semanal', 'Semanal', 'Semanal', 'Semanal'),\n",
                             "  Modulos_Utilizados = c('Alertas, Dashboard Principal', 'Análisis Comparativo, Diferencias Temporales', 'Reportes Automáticos, Métricas Avanzadas', 'Extracción Directa, Validación Regulatoria')\n",
                             ")\n",
                             "kable(stakeholders_data, \n",
                             "      caption = 'Stakeholders, Impactos y Módulos Utilizados', \n",
                             "      booktabs = TRUE, \n",
                             "      longtable = TRUE) %>%\n",
                             "  kable_styling(latex_options = c('striped', 'repeat_header'), \n",
                             "                font_size = 9) %>%\n",
                             "  column_spec(1, width = '2cm') %>%\n",
                             "  column_spec(2, width = '4cm') %>%\n",
                             "  column_spec(3, width = '1.5cm') %>%\n",
                             "  column_spec(4, width = '4cm')\n",
                             "```\n\n",
                             
                             "\\newpage\n\n",
                             
                             "# Análisis de Riesgos y Mitigación\n\n",
                             "Se identifican los principales riesgos del proyecto con sus estrategias de mitigación correspondientes:\n\n",
                             
                             "```{r risk-analysis-table}\n",
                             "risk_data <- data.frame(\n",
                             "  Riesgo = c('Inconsistencias en datos CMF', 'Cambios en estructura web CMF', 'Latencia en actualización TPM', 'Calidad de datos históricos', 'Cambios regulatorios', 'Rendimiento del sistema'),\n",
                             "  Probabilidad = c('Alta', 'Media', 'Media', 'Alta', 'Media', 'Baja'),\n",
                             "  Impacto = c('Alto', 'Muy Alto', 'Alto', 'Alto', 'Muy Alto', 'Medio'),\n",
                             "  Estrategia_Mitigacion = c('Módulo diferencias temporales automático', 'Múltiples métodos de extracción alternativos', 'Cache inteligente con sistema de alertas', 'Validación cruzada automática continua', 'Monitoreo normativo y adaptación rápida', 'Optimización código y paralelización')\n",
                             ")\n",
                             "kable(risk_data, \n",
                             "      caption = 'Análisis de Riesgos y Estrategias de Mitigación', \n",
                             "      booktabs = TRUE, \n",
                             "      longtable = TRUE) %>%\n",
                             "  kable_styling(latex_options = c('striped', 'repeat_header'), \n",
                             "                font_size = 9) %>%\n",
                             "  column_spec(1, width = '3cm') %>%\n",
                             "  column_spec(2, width = '2cm') %>%\n",
                             "  column_spec(3, width = '2cm') %>%\n",
                             "  column_spec(4, width = '5cm')\n",
                             "```\n\n",
                             
                             "\\newpage\n\n",
                             
                             "# Módulos CMF y Procesamiento de Datos\n\n",
                             "El sistema implementa módulos especializados para la extracción y procesamiento de datos desde la CMF con frecuencia **semanal**:\n\n",
                             
                             "```{r cmf-modules-table}\n",
                             "cmf_modules_data <- data.frame(\n",
                             "  Modulo = c('Extracción Directa CMF', 'Monitoreo Diferencias Temporales', 'Validación Regulatoria', 'Sistema de Alertas'),\n",
                             "  Funcion_Principal = c('Obtener datos financieros actualizados', 'Detectar correcciones posteriores en datos', 'Verificar cumplimiento límites inversión', 'Notificar inconsistencias y errores'),\n",
                             "  Fuente_Datos = c('Portal Oficial CMF', 'Base Histórica CMF', 'Normativa Actualizada CMF', 'Comparación Cruzada CMF'),\n",
                             "  Frecuencia_Ejecucion = c('Semanal', 'Semanal', 'Semanal', 'Semanal')\n",
                             ")\n",
                             "kable(cmf_modules_data, \n",
                             "      caption = 'Módulos CMF y Frecuencia de Ejecución', \n",
                             "      booktabs = TRUE, \n",
                             "      longtable = TRUE) %>%\n",
                             "  kable_styling(latex_options = c('striped', 'repeat_header'), \n",
                             "                font_size = 9) %>%\n",
                             "  column_spec(1, width = '3cm') %>%\n",
                             "  column_spec(2, width = '4cm') %>%\n",
                             "  column_spec(3, width = '2.5cm') %>%\n",
                             "  column_spec(4, width = '2cm')\n",
                             "```\n\n",
                             
                             "\\newpage\n\n",
                             
                             "# Metodologías de Análisis Financiero\n\n",
                             "El sistema integra **múltiples metodologías complementarias** de análisis financiero para garantizar robustez en las decisiones de inversión:\n\n",
                             
                             "```{r analysis-methods-table}\n",
                             "analysis_methods_data <- data.frame(\n",
                             "  Metodologia = c('Análisis Fundamental', 'Análisis Técnico', 'Análisis Cuantitativo', 'Simulación Monte Carlo', 'Modelo Black-Litterman', 'Análisis Factorial', 'Análisis de Regímenes de Mercado'),\n",
                             "  Descripcion = c('Evaluación del valor intrínseco de activos', 'Análisis de patrones en precios históricos', 'Modelos matemáticos y estadísticos', 'Simulación de múltiples escenarios', 'Optimización bayesiana de portafolios', 'Descomposición de factores de retorno', 'Identificación de estados del mercado'),\n",
                             "  Ventajas_Principales = c('Visión fundamental a largo plazo', 'Señales precisas de entrada y salida', 'Objetividad y escalabilidad', 'Análisis robusto de riesgos', 'Incorporación de views del mercado', 'Identificación de drivers de retorno', 'Adaptabilidad a cambios de mercado'),\n",
                             "  Aplicacion_Practica = c('Evaluación acciones y bonos', 'Trading de instrumentos líquidos', 'Optimización portafolios complejos', 'Proyecciones VaR y stress testing', 'Asignación estratégica de activos', 'Atribución de performance', 'Asignación táctica dinámica')\n",
                             ")\n",
                             "kable(analysis_methods_data, \n",
                             "      caption = 'Metodologías de Análisis Financiero Implementadas', \n",
                             "      booktabs = TRUE, \n",
                             "      longtable = TRUE) %>%\n",
                             "  kable_styling(latex_options = c('striped', 'repeat_header'), \n",
                             "                font_size = 8) %>%\n",
                             "  column_spec(1, width = '2.5cm') %>%\n",
                             "  column_spec(2, width = '3cm') %>%\n",
                             "  column_spec(3, width = '3cm') %>%\n",
                             "  column_spec(4, width = '3cm')\n",
                             "```\n\n",
                             
                             "\\newpage\n\n",
                             
                             "# Conclusiones y Próximos Pasos\n\n",
                             "## Beneficios Esperados\n\n",
                             "- **Optimización de Decisiones**: Comparación automática y continua con TPM\n",
                             "- **Cumplimiento Regulatorio**: Adherencia automática a normativas CMF\n",
                             "- **Detección Temprana**: Identificación de inconsistencias temporales\n",
                             "- **Análisis Robusto**: Múltiples metodologías complementarias\n",
                             "- **Eficiencia Operativa**: Automatización de procesos manuales\n",
                             "- **Visibilidad Ejecutiva**: Dashboard y reportes automáticos\n\n",
                             
                             "## Sistema Operativo Completo\n\n",
                             "El resultado final es un **sistema operativo completo** que:\n\n",
                             "- Integra datos CMF y TPM en tiempo real\n",
                             "- Detecta diferencias temporales automáticamente\n",
                             "- Aplica múltiples metodologías de análisis\n",
                             "- Genera alertas y reportes automáticos\n",
                             "- Cumple con regulaciones CMF para seguros\n",
                             "- Optimiza continuamente las decisiones de inversión\n\n",
                             
                             "## Impacto Organizacional\n\n",
                             "La implementación del sistema permitirá a la compañía de seguros tomar decisiones de inversión más informadas, reducir riesgos regulatorios y mejorar la rentabilidad de sus productos, beneficiando directamente a todos los stakeholders identificados.\n\n"
    )
    
    # Escribir archivo RMarkdown
    rmd_file <- paste0(output_dir, "proyecto_analisis_inversiones.Rmd")
    writeLines(report_content, rmd_file)
    
    # Renderizar PDF
    pdf_file <- paste0(output_dir, "Proyecto_Analisis_Inversiones.pdf")
    
    tryCatch({
      rmarkdown::render(rmd_file, output_file = pdf_file)
      
      # Información de salida
      cat("=== Proyecto Análisis de Inversiones ===\n")
      cat("✓ Reporte PDF generado exitosamente\n")
      cat("✓ Archivo PDF:", pdf_file, "\n")
      cat("✓ Archivo RMarkdown:", rmd_file, "\n")
      if (gantt_image_path != "") {
        cat("✓ Imagen Gantt incluida:", gantt_image_path, "\n")
      }
      if (flujo_image_path != "") {
        cat("✓ Imagen Flujo de Proceso incluida:", flujo_image_path, "\n")
      }
      cat("✓ Todas las frecuencias configuradas como: SEMANAL\n")
      
    }, error = function(e) {
      cat("Error al generar PDF:", e$message, "\n")
      cat("Archivo RMarkdown creado:", rmd_file, "\n")
    })
    
    return(list(
      pdf_report = pdf_file,
      rmd_file = rmd_file,
      gantt_image = gantt_image_path,
      flujo_image = flujo_image_path,
      project_data = project_data
    ))
  }
  
  # Retornar funciones públicas
  return(list(
    create_stakeholders_table = create_stakeholders_table,
    create_risk_analysis_table = create_risk_analysis_table,
    create_cmf_modules_table = create_cmf_modules_table,
    create_analysis_methods_table = create_analysis_methods_table,
    generate_complete_report = generate_complete_report,
    project_data = project_data
  ))
}

# Uso del generador
generator <- ProyectoAnalisisInversionesReportGenerator()
generator$generate_complete_report()

renv::init()
renv::snapshot()
