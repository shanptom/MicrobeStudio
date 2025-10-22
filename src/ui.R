library(shiny.semantic)
library(shiny)
library(shinyjs)
library(shinycssloaders)

# Explicitly use shiny's functions (shiny.semantic masks them)
selectInput <- shiny::selectInput
checkboxInput <- shiny::checkboxInput
radioButtons <- shiny::radioButtons
sliderInput <- shiny::sliderInput
actionButton <- shiny::actionButton
textInput <- shiny::textInput
numericInput <- shiny::numericInput
downloadButton <- shiny::downloadButton
# Note: fileInput is now custom Semantic UI implementation

ui <- semanticPage(
  title = "MicrobeStudio - Interactive Analysis for Microbial Community Data",
  suppress_bootstrap = TRUE,
  theme = NULL,  # Using custom CSS instead

  useShinyjs(),

  # Custom head content
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$link(rel = "stylesheet", href = "custom_semantic.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    ),
    tags$style(HTML('
      /* Tighten sidebar form spacing and accordion content padding */
      .sidebar-segment .ui.form .field { margin-bottom: 0.5rem !important; }
      #ordination .ui.styled.accordion .content { padding: 0.5rem 1rem !important; }
      #ordination .ui.styled.accordion .title { padding: 0.6rem 1rem !important; }
    ')),

    # Tab management JavaScript
    tags$script(HTML("
      // File upload handler for custom Semantic UI file inputs
      function handleFileSelect(input, inputId) {
        var file = input.files[0];
        var displayInput = $('#' + inputId + '-display');
        var infoDiv = $('#' + inputId + '-info');

        if (file) {
          // Update display input with filename
          displayInput.val(file.name);
          displayInput.parent().addClass('file-selected');

          // Show file information
          var fileSize = formatFileSize(file.size);
          var fileType = file.type || 'Unknown';

          infoDiv.html(
            '<div class=\"ui mini message\">' +
            '<i class=\"file icon\"></i> ' +
            '<strong>' + file.name + '</strong><br>' +
            '<small>Size: ' + fileSize + ' | Type: ' + fileType + '</small>' +
            '</div>'
          );

          // Trigger Shiny file input binding
          $(input).trigger('change');
        } else {
          // Clear selection
          displayInput.val('');
          displayInput.parent().removeClass('file-selected');
          infoDiv.html('');
        }
      }

      // Format file size for display
      function formatFileSize(bytes) {
        if (bytes === 0) return '0 Bytes';
        var k = 1024;
        var sizes = ['Bytes', 'KB', 'MB', 'GB'];
        var i = Math.floor(Math.log(bytes) / Math.log(k));
        return Math.round(bytes / Math.pow(k, i) * 100) / 100 + ' ' + sizes[i];
      }

      // Tab management system
      var TabManager = {
        currentTab: 'home',
        disabledTabs: ['filter', 'rarefaction', 'abundance', 'alpha', 'dendrogram', 'ordination', 'permanova', 'metadata', 'regression', 'indicator'],

        init: function() {
          var self = this;

          // Handle tab clicks
          $('.ui.menu .item:not(.logo)').on('click', function(e) {
            e.preventDefault();
            var targetTab = $(this).attr('data-tab');

            // Check if tab is disabled
            if (self.disabledTabs.includes(targetTab)) {
              console.log('Tab disabled:', targetTab);
              return false;
            }

            self.showTab(targetTab);
          });

          // Show initial tab
          self.showTab(self.currentTab);

          // Initialize accordions with consistent behavior
          $('.ui.accordion').accordion({
            exclusive: false,
            collapsible: true,
            animateChildren: false,
            duration: 150,
            active: false
          });
          $('#ordination_accordion').accordion({
            exclusive: false,
            collapsible: true,
            animateChildren: false,
            duration: 150,
            active: false
          });
        },

        showTab: function(tabId) {
          // Hide all tabs using CSS class (keeps them rendered)
          $('.main-container').addClass('hidden-tab');

          // Show target tab
          $('#' + tabId).removeClass('hidden-tab');

          // Update active menu item
          $('.ui.menu .item').removeClass('active');
          $('.ui.menu .item[data-tab=\"' + tabId + '\"]').addClass('active');

          this.currentTab = tabId;

          // For the User Guide, always scroll to top when opening
          if (tabId === 'manual') {
            try {
              $('html, body').scrollTop(0);
              $('#manual').scrollTop(0);
            } catch(e) {}
          }

          // Notify Shiny of tab change
          if (typeof Shiny !== 'undefined') {
            Shiny.setInputValue('current_tab', tabId, {priority: 'event'});

            // CRITICAL: Trigger Shiny's output binding system
            // This tells Shiny to re-evaluate which outputs are visible
            // and trigger/suspend rendering based on suspendWhenHidden
            setTimeout(function() {
              $(window).trigger('resize');
              Shiny.unbindAll();
              Shiny.bindAll();
              if (tabId === 'manual') {
                try {
                  $('html, body').scrollTop(0);
                  $('#manual').scrollTop(0);
                } catch(e) {}
              }
              // Re-initialize accordions when switching tabs to ensure bindings
              var $allAcc = $('.ui.accordion');
              if ($allAcc.length) {
                try { $allAcc.accordion('refresh'); } catch(e) {}
                try { $allAcc.accordion({ exclusive: false, collapsible: true, active: false }); } catch(e) {}
              }
              if (tabId === 'ordination') {
                var $acc = $('#ordination_accordion');
                // Ensure no panel is forced open
                $acc.find('.title, .content').removeClass('active');
                setTimeout(function(){
                  try {
                    $acc.accordion('close', 0);
                    $acc.accordion('close', 1);
                    $acc.accordion('close', 2);
                  } catch(e) {}
                }, 20);
              }
            }, 50);
          }
        },

        enableTab: function(tabId) {
          var index = this.disabledTabs.indexOf(tabId);
          if (index > -1) {
            this.disabledTabs.splice(index, 1);
            $('.ui.menu .item[data-tab=\"' + tabId + '\"]').removeClass('disabled');

            // Enable corresponding module card on home page
            var $moduleCard = $('.module-card[data-tab=\"' + tabId + '\"]');
            if ($moduleCard.length) {
              $moduleCard.removeClass('module-card-disabled');
              $moduleCard.attr('onclick', 'TabManager.showTab(\"' + tabId + '\")');
              $moduleCard.css('pointer-events', 'auto');
            }
          }
        },

        disableTab: function(tabId) {
          if (!this.disabledTabs.includes(tabId)) {
            this.disabledTabs.push(tabId);
            $('.ui.menu .item[data-tab=\"' + tabId + '\"]').addClass('disabled');

            // Disable corresponding module card on home page
            var $moduleCard = $('.module-card[data-tab=\"' + tabId + '\"]');
            if ($moduleCard.length) {
              $moduleCard.addClass('module-card-disabled');
              $moduleCard.removeAttr('onclick');
              $moduleCard.css('pointer-events', 'none');
            }
          }
        }
      };

      $(document).on(\"shiny:connected\", function() {
        TabManager.init();

        // Make TabManager available to Shiny
        Shiny.addCustomMessageHandler('enableTab', function(tabId) {
          TabManager.enableTab(tabId);
        });

        Shiny.addCustomMessageHandler('disableTab', function(tabId) {
          TabManager.disableTab(tabId);
        });

        Shiny.addCustomMessageHandler('showTab', function(tabId) {
          TabManager.showTab(tabId);
        });

        // Tooltip initialization
        $('[data-tooltip]').popup();
      });
    "))
  ),

  # Top navigation menu
  div(class = "ui top fixed menu",
    a(class = "item logo", href = "#", style = "pointer-events: none;",
      tags$img(src = "logo.png", class = "brand-logo", alt = "MicrobeStudio Logo"),
      "MicrobeStudio"
    ),
    a(class = "item", `data-tab` = "home", "Home"),
    a(class = "item", `data-tab` = "upload", "Upload Data"),
    a(class = "item disabled", `data-tab` = "filter", "Filter"),
    a(class = "item disabled", `data-tab` = "rarefaction", "Rarefaction"),
    a(class = "item disabled", `data-tab` = "abundance", "Abundance"),
    a(class = "item disabled", `data-tab` = "alpha", "Alpha Diversity"),
    a(class = "item disabled", `data-tab` = "dendrogram", "Dendrogram"),
    a(class = "item disabled", `data-tab` = "ordination", "Ordination"),
    a(class = "item disabled", `data-tab` = "permanova", "PERMANOVA"),
    a(class = "item disabled", `data-tab` = "metadata", "Metadata"),
    a(class = "item disabled", `data-tab` = "regression", "Regression"),
    a(class = "item disabled", `data-tab` = "indicator", "Indicator Species")
  ),

  # Main content area (below fixed menu)
  div(style = "margin-top: 60px;",

        # ============================================================
        # HOME TAB (REDESIGNED)
        # ============================================================
        div(id = "home", class = "ui container main-container",
          # --- Hero Section ---
          div(class = "hero-section",
            div(class = "hero-content",
              h1(class = "hero-title", "Welcome to MicrobeStudio"),
              p(class = "hero-subtitle",
                "Interactive Microbial Community Analysis Platform"
              ),
              p(class = "hero-description",
                "A comprehensive toolkit for exploring microbial community data through diversity analysis, ordination, and visualization - no coding required. From raw data to publication-ready insights."
              ),
              div(class = "hero-actions",
                actionButton("cta_upload", "Upload Your Data",
                             class = "ui huge primary button cta-primary",
                             onclick = "TabManager.showTab('upload')"),
                actionButton("cta_demo", "Try Demo Data",
                             class = "ui huge button cta-secondary",
                             onclick = "Shiny.setInputValue('load_demo_from_home', true, {priority: 'event'}); TabManager.showTab('upload');")
              )
            )
          ),

          # --- Research Questions Section ---
          div(class = "ui segment questions-segment",
            h2(class = "section-title center-text",
               tags$i(class = "fas fa-question-circle"),
               " What Questions Can You Answer?"),
            div(class = "questions-grid",
              div(class = "question-card",
                tags$i(class = "fas fa-chart-line question-icon"),
                p(class = "question-text", "How diverse are my microbial communities?")
              ),
              div(class = "question-card",
                tags$i(class = "fas fa-project-diagram question-icon"),
                p(class = "question-text", "Which samples have similar community composition?")
              ),
              div(class = "question-card",
                tags$i(class = "fas fa-microscope question-icon"),
                p(class = "question-text", "Which taxa differ between treatment groups?")
              ),
              div(class = "question-card",
                tags$i(class = "fas fa-link question-icon"),
                p(class = "question-text", "How do environmental factors affect community structure?")
              )
            )
          ),

          # --- Quick Start Guide ---
          div(class = "ui segment quickstart-segment",
            h2(class = "section-title center-text",
               tags$i(class = "fas fa-rocket"),
               " Quick Start Guide"),
            div(class = "workflow-steps",
              div(class = "workflow-step",
                div(class = "step-number", "1"),
                div(class = "step-content",
                  h4("Upload Your Data"),
                  p("Load phyloseq .rds files or CSV files (ASV table, taxonomy, metadata)")
                )
              ),
              div(class = "workflow-arrow", tags$i(class = "fas fa-arrow-right")),
              div(class = "workflow-step",
                div(class = "step-number", "2"),
                div(class = "step-content",
                  h4("Filter & Normalize"),
                  p("Optional: Remove unwanted taxa, apply rarefaction or TSS normalization")
                )
              ),
              div(class = "workflow-arrow", tags$i(class = "fas fa-arrow-right")),
              div(class = "workflow-step",
                div(class = "step-number", "3"),
                div(class = "step-content",
                  h4("Explore & Analyze"),
                  p("Navigate through analysis modules to visualize and interpret your data")
                )
              )
            )
          ),

          # --- Analysis Modules Section ---
          div(class = "modules-section",
            h2(class = "section-title center-text",
               tags$i(class = "fas fa-cogs"),
               " Analysis Workflow"),

            # Data Preparation
            div(class = "module-category",
              h3(class = "category-title",
                 tags$i(class = "fas fa-database"),
                 " Data Preparation"),
              div(class = "modules-grid",
                div(class = "module-card",
                     onclick = "TabManager.showTab('upload')",
                  div(class = "module-icon-wrapper upload-icon",
                    tags$i(class = "fas fa-cloud-upload-alt fa-2x")
                  ),
                  h4(class = "module-title", "Upload Data"),
                  p(class = "module-description",
                    "Load phyloseq objects or CSV files (ASV, taxonomy, metadata)")
                ),
                div(class = "module-card module-card-disabled", `data-tab` = "filter",
                  div(class = "module-icon-wrapper filter-icon",
                    tags$i(class = "fas fa-filter fa-2x")
                  ),
                  h4(class = "module-title", "Filter"),
                  p(class = "module-description",
                    "Remove unwanted taxa and apply normalization (TSS/rarefaction)")
                )
              )
            ),

            # Diversity Analysis
            div(class = "module-category",
              h3(class = "category-title",
                 tags$i(class = "fas fa-dna"),
                 " Diversity Analysis"),
              div(class = "modules-grid modules-grid-3",
                div(class = "module-card module-card-disabled", `data-tab` = "rarefaction",
                  div(class = "module-icon-wrapper rarefaction-icon",
                    tags$i(class = "fas fa-chart-area fa-2x")
                  ),
                  h4(class = "module-title", "Rarefaction"),
                  p(class = "module-description",
                    "Assess sampling depth and sequencing adequacy")
                ),
                div(class = "module-card module-card-disabled", `data-tab` = "alpha",
                  div(class = "module-icon-wrapper alpha-icon",
                    tags$i(class = "fas fa-calculator fa-2x")
                  ),
                  h4(class = "module-title", "Alpha Diversity"),
                  p(class = "module-description",
                    "Within-sample diversity (Shannon, Simpson, Chao1)")
                ),
                div(class = "module-card module-card-disabled", `data-tab` = "ordination",
                  div(class = "module-icon-wrapper ordination-icon",
                    tags$i(class = "fas fa-project-diagram fa-2x")
                  ),
                  h4(class = "module-title", "Ordination"),
                  p(class = "module-description",
                    "Beta diversity visualization (NMDS, PCoA, t-SNE)")
                )
              )
            ),

            # Community Composition
            div(class = "module-category",
              h3(class = "category-title",
                 tags$i(class = "fas fa-chart-bar"),
                 " Community Composition"),
              div(class = "modules-grid",
                div(class = "module-card module-card-disabled", `data-tab` = "abundance",
                  div(class = "module-icon-wrapper abundance-icon",
                    tags$i(class = "fas fa-chart-bar fa-2x")
                  ),
                  h4(class = "module-title", "Abundance"),
                  p(class = "module-description",
                    "Taxonomic composition bar plots and heatmaps")
                ),
                div(class = "module-card module-card-disabled", `data-tab` = "dendrogram",
                  div(class = "module-icon-wrapper dendrogram-icon",
                    tags$i(class = "fas fa-sitemap fa-2x")
                  ),
                  h4(class = "module-title", "Dendrogram"),
                  p(class = "module-description",
                    "Hierarchical clustering of sample similarity")
                )
              )
            ),

            # Advanced Analysis
            div(class = "module-category",
              h3(class = "category-title",
                 tags$i(class = "fas fa-microscope"),
                 " Advanced Analysis"),
              div(class = "modules-grid modules-grid-3",
                div(class = "module-card module-card-disabled", `data-tab` = "metadata",
                  div(class = "module-icon-wrapper metadata-icon",
                    tags$i(class = "fas fa-table fa-2x")
                  ),
                  h4(class = "module-title", "Metadata Analysis"),
                  p(class = "module-description",
                    "Environmental correlations and constrained ordination")
                ),
                div(class = "module-card module-card-disabled", `data-tab` = "regression",
                  div(class = "module-icon-wrapper regression-icon",
                    tags$i(class = "fas fa-line-chart fa-2x")
                  ),
                  h4(class = "module-title", "Regression"),
                  p(class = "module-description",
                    "Relate individual taxa to environmental variables")
                ),
                div(class = "module-card module-card-disabled", `data-tab` = "indicator",
                  div(class = "module-icon-wrapper indicator-icon",
                    tags$i(class = "fas fa-star fa-2x")
                  ),
                  h4(class = "module-title", "Indicator Species"),
                  p(class = "module-description",
                    "Identify characteristic taxa for each group")
                )
              )
            )
          ),

          # --- Footer Section ---
          div(class = "ui segment footer-segment",
            div(class = "footer-grid",
              div(class = "footer-column",
                h4(class = "footer-title", tags$i(class = "fas fa-code"), " Built With"),
                p(class = "footer-text",
                  tags$strong("phyloseq"), ", ",
                  tags$strong("microeco"), ", ",
                  tags$strong("phylosmith"), ", ",
                  tags$strong("vegan"), ", ",
                  tags$strong("ggplot2")
                )
              ),
              div(class = "footer-column",
                h4(class = "footer-title", tags$i(class = "fas fa-book"), " Resources"),
                p(class = "footer-text",
                  tags$a(href = "#", onclick = "TabManager.showTab('manual'); return false;", "User Guide"),
                  " | ",
                  tags$a(href = "https://github.com/shanptom/MicrobeStudio", target = "_blank", "GitHub")
                )
              ),
              div(class = "footer-column",
                h4(class = "footer-title", tags$i(class = "fas fa-user"), " Contact"),
                p(class = "footer-text",
                  "Developed by ",
                  tags$a(href = "https://shanptom.github.io", target = "_blank", "Shan Thomas")
                )
              )
            )
          )
        ),
    # ============================================================
    # USER MANUAL TAB
    # ============================================================
    div(id = "manual", class = "ui container main-container hidden-tab",
      div(class = "ui segment",
        includeMarkdown("docs/user_guide.md"),
        tags$hr(),
        includeMarkdown("docs/Phyloseq.md")
      )
    ),

    # ============================================================
    # UPLOAD DATA TAB
    # ============================================================
    div(id = "upload", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Data Input"),
            div(class = "ui form",
              # Custom Semantic UI File Upload - Count Table
              div(class = "field",
                div(class = "ui fluid action input file-upload-wrapper",
                  tags$input(type = "file", id = "asv", name = "asv", accept = ".csv",
                             style = "display: none;",
                             onchange = "handleFileSelect(this, 'asv')"),
                  tags$input(type = "text", id = "asv-display", readonly = "",
                             placeholder = "Upload count table", class = "file-display-input"),
                  tags$label(`for` = "asv", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "asv-info", class = "file-info-display")
              ),

              # Custom Semantic UI File Upload - Taxonomy Table
              div(class = "field",
                div(class = "ui fluid action input file-upload-wrapper",
                  tags$input(type = "file", id = "tax", name = "tax", accept = ".csv",
                             style = "display: none;",
                             onchange = "handleFileSelect(this, 'tax')"),
                  tags$input(type = "text", id = "tax-display", readonly = "",
                             placeholder = "Upload taxonomy table", class = "file-display-input"),
                  tags$label(`for` = "tax", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "tax-info", class = "file-info-display")
              ),

              # Custom Semantic UI File Upload - Metadata Table
              div(class = "field",
                div(class = "ui fluid action input file-upload-wrapper",
                  tags$input(type = "file", id = "meta", name = "meta", accept = ".csv",
                             style = "display: none;",
                             onchange = "handleFileSelect(this, 'meta')"),
                  tags$input(type = "text", id = "meta-display", readonly = "",
                             placeholder = "Upload metadata table", class = "file-display-input"),
                  tags$label(`for` = "meta", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "meta-info", class = "file-info-display")
              ),

              div(class = "ui horizontal divider", "OR"),

              # Custom Semantic UI File Upload - Phyloseq Object
              div(class = "field",
                div(class = "ui fluid action input file-upload-wrapper",
                  tags$input(type = "file", id = "phylo", name = "phylo", accept = ".rds",
                             style = "display: none;",
                             onchange = "handleFileSelect(this, 'phylo')"),
                  tags$input(type = "text", id = "phylo-display", readonly = "",
                             placeholder = "Upload phyloseq object", class = "file-display-input"),
                  tags$label(`for` = "phylo", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "phylo-info", class = "file-info-display")
              ),

              div(class = "ui divider"),

              # Action button to process uploaded files + inline loader
              div(style = "display: flex; align-items: center; gap: 0.75rem;",
                actionButton("process_files", "Upload Files", class = "ui primary button"),
                div(id = "upload_spinner", class = "ui active inline loader", style = "display: none;")
              )
            ),

            div(class = "ui divider"),

            h4("Load Demo Data"),
            div(class = "ui form",
              div(class = "field",
                selectInput("demo_file", "Select Demo Dataset:",
                            choices = c("Phyloseq RDS (demo_ps.rds)" = "rds",
                                        "CSV Set (demo_asv.csv, demo_meta.csv, demo_tax.csv)" = "csv"))
              ),
              actionButton("load_demo", "Load Demo Data", class = "ui button fluid cta-secondary")
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment",
            uiOutput("upload_status_ui")
          )
        )
      )
    ),

    # ============================================================
    # FILTER TAB
    # ============================================================
    div(id = "filter", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Data Filtering"),

            # Accordion for organized sections
            div(id = "ordination_accordion", class = "ui styled accordion",
              # Normalization Options
              div(class = "active title",
                tags$i(class = "dropdown icon"),
                "Normalization Options"
              ),
              div(class = "active content",
                div(class = "ui form",
                  div(class = "field",
                    checkboxInput("doRarefy", "Apply rarefaction", value = FALSE)
                  ),
                  div(class = "field",
                    checkboxInput("doTSS", "Normalize by TSS", value = FALSE)
                  )
                )
              ),

              # Taxa Filters
              div(class = "title",
                tags$i(class = "dropdown icon"),
                "Taxa Filters"
              ),
              div(class = "content",
                div(class = "ui form",
                  uiOutput("taxa_filters")
                )
              )
            ),

            # Action buttons always visible at bottom
            div(class = "filter-actions",
              actionButton("apply_filter", "Apply Filtering", class = "ui primary button fluid"),
              actionButton("go_analysis", "Go to Analysis", class = "ui positive button fluid")
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment",
            h3("Filter Status"),
            verbatimTextOutput("filter_status")
          )
        )
      )
    ),

    # ============================================================
    # RAREFACTION TAB
    # ============================================================
    div(id = "rarefaction", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Plot Controls"),
            div(class = "ui form",
              uiOutput("rarefaction_color_selector"),
              uiOutput("rarefaction_facet_selector"),
              div(class = "field",
                label("Axis Text Size"),
                sliderInput("rarefaction_beta_label_size", NULL, min = 6, max = 20, value = 12)
              ),
              div(class = "field",
                label("Sample Label Size"),
                sliderInput("rarefaction_label_size", NULL, min = 2, max = 10, value = 4)
              ),
              div(class = "field",
                checkboxInput("show_rarefaction_labels", "Show Sample Labels", value = TRUE)
              ),
              uiOutput("rarefaction_facet_order_selector")
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            withSpinner(plotOutput("rarefactionPlot", width = "100%", height = "770px"))
          )
        )
      )
    ),

    # ============================================================
    # ABUNDANCE TAB
    # ============================================================
    div(id = "abundance", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Plot Settings"),
            div(class = "ui form",
              div(class = "field",
                label("Plot Type"),
                selectInput("abund_plot_type", NULL,
                            choices = c("Bar" = "bar", "Line" = "line", "Heatmap" = "heat"),
                            selected = "bar")
              ),
              uiOutput("tax_rank_selector"),
              div(class = "field",
                label("Number of Top Taxa"),
                sliderInput("ntaxa", NULL, min = 5, max = 15, value = 8, step = 1)
              ),
              uiOutput("abundance_facet_selector"),
              uiOutput("abundance_order_selector"),
              div(class = "field",
                label("Axis Text Size"),
                sliderInput("abundance_beta_label_size", NULL, min = 6, max = 20, value = 12)
              ),
              div(class = "field",
                checkboxInput("flip_abundance", "Flip axes (horizontal plot)", value = FALSE)
              )
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            withSpinner(uiOutput("abundance_plot_output"))
          )
        )
      )
    ),

    # ============================================================
    # ALPHA DIVERSITY TAB
    # ============================================================
    div(id = "alpha", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Diversity Indices"),
            div(class = "ui form",
              div(class = "field",
                label("Select Diversity Index"),
                checkboxGroupInput("alpha_index", NULL,
                                   choices = c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher"),
                                   selected = c("Shannon"))
              ),
              uiOutput("alpha_group_selector"),
              uiOutput("alpha_colour_selector"),
              div(class = "field",
                checkboxInput("flip_alpha", "Flip axes (horizontal plot)", value = FALSE)
              ),
              uiOutput("alpha_order_selector"),
              div(class = "field",
                label("Text Label Size"),
                sliderInput("alpha_beta_label_size", NULL, min = 6, max = 20, value = 12)
              )
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            uiOutput("alpha_plot_output")
          )
        )
      )
    ),

    # ============================================================
    # DENDROGRAM TAB
    # ============================================================
    div(id = "dendrogram", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Dendrogram Settings"),
            div(class = "ui form",
              uiOutput("dend_treatment_selector"),
              div(class = "field",
                label("Distance Method"),
                selectInput("dend_method", NULL,
                            choices = c("euclidian", "manhattan", "canberra", "clark", "bray",
                                        "kulczynski", "jaccard", "gower", "altGower", "morisita",
                                        "horn", "mountford", "raup", "binomial", "chao", "cao", "mahalanobis"),
                            selected = "bray")
              ),
              div(class = "field",
                label("Label Size"),
                sliderInput("dend_label_size", NULL, min = 3, max = 10, value = 5, step = 1)
              ),
              div(class = "field",
                label("Text Size"),
                sliderInput("dend_text_size", NULL, min = 6, max = 20, value = 12)
              )
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            withSpinner(plotOutput("dendrogramPlot", width = "100%", height = "770px"))
          )
        )
      )
    ),

    # ============================================================
    # ORDINATION TAB
    # ============================================================
    div(id = "ordination", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Settings"),

            # Ordination Settings Accordion
            div(class = "ui styled accordion",
              div(class = "title",
                tags$i(class = "dropdown icon"),
                "Ordination Settings"
              ),
              div(class = "content",
                div(class = "ui form",
                  div(class = "field",
                    label("Distance Method"),
                    selectInput("beta_dist", NULL,
                                choices = c("bray", "unifrac", "wunifrac", "jaccard", "dpcoa", "jsd",
                                            "manhattan", "euclidean", "canberra", "binomial"),
                                selected = "bray")
                  ),
                  div(class = "field",
                    label("Ordination Method"),
                    selectInput("beta_ord", NULL,
                                choices = c("NMDS", "MDS", "PCoA", "DCA", "CCA", "RDA", "DPCoA"),
                                selected = "NMDS")
                  ),
                  div(class = "field",
                    label("Random Seed (for stochastic methods)"),
                    numericInput("ord_seed", NULL, value = 123, min = 1, step = 1)
                  )
                )
              )
            ),

            # Aesthetics Accordion
            div(class = "ui styled accordion",
              div(class = "title",
                tags$i(class = "dropdown icon"),
                "Aesthetics"
              ),
              div(class = "content",
                div(class = "ui form",
                  uiOutput("beta_color_selector"),
                  uiOutput("beta_shape_selector"),
                  uiOutput("beta_label_selector"),
                  uiOutput("beta_facet_selector"),
                  uiOutput("beta_facet_order_selector"),
                  div(class = "field",
                    label("Axis Text Size"),
                    sliderInput("beta_label_size", NULL, min = 6, max = 20, value = 12)
                  ),
                  div(class = "field",
                    label("Label Text Size"),
                    sliderInput("beta_label_text_size", NULL, min = 2, max = 15, value = 3)
                  ),
                  div(class = "field",
                    label("Shape Size"),
                    sliderInput("beta_shape_size", NULL, min = 1, max = 10, value = 4)
                  ),
                  div(class = "field",
                    label("Point Transparency (alpha)"),
                    sliderInput("beta_alpha", NULL, min = 0.1, max = 1.0, value = 0.9, step = 0.05)
                  ),
                  div(class = "field",
                    checkboxInput("beta_jitter", "Jitter points", value = FALSE)
                  ),
                  div(class = "field",
                    checkboxInput("beta_label_repel", "Repel labels (reduce overlap)", value = TRUE)
                  ),
                  div(style = "margin-top: 6px;"),
                  actionButton("reset_ordination_aes", "Reset Aesthetics", class = "ui button")
                )
              )
            ),

            # t-SNE Analysis Accordion
            div(class = "ui styled accordion",
              div(class = "title",
                tags$i(class = "dropdown icon"),
                "t-SNE Analysis"
              ),
              div(class = "content",
                div(class = "ui form",
                  uiOutput("tsne_group_selector"),
                  uiOutput("tsne_perplexity_selector"),
                  div(class = "field",
                    checkboxInput("tsne_circle", "Draw circles", value = FALSE)
                  ),
                  uiOutput("tsne_label_selector"),
                  div(class = "field",
                    label("Random Seed"),
                    numericInput("tsne_seed", NULL, value = 123, min = 1, step = 1)
                  ),
                  actionButton("run_tsne", "Run tSNE", class = "ui primary button fluid"),
                  conditionalPanel(
                    condition = "output.show_tsne_flag",
                    actionButton("reset_tsne", "Back to Ordination", class = "ui button fluid")
                  )
                )
              )
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            div(class = "ui tiny info message",
              strong("Ordination Info: "),
              textOutput("ordination_info", inline = TRUE)
            ),
            div(class = "ui tiny message",
              strong("Analysis Summary: "),
              textOutput("analysis_summary", inline = TRUE)
            ),
            conditionalPanel(
              condition = "!output.show_tsne_flag",
              withSpinner(plotOutput("betaPlot", width = "100%", height = "770px"))
            ),
            conditionalPanel(
              condition = "output.show_tsne_flag",
              withSpinner(plotOutput("tsne_plot", width = "100%", height = "770px"))
            )
          )
        )
      )
    ),

    # ============================================================
    # PERMANOVA TAB
    # ============================================================
    div(id = "permanova", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("PERMANOVA"),
            div(class = "ui form",
              div(class = "field",
                label("Distance Method"),
                selectInput("permanova_dist", NULL,
                            choices = c("bray", "unifrac", "wunifrac", "jaccard", "dpcoa", "jsd",
                                        "manhattan", "euclidean", "canberra", "binomial"),
                            selected = "bray")
              ),
              uiOutput("permanova_group_selector"),
              uiOutput("permanova_strata_selector"),
              div(class = "two fields",
                div(class = "field",
                  label("Permutations"),
                  numericInput("permanova_permutations", NULL, value = 999, min = 99, step = 100)
                ),
                div(class = "field",
                  label("P-value Adjustment"),
                  selectInput("p_adjust_method", NULL, choices = c("BH", "Holm", "Bonferroni", "BY"), selected = "BH")
                )
              ),
              div(class = "field",
                checkboxInput("permdisp_enable", "Check homogeneity of dispersion (PERMDISP)", value = FALSE)
              ),
              div(class = "two fields",
                div(class = "field",
                  actionButton("run_permanova", "Run PERMANOVA", class = "ui primary button fluid")
                ),
                div(class = "field",
                  actionButton("reset_permanova", "Reset", class = "ui button fluid")
                )
              )
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            div(class = "ui info message",
              strong("About PERMANOVA: "),
              HTML("Tests for differences in community composition between groups using permutations of a distance matrix. Consider checking homogeneity of dispersion.")
            ),
            div(class = "ui tiny message",
              strong("PERMDISP: "),
              textOutput("permdisp_info", inline = TRUE)
            ),
            DT::dataTableOutput("permanova_table"),
            div(style = "margin-top: 8px;"),
            downloadButton("download_permanova", "Download CSV", class = "ui button")
          )
        )
        )
      )
    ),

    # ============================================================
    # METADATA ANALYSIS TAB
    # ============================================================
    div(id = "metadata", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Setup"),
            div(class = "ui form",
              uiOutput("numeric_column_selector_ui"),
              actionButton("create_transenv", "Create trans_env Object", class = "ui primary button fluid"),
              br(), br(),
              verbatimTextOutput("transenv_display"),
              uiOutput("continue_button_ui"),
              uiOutput("visualization_sidebar")
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            conditionalPanel(
              condition = "output.analysis_mode == 'rda'",
              withSpinner(plotOutput("rda_plot", width = "100%", height = "770px"))
            ),
            conditionalPanel(
              condition = "output.analysis_mode == 'corr'",
              withSpinner(plotOutput("corr_plot", width = "100%", height = "770px"))
            ),
            conditionalPanel(
              condition = "output.analysis_mode == 'mantel'",
              withSpinner(plotOutput("mantel_plot", width = "100%", height = "770px"))
            )
          )
        )
      )
    ),

    # ============================================================
    # REGRESSION TAB
    # ============================================================
    div(id = "regression", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("Regression Setup"),
            div(class = "ui form",
              uiOutput("tax_rank_selector_regression"),
              uiOutput("taxa_selector_regression"),
              uiOutput("env_var_selector"),
              uiOutput("regression_group_selector"),
              actionButton("run_scatter", "Run Scatter Plot", class = "ui primary button fluid"),
              div(class = "field",
                label("Point Size"),
                sliderInput("point_size", NULL, min = 1, max = 6, value = 3)
              ),
              div(class = "field",
                label("Text Size"),
                sliderInput("text_size", NULL, min = 6, max = 20, value = 12)
              )
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            withSpinner(plotOutput("regression_plot", width = "100%", height = "770px"))
          )
        )
      )
    ),

    # ============================================================
    # INDICATOR SPECIES TAB
    # ============================================================
    div(id = "indicator", class = "ui container main-container hidden-tab",
      div(class = "ui grid",
        div(class = "four wide column",
          div(class = "ui segment sidebar-segment",
            h3("SHAP Analysis Setup"),
            div(class = "ui form",
              uiOutput("indicator_variable_selector"),
              uiOutput("indicator_group1_selector"),
              uiOutput("indicator_group2_selector"),
              div(class = "field",
                label("Number of Top Taxa to Display"),
                sliderInput("top_n_taxa", NULL, min = 5, max = 30, value = 10, step = 1)
              ),
              div(class = "field",
                label("Font Size"),
                sliderInput("indicator_font_size", NULL, min = 6, max = 20, value = 10, step = 1)
              ),
              actionButton("run_indicator_analysis", "Run Analysis", class = "ui primary button fluid")
            )
          )
        ),
        div(class = "twelve wide column",
          div(class = "ui segment plot-container",
            withSpinner(plotOutput("indicator_plot", width = "100%", height = "770px")),
            h3("Results Table"),
            DT::DTOutput("indicator_table")
          )
        )
      )
    )

  
)  # End of semanticPage
