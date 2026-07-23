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
  theme = NULL, # Using custom CSS instead

  useShinyjs(),
  use_telemetry(),

  # Custom head content
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$link(rel = "stylesheet", href = "custom_semantic.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600;700&family=DM+Sans:wght@400;500;600;700&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    ),
    tags$style(HTML("
      /* Tighten sidebar form spacing and accordion content padding */
      .sidebar-segment .ui.form .field { margin-bottom: 0.5rem !important; }
      #ordination .ui.styled.accordion .content { padding: 0.5rem 1rem !important; }
      #ordination .ui.styled.accordion .title { padding: 0.6rem 1rem !important; }
    ")),

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
          // Check consent before allowing access to upload tab
          if (tabId === 'upload' && !window._dataConsentGiven) {
            // Show consent modal instead
            if (typeof Shiny !== 'undefined') {
              Shiny.setInputValue('request_consent', Math.random(), {priority: 'event'});
            }
            return;
          }

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

      // Initialize consent flag
      window._dataConsentGiven = false;

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

        // Data consent modal logic
        Shiny.addCustomMessageHandler('showConsentModal', function(msg) {
          $('#consent-overlay').addClass('active');
        });

        Shiny.addCustomMessageHandler('hideConsentModal', function(msg) {
          $('#consent-overlay').removeClass('active');
        });

        // Handle consent buttons
        $(document).on('click', '#consent-agree-btn', function() {
          window._dataConsentGiven = true;
          $('#consent-overlay').removeClass('active');
          Shiny.setInputValue('data_consent', true, {priority: 'event'});
          // If user clicked Try Demo Data before consent, trigger it now
          if (window._pendingDemoLoad) {
            window._pendingDemoLoad = false;
            Shiny.setInputValue('load_demo_from_home', true, {priority: 'event'});
          }
        });

        $(document).on('click', '#consent-disagree-btn', function() {
          window._dataConsentGiven = false;
          window._pendingDemoLoad = false;
          $('#consent-overlay').removeClass('active');
          TabManager.showTab('home');
          Shiny.setInputValue('data_consent', false, {priority: 'event'});
        });

        // Reset file inputs handler (used by Delete My Data)
        Shiny.addCustomMessageHandler('resetFileInputs', function(msg) {
          // Clear all custom file input displays
          var ids = ['asv', 'tax', 'meta', 'phylo', 'biom'];
          ids.forEach(function(id) {
            var fileInput = document.getElementById(id);
            if (fileInput) fileInput.value = '';
            var display = document.getElementById(id + '-display');
            if (display) {
              display.value = '';
              $(display).parent().removeClass('file-selected');
            }
            var info = document.getElementById(id + '-info');
            if (info) info.innerHTML = '';
          });
        });

        // Tooltip initialization
        $('[data-tooltip]').popup();
      });
    "))
  ),

  # Top navigation menu
  div(
    class = "ui top fixed menu",
    a(
      class = "item logo", href = "#", style = "pointer-events: none;",
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
  div(
    style = "margin-top: 60px;",

    # ============================================================
    # HOME TAB (REDESIGNED)
    # ============================================================
    div(
      id = "home", class = "ui container main-container",

      # ========== HERO SECTION — Dark Scientific ==========
      div(
        class = "hero-section",
        # Animated mesh overlay (CSS-driven)
        div(class = "hero-mesh"),
        div(
          class = "hero-content",
          div(
            class = "hero-badge",
            tags$i(class = "fas fa-flask"),
            "Microbiome Analysis Platform"
          ),
          h1(class = "hero-title", "MicrobeStudio"),
          p(
            class = "hero-subtitle",
            "From raw amplicon data to publication-ready insights — no coding required"
          ),
          div(
            class = "hero-actions",
            actionButton("cta_upload", tags$span(tags$i(class = "fas fa-upload"), " Upload Your Data"),
              class = "ui huge button cta-primary",
              onclick = "TabManager.showTab('upload')"
            ),
            actionButton("cta_demo", tags$span(tags$i(class = "fas fa-play-circle"), " Try Demo Data"),
              class = "ui huge button cta-secondary",
              onclick = "window._pendingDemoLoad = true; TabManager.showTab('upload');"
            )
          ),
          # Stat counters
          div(
            class = "hero-stats",
            div(
              class = "hero-stat",
              div(class = "hero-stat-number", "10+"),
              div(class = "hero-stat-label", "Analysis Modules")
            ),
            div(
              class = "hero-stat",
              div(class = "hero-stat-number", "PDF"),
              div(class = "hero-stat-label", "Export All Plots")
            ),
            div(
              class = "hero-stat",
              div(class = "hero-stat-number", "4"),
              div(class = "hero-stat-label", "Input Formats")
            ),
            div(
              class = "hero-stat",
              div(class = "hero-stat-number", tags$i(class = "fas fa-check")),
              div(class = "hero-stat-label", "Statistical Tests")
            )
          )
        )
      ),

      # ========== CAPABILITIES SECTION ==========
      div(
        class = "capabilities-section",
        h2(class = "section-title center-text", "What You Can Do"),
        div(
          class = "capabilities-grid",
          div(
            class = "capability-card capability-diversity",
            div(class = "capability-icon", tags$i(class = "fas fa-chart-line")),
            div(
              class = "capability-body",
              h4("Diversity Analysis"),
              p("Measure alpha diversity (Shannon, Simpson, Chao1), compare groups with Kruskal-Wallis tests, and assess sampling adequacy with rarefaction curves")
            )
          ),
          div(
            class = "capability-card capability-community",
            div(class = "capability-icon", tags$i(class = "fas fa-layer-group")),
            div(
              class = "capability-body",
              h4("Community Profiling"),
              p("Visualize taxonomic composition with bar plots, heatmaps, and dendrograms at any rank from Phylum to Species")
            )
          ),
          div(
            class = "capability-card capability-stats",
            div(class = "capability-icon", tags$i(class = "fas fa-vials")),
            div(
              class = "capability-body",
              h4("Statistical Testing"),
              p("Run PERMANOVA, PERMDISP, and ordination (NMDS, PCoA, t-SNE) with significance testing and downloadable results")
            )
          ),
          div(
            class = "capability-card capability-env",
            div(class = "capability-icon", tags$i(class = "fas fa-project-diagram")),
            div(
              class = "capability-body",
              h4("Environmental Correlations"),
              p("RDA, Mantel tests, regression analysis, and SHAP-based indicator species identification linking taxa to metadata")
            )
          )
        )
      ),

      # ========== WORKFLOW SECTION ==========
      div(
        class = "workflow-section",
        h2(class = "section-title center-text", "How It Works"),
        div(
          class = "workflow-timeline",
          div(
            class = "workflow-step",
            div(class = "step-number", "1"),
            h4("Upload"),
            p("Drag & drop phyloseq .rds, BIOM, or CSV files")
          ),
          div(class = "workflow-connector"),
          div(
            class = "workflow-step",
            div(class = "step-number", "2"),
            h4("Filter & Normalize"),
            p("Remove rare taxa, apply rarefaction or TSS")
          ),
          div(class = "workflow-connector"),
          div(
            class = "workflow-step",
            div(class = "step-number", "3"),
            h4("Analyze & Export"),
            p("Explore modules, download plots as PDF")
          )
        )
      ),

      # ========== MODULES SECTION ==========
      div(
        class = "modules-section",
        h2(class = "section-title center-text", "Analysis Modules"),
        div(
          class = "modules-grid-new",
          # Upload
          div(
            class = "module-card-new module-accent-upload",
            onclick = "TabManager.showTab('upload')",
            div(class = "module-card-icon", tags$i(class = "fas fa-cloud-upload-alt")),
            div(
              class = "module-card-body",
              h4("Upload Data"),
              p("Load .rds, BIOM, or CSV files")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-chevron-right"))
          ),
          # Filter
          div(
            class = "module-card-new module-card-locked module-accent-filter",
            div(class = "module-card-icon", tags$i(class = "fas fa-filter")),
            div(
              class = "module-card-body",
              h4("Filter & Normalize"),
              p("Remove taxa, apply TSS or rarefaction")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # Rarefaction
          div(
            class = "module-card-new module-card-locked module-accent-rarefaction",
            div(class = "module-card-icon", tags$i(class = "fas fa-chart-area")),
            div(
              class = "module-card-body",
              h4("Rarefaction Curves"),
              p("Assess sampling depth adequacy")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # Abundance
          div(
            class = "module-card-new module-card-locked module-accent-abundance",
            div(class = "module-card-icon", tags$i(class = "fas fa-chart-bar")),
            div(
              class = "module-card-body",
              h4("Abundance Plots"),
              p("Bar plots, heatmaps, line charts")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # Alpha
          div(
            class = "module-card-new module-card-locked module-accent-alpha",
            div(class = "module-card-icon", tags$i(class = "fas fa-calculator")),
            div(
              class = "module-card-body",
              h4("Alpha Diversity"),
              p("Shannon, Simpson, Chao1 with statistics")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # Dendrogram
          div(
            class = "module-card-new module-card-locked module-accent-dendrogram",
            div(class = "module-card-icon", tags$i(class = "fas fa-sitemap")),
            div(
              class = "module-card-body",
              h4("Dendrogram"),
              p("Hierarchical clustering of samples")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # Ordination
          div(
            class = "module-card-new module-card-locked module-accent-ordination",
            div(class = "module-card-icon", tags$i(class = "fas fa-project-diagram")),
            div(
              class = "module-card-body",
              h4("Ordination"),
              p("NMDS, PCoA, t-SNE with stress info")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # PERMANOVA
          div(
            class = "module-card-new module-card-locked module-accent-permanova",
            div(class = "module-card-icon", tags$i(class = "fas fa-not-equal")),
            div(
              class = "module-card-body",
              h4("PERMANOVA"),
              p("Statistical testing of group differences")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # Metadata
          div(
            class = "module-card-new module-card-locked module-accent-metadata",
            div(class = "module-card-icon", tags$i(class = "fas fa-table")),
            div(
              class = "module-card-body",
              h4("Metadata Analysis"),
              p("RDA, correlations, Mantel tests")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # Regression
          div(
            class = "module-card-new module-card-locked module-accent-regression",
            div(class = "module-card-icon", tags$i(class = "fas fa-chart-line")),
            div(
              class = "module-card-body",
              h4("Regression"),
              p("Taxa vs. environmental variables")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          ),
          # Indicator
          div(
            class = "module-card-new module-card-locked module-accent-indicator",
            div(class = "module-card-icon", tags$i(class = "fas fa-star")),
            div(
              class = "module-card-body",
              h4("Indicator Species"),
              p("SHAP-based biomarker identification")
            ),
            div(class = "module-card-arrow", tags$i(class = "fas fa-lock"))
          )
        )
      ),

      # ========== TRUST BAR ==========
      div(
        class = "trust-bar",
        span(class = "trust-label", "Powered by"),
        div(
          class = "trust-packages",
          span(class = "trust-pkg", "phyloseq"),
          span(class = "trust-pkg", "vegan"),
          span(class = "trust-pkg", "microeco"),
          span(class = "trust-pkg", "ggplot2"),
          span(class = "trust-pkg", "XGBoost"),
          span(class = "trust-pkg", "phylosmith")
        )
      ),

      # ========== FOOTER ==========
      div(
        class = "home-footer",
        div(
          class = "footer-inner",
          tags$a(href = "https://github.com/shanptom/MicrobeStudio", target = "_blank", tags$i(class = "fab fa-github"), " GitHub"),
          span(class = "footer-sep", "\u00B7"),
          tags$a(href = "#", onclick = "TabManager.showTab('manual'); return false;", tags$i(class = "fas fa-book"), " User Guide")
        )
      )
    ),
    # ============================================================
    # USER MANUAL TAB
    # ============================================================
    div(
      id = "manual", class = "ui container main-container hidden-tab",
      div(
        class = "ui segment",
        includeMarkdown("docs/user_guide.md"),
        tags$hr(),
        includeMarkdown("docs/Phyloseq.md")
      )
    ),

    # ============================================================
    # UPLOAD DATA TAB
    # ============================================================
    div(
      id = "upload", class = "ui container main-container hidden-tab",
      div(
        class = "ui grid",
        div(
          class = "four wide column",
          div(
            class = "ui segment sidebar-segment",
            h3("Data Input"),
            div(
              class = "ui form",
              # Custom Semantic UI File Upload - Count Table
              div(
                class = "field",
                div(
                  class = "ui fluid action input file-upload-wrapper",
                  tags$input(
                    type = "file", id = "asv", name = "asv", accept = ".csv",
                    style = "display: none;",
                    onchange = "handleFileSelect(this, 'asv')"
                  ),
                  tags$input(
                    type = "text", id = "asv-display", readonly = "",
                    placeholder = "Upload count table", class = "file-display-input"
                  ),
                  tags$label(
                    `for` = "asv", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "asv-info", class = "file-info-display")
              ),

              # Custom Semantic UI File Upload - Taxonomy Table
              div(
                class = "field",
                div(
                  class = "ui fluid action input file-upload-wrapper",
                  tags$input(
                    type = "file", id = "tax", name = "tax", accept = ".csv",
                    style = "display: none;",
                    onchange = "handleFileSelect(this, 'tax')"
                  ),
                  tags$input(
                    type = "text", id = "tax-display", readonly = "",
                    placeholder = "Upload taxonomy table", class = "file-display-input"
                  ),
                  tags$label(
                    `for` = "tax", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "tax-info", class = "file-info-display")
              ),

              # Custom Semantic UI File Upload - Metadata Table
              div(
                class = "field",
                div(
                  class = "ui fluid action input file-upload-wrapper",
                  tags$input(
                    type = "file", id = "meta", name = "meta", accept = ".csv",
                    style = "display: none;",
                    onchange = "handleFileSelect(this, 'meta')"
                  ),
                  tags$input(
                    type = "text", id = "meta-display", readonly = "",
                    placeholder = "Upload metadata table", class = "file-display-input"
                  ),
                  tags$label(
                    `for` = "meta", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "meta-info", class = "file-info-display")
              ),
              div(class = "ui horizontal divider", "OR"),

              # Custom Semantic UI File Upload - Phyloseq Object
              div(
                class = "field",
                div(
                  class = "ui fluid action input file-upload-wrapper",
                  tags$input(
                    type = "file", id = "phylo", name = "phylo", accept = ".rds",
                    style = "display: none;",
                    onchange = "handleFileSelect(this, 'phylo')"
                  ),
                  tags$input(
                    type = "text", id = "phylo-display", readonly = "",
                    placeholder = "Upload phyloseq object", class = "file-display-input"
                  ),
                  tags$label(
                    `for` = "phylo", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "phylo-info", class = "file-info-display")
              ),
              div(class = "ui horizontal divider", "OR"),

              # Custom Semantic UI File Upload - BIOM File
              div(
                class = "field",
                div(
                  class = "ui fluid action input file-upload-wrapper",
                  tags$input(
                    type = "file", id = "biom", name = "biom", accept = ".biom",
                    style = "display: none;",
                    onchange = "handleFileSelect(this, 'biom')"
                  ),
                  tags$input(
                    type = "text", id = "biom-display", readonly = "",
                    placeholder = "Upload BIOM file (.biom)", class = "file-display-input"
                  ),
                  tags$label(
                    `for` = "biom", class = "ui icon button primary",
                    tags$i(class = "cloud upload icon"),
                    "Browse"
                  )
                ),
                div(id = "biom-info", class = "file-info-display"),
                tags$small(class = "text-muted", "BIOM v1 (JSON) or v2 (HDF5). Optionally add metadata CSV above.")
              ),
              div(class = "ui divider"),

              # Action button to process uploaded files + inline loader
              div(
                style = "display: flex; align-items: center; gap: 0.75rem;",
                actionButton("process_files", "Upload Files", class = "ui primary button"),
                div(id = "upload_spinner", class = "ui active inline loader", style = "display: none;")
              )
            ),
            div(class = "ui divider"),
            h4("Load Demo Data"),
            div(
              class = "ui form",
              div(
                class = "field",
                selectInput("demo_file", "Select Demo Dataset:",
                  choices = c(
                    "Phyloseq RDS (demo_ps.rds)" = "rds",
                    "CSV Set (demo_asv.csv, demo_meta.csv, demo_tax.csv)" = "csv"
                  )
                )
              ),
              actionButton("load_demo", "Load Demo Data", class = "ui button fluid", style = "background: linear-gradient(135deg, #f0f4ff, #e8eeff); color: #0066cc; border: 1px solid rgba(0,102,204,0.3); font-weight: 600;")
            ),
            div(class = "ui divider"),
            # Delete My Data section
            div(
              class = "delete-data-section",
              tags$p(
                class = "delete-data-info",
                tags$i(class = "fas fa-shield-alt"),
                " Uploaded files are stored temporarily on the server."
              ),
              actionButton(
                "delete_my_data",
                tags$span(tags$i(class = "fas fa-trash-alt"), " Delete My Data"),
                class = "ui button fluid delete-data-btn"
              )
            )
          )
        ),
        div(
          class = "twelve wide column",
          div(
            class = "ui segment",
            uiOutput("upload_status_ui")
          )
        )
      )
    ),

    # ============================================================
    # FILTER TAB
    # ============================================================
    div(
      id = "filter", class = "ui container main-container hidden-tab",
      div(
        class = "ui grid",
        div(
          class = "four wide column",
          div(
            class = "ui segment sidebar-segment",
            h3("Data Filtering"),

            # Accordion for organized sections
            div(
              id = "ordination_accordion", class = "ui styled accordion",
              # Normalization Options
              div(
                class = "active title",
                tags$i(class = "dropdown icon"),
                "Normalization Options"
              ),
              div(
                class = "active content",
                div(
                  class = "ui form",
                  div(
                    class = "field",
                    checkboxInput("doRarefy", "Apply rarefaction", value = FALSE)
                  ),
                  div(
                    class = "field",
                    checkboxInput("doTSS", "Normalize by TSS", value = FALSE)
                  )
                )
              ),

              # Taxa Filters
              div(
                class = "title",
                tags$i(class = "dropdown icon"),
                "Taxa Filters"
              ),
              div(
                class = "content",
                div(
                  class = "ui form",
                  uiOutput("taxa_filters")
                )
              )
            ),

            # Action buttons always visible at bottom
            div(
              class = "filter-actions",
              actionButton("apply_filter", "Apply Filtering", class = "ui primary button fluid"),
              actionButton("go_analysis", "Go to Analysis", class = "ui positive button fluid")
            )
          )
        ),
        div(
          class = "twelve wide column",
          div(
            class = "ui segment",
            h3("Dataset Summary"),
            uiOutput("filter_status"),
            div(style = "margin-top: 8px;"),
            downloadButton("download_phyloseq", "Download Filtered Phyloseq (.rds)", class = "ui button")
          )
        )
      )
    ),

    # ============================================================
    # RAREFACTION TAB
    # ============================================================
    div(
      id = "rarefaction", class = "ui container main-container hidden-tab",
      div(
        class = "ui grid",
        div(
          class = "four wide column",
          div(
            class = "ui segment sidebar-segment",
            h3("Plot Controls"),
            div(
              class = "ui form",
              uiOutput("rarefaction_color_selector"),
              uiOutput("rarefaction_facet_selector"),
              div(
                class = "field",
                label("Axis Text Size"),
                sliderInput("rarefaction_beta_label_size", NULL, min = 6, max = 20, value = 12)
              ),
              div(
                class = "field",
                label("Sample Label Size"),
                sliderInput("rarefaction_label_size", NULL, min = 2, max = 10, value = 4)
              ),
              div(
                class = "field",
                checkboxInput("show_rarefaction_labels", "Show Sample Labels", value = TRUE)
              ),
              uiOutput("rarefaction_facet_order_selector")
            )
          )
        ),
        div(
          class = "twelve wide column",
          div(
            class = "ui segment plot-container",
            withSpinner(plotOutput("rarefactionPlot", width = "100%", height = "770px")),
            div(style = "margin-top: 8px;"),
            downloadButton("download_rarefaction_plot", "Download Plot (PDF)", class = "ui button")
          )
        )
      )
    ),

    # ============================================================
    # ABUNDANCE TAB
    # ============================================================
    div(
      id = "abundance", class = "ui container main-container hidden-tab",
      div(
        class = "ui grid",
        div(
          class = "four wide column",
          div(
            class = "ui segment sidebar-segment",
            h3("Plot Settings"),
            div(
              class = "ui form",
              div(
                class = "field",
                label("Plot Type"),
                selectInput("abund_plot_type", NULL,
                  choices = c("Bar" = "bar", "Line" = "line", "Heatmap" = "heat"),
                  selected = "bar"
                )
              ),
              uiOutput("tax_rank_selector"),
              div(
                class = "field",
                label("Number of Top Taxa"),
                sliderInput("ntaxa", NULL, min = 5, max = 15, value = 8, step = 1)
              ),
              uiOutput("abundance_facet_selector"),
              uiOutput("abundance_order_selector"),
              div(
                class = "field",
                label("Axis Text Size"),
                sliderInput("abundance_beta_label_size", NULL, min = 6, max = 20, value = 12)
              ),
              div(
                class = "field",
                checkboxInput("flip_abundance", "Flip axes (horizontal plot)", value = FALSE)
              )
            )
          )
        ),
        div(
          class = "twelve wide column",
          div(
            class = "ui segment plot-container",
            withSpinner(uiOutput("abundance_plot_output")),
            div(style = "margin-top: 8px;"),
            downloadButton("download_abundance_plot", "Download Plot (PDF)", class = "ui button")
          )
        )
      )
    ),

    # ============================================================
    # ALPHA DIVERSITY TAB
    # ============================================================
    div(
      id = "alpha", class = "ui container main-container hidden-tab",
      div(
        class = "ui grid",
        div(
          class = "four wide column",
          div(
            class = "ui segment sidebar-segment",
            h3("Diversity Indices"),
            div(
              class = "ui form",
              div(
                class = "field",
                label("Select Diversity Index"),
                checkboxGroupInput("alpha_index", NULL,
                  choices = c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher"),
                  selected = c("Shannon")
                )
              ),
              uiOutput("alpha_group_selector"),
              uiOutput("alpha_colour_selector"),
              div(
                class = "field",
                checkboxInput("flip_alpha", "Flip axes (horizontal plot)", value = FALSE)
              ),
              uiOutput("alpha_order_selector"),
              div(
                class = "field",
                label("Text Label Size"),
                sliderInput("alpha_beta_label_size", NULL, min = 6, max = 20, value = 12)
              )
            )
          )
        ),
        div(
          class = "twelve wide column",
          div(
            class = "ui segment plot-container",
            uiOutput("alpha_plot_output"),
            uiOutput("alpha_stats"),
            div(
              style = "margin-top: 8px;",
              downloadButton("download_alpha_plot", "Download Plot (PDF)", class = "ui button"),
              downloadButton("download_alpha_table", "Download Table (CSV)", class = "ui button", style = "margin-left: 4px;")
            )
          )
        )
      )
    ),

    # ============================================================
    # DENDROGRAM TAB
    # ============================================================
    div(
      id = "dendrogram", class = "ui container main-container hidden-tab",
      div(
        class = "ui grid",
        div(
          class = "four wide column",
          div(
            class = "ui segment sidebar-segment",
            h3("Dendrogram Settings"),
            div(
              class = "ui form",
              uiOutput("dend_treatment_selector"),
              div(
                class = "field",
                label("Distance Method"),
                selectInput("dend_method", NULL,
                  choices = c(
                    "euclidian", "manhattan", "canberra", "clark", "bray",
                    "kulczynski", "jaccard", "gower", "altGower", "morisita",
                    "horn", "mountford", "raup", "binomial", "chao", "cao", "mahalanobis"
                  ),
                  selected = "bray"
                )
              ),
              div(
                class = "field",
                label("Label Size"),
                sliderInput("dend_label_size", NULL, min = 3, max = 10, value = 5, step = 1)
              ),
              div(
                class = "field",
                label("Text Size"),
                sliderInput("dend_text_size", NULL, min = 6, max = 20, value = 12)
              )
            )
          )
        ),
        div(
          class = "twelve wide column",
          div(
            class = "ui segment plot-container",
            withSpinner(plotOutput("dendrogramPlot", width = "100%", height = "770px")),
            div(style = "margin-top: 8px;"),
            downloadButton("download_dendrogram_plot", "Download Plot (PDF)", class = "ui button")
          )
        )
      )
    ),

    # ============================================================
    # ORDINATION TAB
    # ============================================================
    div(
      id = "ordination", class = "ui container main-container hidden-tab",
      div(
        class = "ui grid",
        div(
          class = "four wide column",
          div(
            class = "ui segment sidebar-segment",
            h3("Settings"),

            # Ordination Settings Accordion
            div(
              class = "ui styled accordion",
              div(
                class = "title",
                tags$i(class = "dropdown icon"),
                "Ordination Settings"
              ),
              div(
                class = "content",
                div(
                  class = "ui form",
                  div(
                    class = "field",
                    label("Distance Method", `data-tooltip` = "Bray-Curtis: abundance-based. Jaccard: presence/absence. UniFrac: phylogenetic (requires tree).", `data-position` = "right center"),
                    selectInput("beta_dist", NULL,
                      choices = c(
                        "bray", "unifrac", "wunifrac", "jaccard", "dpcoa", "jsd",
                        "manhattan", "euclidean", "canberra", "binomial"
                      ),
                      selected = "bray"
                    )
                  ),
                  div(
                    class = "field",
                    label("Ordination Method"),
                    selectInput("beta_ord", NULL,
                      choices = c("NMDS", "MDS", "PCoA", "DCA", "CCA", "RDA", "DPCoA"),
                      selected = "NMDS"
                    )
                  ),
                  div(
                    class = "field",
                    label("Random Seed (for stochastic methods)"),
                    numericInput("ord_seed", NULL, value = 123, min = 1, step = 1)
                  )
                )
              )
            ),

            # Aesthetics Accordion
            div(
              class = "ui styled accordion",
              div(
                class = "title",
                tags$i(class = "dropdown icon"),
                "Aesthetics"
              ),
              div(
                class = "content",
                div(
                  class = "ui form",
                  uiOutput("beta_color_selector"),
                  uiOutput("beta_shape_selector"),
                  uiOutput("beta_label_selector"),
                  uiOutput("beta_facet_selector"),
                  uiOutput("beta_facet_order_selector"),
                  div(
                    class = "field",
                    label("Axis Text Size"),
                    sliderInput("beta_label_size", NULL, min = 6, max = 20, value = 12)
                  ),
                  div(
                    class = "field",
                    label("Label Text Size"),
                    sliderInput("beta_label_text_size", NULL, min = 2, max = 15, value = 3)
                  ),
                  div(
                    class = "field",
                    label("Shape Size"),
                    sliderInput("beta_shape_size", NULL, min = 1, max = 10, value = 4)
                  ),
                  div(
                    class = "field",
                    label("Point Transparency (alpha)"),
                    sliderInput("beta_alpha", NULL, min = 0.1, max = 1.0, value = 0.9, step = 0.05)
                  ),
                  div(
                    class = "field",
                    checkboxInput("beta_jitter", "Jitter points", value = FALSE)
                  ),
                  div(
                    class = "field",
                    checkboxInput("beta_label_repel", "Repel labels (reduce overlap)", value = TRUE)
                  ),
                  div(style = "margin-top: 6px;"),
                  actionButton("reset_ordination_aes", "Reset Aesthetics", class = "ui button")
                )
              )
            ),

            # t-SNE Analysis Accordion
            div(
              class = "ui styled accordion",
              div(
                class = "title",
                tags$i(class = "dropdown icon"),
                "t-SNE Analysis"
              ),
              div(
                class = "content",
                div(
                  class = "ui form",
                  uiOutput("tsne_group_selector"),
                  uiOutput("tsne_perplexity_selector"),
                  div(
                    class = "field",
                    checkboxInput("tsne_circle", "Draw circles", value = FALSE)
                  ),
                  uiOutput("tsne_label_selector"),
                  div(
                    class = "field",
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
        div(
          class = "twelve wide column",
          div(
            class = "ui segment plot-container",
            div(
              class = "ui tiny info message",
              strong("Ordination Info: "),
              textOutput("ordination_info", inline = TRUE)
            ),
            div(
              class = "ui tiny message",
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
            ),
            div(style = "margin-top: 8px;"),
            downloadButton("download_ordination_plot", "Download Plot (PDF)", class = "ui button")
          )
        )
      )
    ),

    # ============================================================
    # PERMANOVA TAB
    # ============================================================
    div(
      id = "permanova", class = "ui container main-container hidden-tab",
      div(
        class = "ui grid",
        div(
          class = "four wide column",
          div(
            class = "ui segment sidebar-segment",
            h3("PERMANOVA"),
            div(
              class = "ui form",
              div(
                class = "field",
                label("Distance Method", `data-tooltip` = "Bray-Curtis: abundance-based. Jaccard: presence/absence. UniFrac: phylogenetic (requires tree).", `data-position` = "right center"),
                selectInput("permanova_dist", NULL,
                  choices = c(
                    "bray", "unifrac", "wunifrac", "jaccard", "dpcoa", "jsd",
                    "manhattan", "euclidean", "canberra", "binomial"
                  ),
                  selected = "bray"
                )
              ),
              uiOutput("permanova_group_selector"),
              uiOutput("permanova_strata_selector"),
              div(
                class = "two fields",
                div(
                  class = "field",
                  label("Permutations", `data-tooltip` = "999 is standard for most analyses. Use 9999 for publication-quality results.", `data-position` = "right center"),
                  numericInput("permanova_permutations", NULL, value = 999, min = 99, step = 100)
                ),
                div(
                  class = "field",
                  label("P-value Adjustment"),
                  selectInput("p_adjust_method", NULL, choices = c("BH", "Holm", "Bonferroni", "BY"), selected = "BH")
                )
              ),
              div(
                class = "field",
                checkboxInput("permdisp_enable", "Check homogeneity of dispersion (PERMDISP)", value = FALSE)
              ),
              div(
                class = "two fields",
                div(
                  class = "field",
                  actionButton("run_permanova", "Run PERMANOVA", class = "ui primary button fluid")
                ),
                div(
                  class = "field",
                  actionButton("reset_permanova", "Reset", class = "ui button fluid")
                )
              )
            )
          )
        ),
        div(
          class = "twelve wide column",
          div(
            class = "ui segment plot-container",
            div(
              class = "ui info message", style = "display: block;",
              strong("About PERMANOVA: "),
              HTML("Tests for differences in community composition between groups using permutations of a distance matrix (<em>adonis2</em>). "),
              HTML("<strong>R\u00B2</strong> indicates effect size (proportion of variance explained). "),
              HTML("Consider checking homogeneity of dispersion (PERMDISP) to validate assumptions.")
            ),
            div(
              class = "ui tiny message",
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
  div(
    id = "metadata", class = "ui container main-container hidden-tab",
    div(
      class = "ui grid",
      div(
        class = "four wide column",
        div(
          class = "ui segment sidebar-segment",
          style = "overflow: visible !important; z-index: 100;",
          h3("Setup"),
          div(
            class = "ui form",
            uiOutput("numeric_column_selector_ui"),
            actionButton("create_transenv", "Create trans_env Object", class = "ui primary button fluid"),
            br(), br(),
            verbatimTextOutput("transenv_display"),
            uiOutput("continue_button_ui"),
            uiOutput("visualization_sidebar")
          )
        )
      ),
      div(
        class = "twelve wide column",
        div(
          class = "ui segment plot-container",
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
          ),
          div(style = "margin-top: 8px;"),
          downloadButton("download_metadata_plot", "Download Plot (PDF)", class = "ui button download-btn")
        )
      )
    )
  ),

  # ============================================================
  # REGRESSION TAB
  # ============================================================
  div(
    id = "regression", class = "ui container main-container hidden-tab",
    div(
      class = "ui grid",
      div(
        class = "four wide column",
        div(
          class = "ui segment sidebar-segment",
          h3("Regression Setup"),
          div(
            class = "ui form",
            uiOutput("tax_rank_selector_regression"),
            uiOutput("taxa_selector_regression"),
            uiOutput("env_var_selector"),
            uiOutput("regression_group_selector"),
            actionButton("run_scatter", "Run Scatter Plot", class = "ui primary button fluid"),
            div(
              class = "field",
              label("Point Size"),
              sliderInput("point_size", NULL, min = 1, max = 6, value = 3)
            ),
            div(
              class = "field",
              label("Text Size"),
              sliderInput("text_size", NULL, min = 6, max = 20, value = 12)
            )
          )
        )
      ),
      div(
        class = "twelve wide column",
        div(
          class = "ui segment plot-container",
          withSpinner(plotOutput("regression_plot", width = "100%", height = "770px")),
          div(style = "margin-top: 8px;"),
          downloadButton("download_regression_plot", "Download Plot (PDF)", class = "ui button download-btn")
        )
      )
    )
  ),

  # ============================================================
  # INDICATOR SPECIES TAB
  # ============================================================
  div(
    id = "indicator", class = "ui container main-container hidden-tab",
    div(
      class = "ui grid",
      div(
        class = "four wide column",
        div(
          class = "ui segment sidebar-segment",
          h3("SHAP Analysis Setup"),
          div(
            class = "ui form",
            uiOutput("indicator_variable_selector"),
            uiOutput("indicator_group1_selector"),
            uiOutput("indicator_group2_selector"),
            div(
              class = "field",
              label("Number of Top Taxa to Display"),
              sliderInput("top_n_taxa", NULL, min = 5, max = 30, value = 10, step = 1)
            ),
            div(
              class = "field",
              label("Font Size"),
              sliderInput("indicator_font_size", NULL, min = 6, max = 20, value = 10, step = 1)
            ),
            actionButton("run_indicator_analysis", "Run Analysis", class = "ui primary button fluid")
          )
        )
      ),
      div(
        class = "twelve wide column",
        div(
          class = "ui segment plot-container",
          withSpinner(plotOutput("indicator_plot", width = "100%", height = "770px")),
          div(
            style = "margin-top: 8px;",
            downloadButton("download_indicator_plot", "Download Plot (PDF)", class = "ui button download-btn"),
            downloadButton("download_indicator_table", "Download Table (CSV)", class = "ui button download-btn", style = "margin-left: 4px;")
          ),
          h3("Results Table"),
          DT::DTOutput("indicator_table")
        )
      )
    )
  ),

  # ============================================================
  # DATA CONSENT MODAL OVERLAY
  # ============================================================
  div(
    id = "consent-overlay", class = "consent-overlay",
    div(
      class = "consent-modal",
      div(
        class = "consent-header",
        tags$i(class = "fas fa-shield-alt consent-icon"),
        h3("Data Collection & Privacy Policy")
      ),
      div(
        class = "consent-body",
        div(
          class = "consent-section",
          h4(tags$i(class = "fas fa-chart-bar"), " What We Collect"),
          tags$ul(
            tags$li("Anonymous usage patterns (which tools and tabs you use)"),
            tags$li("Session duration and general workflow statistics"),
            tags$li("Approximate geographic location (country/region only)"),
            tags$li("Browser and operating system type")
          )
        ),
        div(
          class = "consent-section",
          h4(tags$i(class = "fas fa-lock"), " What We Do NOT Collect"),
          tags$ul(
            tags$li("Your uploaded data files or their contents"),
            tags$li("Personal information (name, email, IP address)"),
            tags$li("Analysis results or parameter choices"),
            tags$li("Any data that could identify you personally")
          )
        ),
        div(
          class = "consent-section",
          h4(tags$i(class = "fas fa-trash-alt"), " Your Data, Your Control"),
          tags$ul(
            tags$li("Uploaded files are stored temporarily on the server during your session"),
            tags$li("You can delete all your uploaded data at any time using the \"Delete My Data\" button"),
            tags$li("Files are automatically removed when your session ends")
          )
        ),
        div(
          class = "consent-note",
          tags$i(class = "fas fa-info-circle"),
          " This data helps us improve MicrobeStudio. All analytics are aggregated and anonymous."
        )
      ),
      div(
        class = "consent-actions",
        tags$button(
          id = "consent-agree-btn", class = "consent-btn consent-btn-agree",
          tags$i(class = "fas fa-check"), " I Agree"
        ),
        tags$button(
          id = "consent-disagree-btn", class = "consent-btn consent-btn-disagree",
          tags$i(class = "fas fa-times"), " Do Not Agree"
        )
      )
    )
  )
) # End of semanticPage
