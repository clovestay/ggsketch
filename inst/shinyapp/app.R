library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork)
library(bslib)


design_from_df <- function(df, add.margins = T) {

    pad = 1

    areas <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]

      patchwork::area(
        l = (row$x + 1) * pad,
        t = (row$y + 1) * pad,
        r = (row$x + row$w) * pad,
        b = (row$y + row$h) * pad
      )
    })

    do.call(c, areas)
}

build_patchwork <- function(df, available, add.labels=T, add.margins=0) {
  plots.for.layout = available[df$id]
  if(add.labels) {
    plots.for.layout <- lapply(seq_along(df$id), function(i) {
      annotate_figure(plots.for.layout[[i]],
                              fig.lab = letters[[i]], fig.lab.face = "bold",
                              fig.lab.pos = "top.left", fig.lab.size = 24, left = " ")
    })
  }
  if(add.margins > 0) {
    plots.for.layout <- lapply(seq_along(df$id), function(i) {
      plots.for.layout[[i]] +
        ggplot2::theme(
          plot.margin = ggplot2::margin(
            t = add.margins,
            r = add.margins,
            b = add.margins,
            l = add.margins
          )
        )
    })
  }
  return(
    wrap_plots(plots.for.layout) + plot_layout(design = design_from_df(df))
  )
}
as_ggplot <- function (x)
{
  null_device <- base::getOption("ggsketch.null_device", default = cowplot::pdf_null_device)
  cur_dev <- grDevices::dev.cur()
  null_device(width = 6, height = 6)
  null_dev <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(null_dev)
    if (cur_dev > 1) grDevices::dev.set(cur_dev)
  })
  cowplot::ggdraw() + cowplot::draw_grob(grid::grobTree(x))
}
annotate_figure <- function (p, top = NULL, bottom = NULL, left = NULL, right = NULL,
                             fig.lab = NULL, fig.lab.pos = c("top.left", "top", "top.right",
                                                             "bottom.left", "bottom", "bottom.right"), fig.lab.size,
                             fig.lab.face)
{
  fig.lab.pos <- match.arg(fig.lab.pos)
  annot.args <- list(top = top, bottom = bottom, left = left,
                     right = right) %>% purrr::compact()
  lab.args <- list(label = fig.lab, position = fig.lab.pos) %>%
    purrr::compact()
  if (!missing(fig.lab.size))
    lab.args$size <- fig.lab.size
  if (!missing(fig.lab.face))
    lab.args$fontface <- fig.lab.face
  if (!purrr::is_empty(annot.args)) {
    p <- gridExtra::arrangeGrob(p, top = top, bottom = bottom,
                                left = left, right = right) %>% as_ggplot()
  }
  if (!is.null(fig.lab)) {
    p <- cowplot::ggdraw(p) + do.call(cowplot::draw_figure_label,
                                      lab.args)
  }
  p
}



ui <- page_fillable(
  theme = bs_theme(bootswatch = "sketchy"),

  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/gridstack.js/12.3.3/gridstack.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/gridstack.js/12.3.3/gridstack-all.min.js")
  ),

  tags$style(HTML("
    .grid-stack-item-content {
      background: #f8f9fa;
      border: 1px solid #ddd;
      overflow: hidden;
    }
  ")),

  titlePanel(
    div(
      style = "display:flex; justify-content:space-between; align-items:center;",
      h2("ggsketch", style = "margin:0;"),
      actionButton("quit", "Quit App", class = "btn-danger")
    )
  ),


  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function () {

      const grid = GridStack.init({
        column: 16,
        cellHeight: 80,
        //cellWidth: 80,
        margin: 5,
        minRow: 6,
        float: true,
        disableResize: false,

        acceptWidgets: true,
        dragIn: '.sidebar-widget',

        dragInOptions: {
          helper: 'clone',
          appendTo: 'body'
        }
      });

      grid.on('dropped', function(event, prev, widget) {

        console.log('trying to make widget')

        const el = document.createElement('template');
        el.innerHTML = `
            <div id='plot_${widget.id}' class='shiny-plot-output'
                 style='width:100%; height:100%'></div>`
        widget.el.querySelector('.grid-stack-item-content').replaceChildren(el.content.firstElementChild)
        widget.el.querySelector('.grid-stack-item-content').classList = 'grid-stack-item-content';

        if (window.Shiny && Shiny.bindAll) {
          Shiny.bindAll(widget.el);
        }
        Shiny.setInputValue('dragged_in_plot', {
          id: widget.id
        }, {priority: 'event'});

      });

      window.grid = grid;

      Shiny.addCustomMessageHandler('set_margin', function(m) {

          if (!window.grid) return;

          // update option
          window.grid.opts.margin = m;

          // apply layout recalculation safely
          window.grid.batchUpdate();
          window.grid.compact();
          window.grid.commit();

          // ensure geometry recalculates
          window.dispatchEvent(new Event('resize'));
      });

      // layout tracking (FIXED: safe payload)
      grid.on('change', function(event, items) {
        const clean = items.map(i => ({
          id: i.id,
          x: i.x,
          y: i.y,
          w: i.w,
          h: i.h
        }));

        Shiny.setInputValue('layout', clean, {priority: 'event'});
      });

      // resize trigger
      grid.on('resizestop', function (event, el) {

        Shiny.setInputValue(
          'grid_item_click',
          {
            id: el.getAttribute('gs-id'),
            x: el.getAttribute('gs-x') ?? 0,
            y: el.getAttribute('gs-y') ?? 0,
            w: el.getAttribute('gs-w') ?? 1,
            h: el.getAttribute('gs-h') ?? 1
          },
          {priority: 'event'}
        );

        setTimeout(() => {
          window.dispatchEvent(new Event('resize'));
        }, 10);
      });

      grid.on('dragstop', function(event, el) {

        Shiny.setInputValue(
          'grid_item_click',
          {
            id: el.getAttribute('gs-id'),
            x: el.getAttribute('gs-x') ?? 0,
            y: el.getAttribute('gs-y') ?? 0,
            w: el.getAttribute('gs-w') ?? 1,
            h: el.getAttribute('gs-h') ?? 1
          },
          {priority: 'event'}
        );

      });

      // ADD WIDGET (FIXED: correct API)
      Shiny.addCustomMessageHandler('add_widget', function(msg) {

        const el = document.createElement('div');
        el.className = 'grid-stack-item';
        el.setAttribute('gs-id', msg.id);
        el.setAttribute('gs-w', msg.w ?? 4);
        el.setAttribute('gs-h', msg.h ?? 3);
        el.setAttribute('gs-x', msg.x ?? 1);
        el.setAttribute('gs-y', msg.y ?? 1);

        el.innerHTML = `
          <div class='grid-stack-item-content'>
            <div id='plot_${msg.id}' class='shiny-plot-output'
                 style='width:100%; height:100%'></div>
          </div>
        `;

        window.grid.makeWidget(el);

        if (window.Shiny && Shiny.bindAll) {
          Shiny.bindAll(el);
        }

        //widget.el.appendElement(el)

      });

      Shiny.addCustomMessageHandler('remove_widget', function(msg) {
        const el = document.getElementById(`plot_${msg.id}`).parentElement.parentElement;
        console.log(el)
        if (el && window.grid) {
          window.grid.removeWidget(el);
        }
        Shiny.setInputValue('grid_item_click', 'none');
      });

      Shiny.addCustomMessageHandler('export_grid', function(msg) {

        console.log('exporting grid');

        const layout = window.grid.engine.nodes.map(n => ({
          id: n.id,
          x: n.x,
          y: n.y,
          w: n.w,
          h: n.h
        }));
        console.log(layout)

        Shiny.setInputValue('layout_export', JSON.stringify(layout), {priority: 'event'});
      });

      Shiny.addCustomMessageHandler('update-grid-dragins', function(msg) {
        GridStack.setupDragIn('.sidebar-widget')
      })

    });
    $(document).on('click', '.grid-stack-item', function() {
        console.log('clicked widget')
        // Get item info
          let id = $(this).attr('gs-id');
          let x  = $(this).attr('gs-x') ?? 0;
          let y  = $(this).attr('gs-y') ?? 0;
          let w  = $(this).attr('gs-w') ?? 0;
          let h  = $(this).attr('gs-h') ?? 0;

          // Send to shiny
          Shiny.setInputValue(
            'grid_item_click',
            {
              id: id,
              x: x,
              y: y,
              w: w,
              h: h
            },
            {priority: 'event'}
          );
      })
  ")),

  layout_columns(

      layout_column_wrap(
          card(
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center w-100",

                span("Environment"),

                actionButton(
                  "refresh_plots",
                  label = NULL,
                  icon = icon("rotate-right"),
                  class = "btn-sm btn-outline-secondary"
                )
              )
            ),
            uiOutput("buttons")
          ),
          card(
            card_header("Settings"),
            input_switch("settingsAutosave", "Autosave layout", value=ggsketch::fetch_autosave())
          ),
          heights_equal = "row", fill = F, fillable = T
      ),

      navset_card_tab(
        id = "editor_tab",
        nav_panel(
          "Editor", div(class = "grid-stack", id = "grid", style = "width: 100%;
          background: #eee; min-height: 600px;
          border: 2px dashed #ccc;"),
          value = "tab_editor"
        ),
        nav_panel(
          "Output", uiOutput("pdf_view"), value = "pdf_refresh"
        )
      ),

      layout_column_wrap(
        card(
          card_header("Properties"),
          uiOutput("properties")
        ),
        card(
          card_header("Render PDF"),
          input_switch("renderAddLetterLabels", "Add lettered labels", value=T),
          sliderInput("renderAddMargins", "Add plot margins", min = 0, max = 10, value = 6),
          sliderInput("renderBaseSize", "Figure scaling", min = 1, max = 8, value = 3),
          div(
            class = "d-flex justify-content-between align-items-center w-100",
            input_switch("renderUseFixedSize", "Fixed size", value=F),
            numericInput("renderWidth", "Width (mm)", value = NA),
            numericInput("renderHeight", "Height (mm)", value = NA)
          ),
          uiOutput("renderAll")
        )
      ),
      col_widths=c(2,8,2)
  )

)



server <- function(input, output, session) {


  active.plots <- reactiveVal(character(0))
  eligible.plots <- reactiveVal(character(0))
  available.plots <- reactiveVal(character(0))
  plot.labels <- reactiveVal(list())
  pdf_url <- reactiveVal("output.pdf")

  refreshAvailablePlots <- function(active) {
    temp.available <- ggsketch::get_plot_data()
    eligible <- temp.available
    eligible.plots(eligible)
    temp.available <- temp.available[!(names(temp.available) %in% active)]
    available.plots(temp.available)
  }

  observeEvent(TRUE, {
    current <- active.plots()
    refreshAvailablePlots(current)
  }, once = TRUE)

  observeEvent(input$refresh_plots, {
    current <- active.plots()
    refreshAvailablePlots(current)
  })

  observeEvent(input$editor_tab, {
    if (input$editor_tab == "pdf_refresh") {
      pdf_url(paste0("output.pdf?ts=", Sys.time()))
    }
  })

  output$pdf_view <- renderUI({
    tags$iframe(
      id = "pdf_frame",
      src = pdf_url(),
      width = "100%",
      height = "800px"
    )
  })

  output$properties <- renderText("Click on a plot in the grid to inspect")

  observeEvent(input$grid_item_click, {
    if(input$grid_item_click[[1]] == "none") {
      output$properties <- renderText("Click on a plot in the grid to inspect")
    } else {
      labels <- plot.labels()
      output$properties_table <- renderTable({
        df = data.frame(
          input$grid_item_click
        ) %>% dplyr::select(-id) %>% t %>% data.frame %>%
          `colnames<-`(c("value")) %>% tibble::rownames_to_column(var="field")
        return(df)
      })
      output$properties_id <- renderText(paste0(input$grid_item_click$id[[1]]))
      output$properties <- renderUI({
        tagList(
          textOutput("properties_id"),
          tableOutput("properties_table"),
          textInput(
            inputId = "properties_label",
            label = "Panel label",
            value = ifelse(!is.na(labels[input$grid_item_click$id]), labels[input$grid_item_click$id], "")
          ),
          actionButton("remove_plot_properties", "Remove Plot")
        )
      })
    }
  })
  observeEvent(input$remove_plot_properties,
               {
                 current <- active.plots()
                 active.plots(setdiff(current, input$grid_item_click$id))
                 refreshAvailablePlots(setdiff(current, input$grid_item_click$id))
                 session$sendCustomMessage("remove_widget", list(id = input$grid_item_click$id))
               })
  observeEvent(input$properties_label,
               {
                 labels <- plot.labels()
                 labels[input$grid_item_click$id] = input$properties_label
               })
  observeEvent(input$layout_export, {
    if(!is.null(input$layout_export) & length(active.plots()) > 0) {
      withProgress(message = 'Making plot', value = 0, {
        df = jsonlite::fromJSON(input$layout_export)
        ggsketch::save_sketch_layout(df)
        incProgress(0.5, detail = "Building layout..")

        layout_auto = build_patchwork(df, available = eligible.plots(),
                                      add.labels = input$renderAddLetterLabels, add.margins = input$renderAddMargins*2)
        width_mult = 1
        layout_auto_wh = df %>% mutate(xw = x*width_mult + w*width_mult,
                                       yh = y + h)

        if(input$renderUseFixedSize & !(is.na(input$renderWidth) & is.na(input$renderHeight))) {
          widthComputed = ifelse(
            is.na(input$renderWidth), # if not specified,
            (max(layout_auto_wh$xw) / max(layout_auto_wh$yh)) * input$renderHeight, # compute based on height;
            input$renderWidth / 25.4 # otherwise, use specified width
          )
          heightComputed = ifelse(
            is.na(input$renderHeight), # if not specified,
            (max(layout_auto_wh$yh) / max(layout_auto_wh$xw)) * input$renderWidth, # compute based on width;
            input$renderHeight / 25.4 # otherwise, use specified height
          )
          cowplot::save_plot(
            "www/output.pdf",
            plot = layout_auto,
            base_width = widthComputed,
            base_height = heightComputed
          )
        } else {
          cowplot::save_plot(
            "www/output.pdf",
            plot = layout_auto,
            base_width = max(layout_auto_wh$xw) * ((input$renderBaseSize + 3)/6),
            base_height = max(layout_auto_wh$yh) * ((input$renderBaseSize + 3)/6),
          )
        }

        incProgress(0.5, detail = "Rendering PDF..")
        # refresh render tab if loaded:
        if (input$editor_tab == "pdf_refresh") {
          pdf_url(paste0("output.pdf?ts=", Sys.time()))
        } else {
          updateTabsetPanel(
            session = session,
            inputId = "editor_tab",
            selected = "pdf_refresh"
          )
        }
      })

    } else {
    }
  })

  # ---------------------------
  # 1. Buttons
  # ---------------------------
  output$buttons <- renderUI({
    lapply(names(available.plots()), function(id) {
      tagList(
        div(
          class = "sidebar-widget grid-stack-item",
          id = paste0("btn_", id),
          `gs-id` = id,
          `gs-w` = 4,
          `gs-h` = 3,
          div(
            class = "grid-stack-item-content d-flex justify-content-between align-items-center w-100 p-4",
            span(id), icon("grip-vertical")
          )
        ),
        tags$script(HTML("
        setTimeout(function() {
          GridStack.setupDragIn('.sidebar-widget');
        }, 0);
        "))
      )
    })
  })

  output$renderAll <- renderUI({
    actionButton("renderAll", "Render All", width = "100%")
  })

  observeEvent(input$renderAll, {
    session$sendCustomMessage("export_grid", list())
  })


  # ---------------------------
  # 3. Add widget only (no rendering trigger)
  # ---------------------------
  observeEvent(TRUE, {
    lapply(names(available.plots()), function(id) {

      output[[paste0("plot_", id)]] <- renderPlot({
        eligible.plots()[[id]]
      })

      observeEvent(input$dragged_in_plot, {
        current <- active.plots()
        if(!(input$dragged_in_plot$id %in% current)) {
          active.plots(c(current, input$dragged_in_plot$id))
          refreshAvailablePlots(active = c(current, input$dragged_in_plot$id))
        }
      })

    })
  }, once=T)

  observeEvent(input$margin_grid, {
    session$sendCustomMessage("set_margin", input$margin_grid)
  })

  # ---------------------------
  # 4. Layout debug
  # ---------------------------

  # load layouts
  observeEvent(TRUE, {
    if(input$settingsAutosave) {
      last_layout <- ggsketch::get_sketch_layout()
      add.to.active.plots <- c()
      if(!is.null(last_layout)) {
        apply(last_layout, MARGIN=1, FUN=function(widget) {
          if(widget["id"][[1]] %in% names(ggsketch::get_plot_data())) {
            session$sendCustomMessage("add_widget", widget %>% as.list)
          }
        })
        add.to.active.plots <- last_layout$id[last_layout$id %in% names(ggsketch::get_plot_data())]
        active.plots(add.to.active.plots)
        refreshAvailablePlots(add.to.active.plots)
      }
    }
  }, once = TRUE)

  observeEvent(input$settingsAutosave, {
    ggsketch::toggle_autosave(input$settingsAutosave)
  }, ignoreInit = T)

  observeEvent(input$quit, {
    stopApp()
  })

}

shinyApp(ui, server)

