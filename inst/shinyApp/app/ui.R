library(shiny)
library(shinydashboard)
library(shinyjs)
library(sunburstR)
library(heatmaply)
library(shinyHeatmaply)


ui <- dashboardPage(
    
    dashboardHeader
    (
        title="FCS Generator 2.5"
    ),
    
    dashboardSidebar
    (
        sidebarMenu
        (
            id="tabs",
            menuItem("Reference Files", tabName="t_1"),
            menuItem("Compensate & Transform", tabName="t_2"),
            menuItem("Generate Files From Reference", tabName="t_3",
                     menuSubItem("MFI Shifts", tabName = "t_3_shifts"),
                     menuSubItem("Pseudo-timeline", tabName = "t_3_pt")),
            menuItem("Save Generated Files As References", tabName="t_3_conv"),
            menuItem("Modify Populations", tabName="t_4",
                menuSubItem("Move Populations", tabName = "t_4_move"),
                menuSubItem("Manage Populations", tabName = "t_4_manage"),
                menuSubItem("Change Sizes", tabName = "t_4_size")), 
            menuItem("Mix Files", tabName = "t_5"),
            menuItem("Visualization Tools", tabName = "t_6",
                     menuSubItem("Heatmaps", tabName = "t_6_hm"),
                     menuSubItem("Joyplot", tabName = "t_6_jp")),
            menuItem("Decompensate & Detransform", tabName="t_7"),
            menuItem("Download", tabName = "t_8")
            # menuItem("Log", tabName = "t_10")
        )
    ),
    
    dashboardBody
    (
        useShinyjs(),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
        fluidRow(
            tabItems
            (
                #Reference Files
                #===============================================================================================
                tabItem
                (
                    tabName="t_1",
                    column
                    (
                        width=9,
                        shinydashboard::box
                        (
                            width=12,title="ADD FILES",solidHeader=T,status="info",
                            shinydashboard::tabBox
                            (
                                width=12,id="t_1_tb",
                                tabPanel
                                (
                                    title="Generate New File", value="A",
                                    style="padding-right:0",
                                    fluidRow
                                    (
                                        style="margin-right:0",
                                        column
                                        (
                                            width=2,
                                            numericInput("t_1_nmb_files", "Number of Files", value=1),
                                            numericInput("t_1_nmb_events", "Number of Events", value=1000),
                                            numericInput("t_1_nmb_markers", "Number of Markers", value=2)
                                        ),
                                        column
                                        (
                                            width=3,
                                            numericInput("t_1_nmb_populations", "Number of Populations", value=1),
                                            actionButton("t_1_create", "Generate", width="100%")
                                        ),
                                        column
                                        (
                                            width=7,style="max-height:35vh;overflow:auto;padding:0",
                                            uiOutput("t_1_pop_list")
                                        )
                                    )
                                ),
                                tabPanel
                                (
                                    title="Import Files",value="B",
                                    fluidRow
                                    (
                                        width=12,
                                        column
                                        (
                                            width=4,
                                            actionButton("t_1_select", "Add Files", width="100%")
                                        ),
                                        column
                                        (
                                            width=8,style="max-height:35vh;overflow:visible;padding:0",
                                            uiOutput("t_1_fcs_list")
                                        )
                                    )
                                )
                            ),
                            column
                            (
                                width=12,
                                fluidRow(
                                    uiOutput("t_1_files_main")
                                )
                            )
                        )
                    ),
                    column
                    (
                        width=3,
                        shinydashboard::box
                        (
                            width=12,title="Remove Files",solidHeader=T,status="info",
                            selectInput("t_1_files_rm_sel", "Reference Files", choices = NULL, selected = NULL, multiple = T),
                            actionButton("t_1_files_rm", "Remove")
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                #Compensate and Transform
                #===============================================================================================
                tabItem
                (
                    tabName="t_2",
                    column
                    (
                        width=4,
                        shinydashboard::box
                        (
                            width=12,title="Compensation",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            actionButton("t_2_compensate", "Compensate Selection", width="100%")
                        ),
                        shinydashboard::box
                        (
                            width=12,title="Transformation",solidHeader=T,status="info",style="max-height:70vh;overflow:auto",
                            selectInput("t_2_transform_sel", "Transformation Type", choices=c("Logicle"=1, "Arcshinh"=2)),
                            uiOutput("t_2_transform_param"),
                            actionButton("t_2_transform", "Transform Selection", width="100%")
                        )
                    ),
                    column
                    (
                        width=8,
                        shinydashboard::box
                        (
                            width=12,title="Reference Files",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            fluidRow
                            (
                                column
                                (
                                    width=2,
                                    checkboxInput("t_2_file_select_all", "Select All", value = F)
                                ),
                                column
                                (
                                    width=4,style="padding-top:1%",
                                    "Reference File"
                                ),
                                column
                                (
                                    width=3,style="padding-top:1%",
                                    "Compensated"
                                ),
                                column
                                (
                                    width=3,style="padding-top:1%",
                                    "Transformed"
                                )
                            ),
                            uiOutput("t_2_files")
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                #Generate From Reference (SHIFTS)
                #===============================================================================================
                tabItem
                (
                    tabName="t_3_shifts",
                    column
                    (
                        
                        width=9,
                        shinydashboard::box
                        (
                            width=12,title="Generate Files with MFI shifts",solidHeader=T,status="info",
                            column
                            (
                                width=12,
                                fluidRow
                                (
                                    width=12,
                                    column
                                    (
                                        width=6,
                                        selectInput("t_3_shifts_ref_sel", "Reference File", choices = NULL, selected = NULL)
                                    )
                                    
                                ),
                                fluidRow
                                (
                                    width=12,
                                    column
                                    (
                                        width=4,
                                        selectInput("t_3_shifts_shifted_markers", "Shifted Markers", choices = NULL, selected = NULL, multiple = T)
                                    ),
                                    column
                                    (
                                        width=2,
                                        numericInput("t_3_shifts_mean_shift", "Max Mean shift", value = 0.8)
                                    ),
                                    column
                                    (
                                        width=2,
                                        numericInput("t_3_shifts_sd_shift", "Max SD shift", value = 0.2)
                                    )
                                ),
                                fluidRow
                                (
                                    width=12,
                                    column
                                    (
                                        width=4,
                                        selectInput("t_3_shifts_inverted_markers", "Inverted Markers", choices = NULL, selected = NULL, multiple = T)
                                    )
                                ),
                                fluidRow
                                (
                                    width=12,
                                    column
                                    (
                                        width=2
                                        
                                    )
                                )
                            )
                        ),
                        shinydashboard::box
                        (
                            width=12,title="Reference File - Heatmap",solidHeader=T,status="info",
                            fluidRow
                            (
                                column
                                (
                                    width=10,
                                    plotlyOutput("t_3_shifts_hm_ref", width = "100%")
                                )
                            )
                        ),
                        shinydashboard::box
                        (
                            width=12,title="Generated File - Heatmaps",solidHeader=T,status="info",style="overflow:auto",
                            uiOutput("t_3_shifts_hm_list")
                        )
                    ),
                    column
                    (
                        width=3,
                        shinydashboard::box
                        (
                            width=12,title="File Options",solidHeader=T,status="info",
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    numericInput("t_3_shifts_nmb_files", "Number of Files", value=1)
                                ),
                                column
                                (
                                    width=12,
                                    actionButton("t_3_shifts_generate", "Generate", width="100%")
                                ),
                                column
                                (
                                    width=12, style="margin-top:5vh",
                                    selectInput("t_3_shifts_remove_sel", "Generated Files", choices = NULL, selected = NULL, multiple = T),
                                    actionButton("t_3_shifts_remove", "Remove", width="100%")
                                )
                            )
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                #Generate With From Reference (TIMEPOINTS)
                #===============================================================================================
                tabItem
                (
                    tabName="t_3_pt",
                    column
                    (
                        width=3,
                        shinydashboard::box
                        (
                            width=12,title="File Options",solidHeader=T,status="info",
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_3_tp_ref_sel", "Reference File", choices = NULL, selected = NULL),
                                    actionButton("t_3_tp_add", "Add a TP", width="100%"),
                                    actionButton("t_3_tp_update", "Update", width="100%")
                                ),
                                column
                                (
                                    width=12, style="margin-top:5vh",
                                    selectInput("t_3_tp_remove_sel", "TP List", choices = NULL, selected = NULL, multiple = T),
                                    actionButton("t_3_tp_remove", "Remove", width="100%")
                                )
                            )
                        ),
                        shinydashboard::box
                        (
                            width=12,title="Visualization Options",solidHeader=T,status="info",
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_3_tp_M1", "1st Marker", choices = NULL, selected = NULL)
                                ),
                                column
                                (
                                    width=12,
                                    selectInput("t_3_tp_M2", "2nd Marker", choices = NULL, selected = NULL)
                                )
                            )
                        )
                    ),
                    column
                    (
                        width=9,
                        uiOutput("t_3_tp_body")
                        
                    )
                ),
                #===============================================================================================
                
                
                
                #Convert To References
                #===============================================================================================
                tabItem
                (
                    tabName="t_3_conv",
                    shinydashboard::box
                    (
                        width=12,title="File Options",solidHeader=T,status="info",
                        fluidRow
                        (
                            column
                            (
                                width=8,
                                selectInput("t_3_conv_ref_sel", "Reference File", choices = NULL, selected = NULL),
                                selectInput("t_3_conv_mfi_sel", "Generated Files (MFI Shift)", multiple = T, selected = NULL, choices = NULL),
                                selectInput("t_3_conv_tp_sel", "Generated Files (Time Points)", multiple = T, selected = NULL, choices = NULL)
                            ),
                            column
                            (
                                width=4,style="padding-top:1.7%",
                                actionButton("t_3_conv_validate", "Convert")
                            )
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                #Move Populations
                #===============================================================================================
                tabItem
                (
                    tabName="t_4_move",
                    shinydashboard::box
                    (
                        width=12,title="File Options",solidHeader=T,status="info",
                        column
                        (
                            width=9,
                            fluidRow
                            (
                                column
                                (
                                    width=6,
                                    selectInput("t_4_move_ref_sel", "Reference File", choices = NULL, selected = NULL)
                                ),
                                column
                                (
                                    width=6,
                                    selectInput("t_4_move_pop_sel", "Population", choices = NULL, selected = NULL)
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=6,
                                    selectInput("t_4_move_markers_sel", "Markers", choices = NULL, selected = NULL, multiple = T)
                                ),
                                column
                                (
                                    width=6,
                                    actionButton("t_4_move_validate", "Validate", width="100%")
                                )
                            )
                        )
                    ),
                    column
                    (
                        width=12,
                        uiOutput("t_4_move_body")
                        
                    )
                ),
                #===============================================================================================
                
                
                
                #Manage Populations
                #===============================================================================================
                tabItem
                (
                    tabName="t_4_manage",
                    column
                    (
                        width=3,
                        shinydashboard::box
                        (
                            title="File Options",solidHeader=T,status="info",width=12,
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_4_manage_ref_sel", "Reference File", choices = NULL, selected = NULL)
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    uiOutput("t_4_manage_ui_options")
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    actionButton("t_4_manage_validate", "Validate")
                                )
                            )
                        ),
                        shinydashboard::box
                        (
                            title="Visualization Options",solidHeader=T,status="info",width=12,
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_4_manage_m1", "1st Marker", choices = NULL, selected = NULL),
                                    selectInput("t_4_manage_m2", "2nd Marker", choices = NULL, selected = NULL),
                                    selectInput("t_4_manage_hp", "Highlighted populations", choices = NULL, selected = NULL, multiple = T)
                                )
                            )
                        )
                    ),
                    column
                    (
                        width=9,
                        shinydashboard::box
                        (
                            title="Delete Population",solidHeader=T,status="info",width=12,collapsible=T,
                            fluidRow
                            (
                                width=12,
                                column
                                (
                                    width=9,
                                    selectInput("t_4_manage_rm_sel", "Populations", choices = NULL)
                                ),
                                column
                                (
                                    width=3,style="padding-top:2%",
                                    actionButton("t_4_manage_rm", "Remove")
                                )
                            )
                        ),
                        shinydashboard::box
                        (
                            title="Add Population",solidHeader=T,status="info",width=12,collapsible=T,
                            fluidRow
                            (
                                width=12,
                                column
                                (
                                    width=5,
                                    selectInput("t_4_manage_add_sel", "Model Population", choices = NULL)
                                ),
                                column
                                (
                                    width=5,
                                    numericInput("t_4_manage_add_events", "Number of Events", min = 0, value = 100)
                                ),
                                column
                                (
                                    width=2,style="padding-top:2%",
                                    actionButton("t_4_manage_add", "Add")
                                )
                            )
                        ),
                        shinydashboard::box
                        (
                            title="Visualization",solidHeader=T,status="info",width=12,collapsible=T,
                            fluidRow
                            (
                                width=12,
                                column
                                (
                                    width=12,
                                    plotOutput("t_4_manage_plot")
                                )
                            )
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                #Change Population Size
                #===============================================================================================
                tabItem
                (
                    tabName="t_4_size",
                    column
                    (
                        width=3,
                        shinydashboard::box
                        (
                            title="File Options",solidHeader=T,status="info",width=12,
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_4_size_ref_sel", "Reference File", choices = NULL, selected = NULL)
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    uiOutput("t_4_size_ui_options")
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    actionButton("t_4_size_validate", "Validate")
                                )
                            )
                        ),
                        shinydashboard::box
                        (
                            title="Visualization Options",solidHeader=T,status="info",width=12,
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_4_size_m1", "1st Marker", choices = NULL, selected = NULL),
                                    selectInput("t_4_size_m2", "2nd Marker", choices = NULL, selected = NULL),
                                    selectInput("t_4_size_hp", "Highlighted populations", choices = NULL, selected = NULL, multiple = T)
                                )
                            )
                        )
                    ),
                    column
                    (
                        width=9,
                        shinydashboard::box
                        (
                            title="Change Size of Population",solidHeader=T,status="info",width=12,collapsible=T,
                            fluidRow
                            (
                                width=12,
                                column
                                (
                                    width=3,
                                    selectInput("t_4_size_pop_sel", "Populations", choices = NULL)
                                ),
                                column
                                (
                                    width=9,
                                    sliderInput("t_4_size_slider", "Relative size (%)", min = 0, max = 600, value = 100, step = 0.05)
                                )
                            ),
                            fluidRow
                            (
                                width=12,
                                column
                                (
                                    width=2,
                                    h4("Number of events: "),
                                    uiOutput("t_4_size_nmb_events")
                                )
                            )
                        ),
                        shinydashboard::box
                        (
                            title="Visualization",solidHeader=T,status="info",width=12,collapsible=T,
                            fluidRow
                            (
                                width=12,
                                column
                                (
                                    width=12,
                                    plotOutput("t_4_size_plot")
                                )
                            )
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                #Mix Populations from 2 Files
                #===============================================================================================
                tabItem
                (
                    tabName="t_5",
                    column
                    (
                        width=4,
                        shinydashboard::box
                        (
                            width=12,title="1st Reference File",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            selectInput("t_5_r1_sel", "Reference File", choices = NULL, selected = NULL),
                            selectInput("t_5_r1_pop_sel", "Populations to extract", choices = NULL, selected = NULL, multiple = T),
                            uiOutput("t_5_r1_pop_ui")
                        ),
                        shinydashboard::box
                        (
                            width=12,title="2nd Reference File",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            selectInput("t_5_r2_sel", "Reference File", choices = NULL, selected = NULL),
                            selectInput("t_5_r2_pop_sel", "Populations to extract", choices = NULL, selected = NULL, multiple = T),
                            uiOutput("t_5_r2_pop_ui")
                        ),
                        shinydashboard::box
                        (
                            width=12,title="File Options",solidHeader=T,status="info",style="max-height:17vh;overflow:auto",
                            column
                            (
                                width=12,
                                actionButton("t_5_generate", "Generate The File", width = "100%")
                            ),
                            column
                            (
                                width=12,
                                actionButton("t_5_validate", "Validate All Changes", width = "100%")
                            )
                        )
                    ),
                    column
                    (
                        width=8,
                        shinydashboard::box
                        (
                            width=12,title="Populations Summary",solidHeader=T,status="info",style="max-height:45vh;overflow:auto",
                            uiOutput("t_5_summary")
                        ),
                        shinydashboard::box
                        (
                            width=12,title="Visualization",solidHeader=T,status="info",
                            fluidRow
                            (
                                column
                                (
                                    width=6,
                                    selectInput("t_5_m1", "1st Marker", choices = NULL, selected = NULL)
                                    
                                ),
                                column
                                (
                                    width=6,
                                    selectInput("t_5_m2", "2nd Marker", choices = NULL, selected = NULL)
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=8,
                                    selectInput("t_5_hp", "Highlighted Populations", choices = NULL, selected = NULL, multiple=T)
                                    
                                )
                            ),
                            plotOutput("t_5_plot")
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                #VISUALIZATIONS (HEATMAPS)
                #===============================================================================================
                tabItem
                (
                    tabName="t_6_hm",
                    column
                    (
                        width=3,
                        shinydashboard::box
                        (
                            title="File Options",solidHeader=T,status="info",width=12,
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_6_hm_ref1_sel", "1st Reference File", choices = NULL, selected = NULL),
                                    selectInput("t_6_hm_ref2_sel", "2nd Reference File", choices = NULL, selected = NULL)
                                )
                            )
                        )
                    ),
                    column
                    (
                        width=9,
                        shinydashboard::box
                        (
                            title="Sunburst Plots",solidHeader=T,status="info",width=12,collapsible=T,
                            column
                            (
                                width=6,
                                sunburstOutput("t_6_hm_sb_1")
                            ),
                            column
                            (
                                width=6,
                                sunburstOutput("t_6_hm_sb_2")
                            )
                        ),
                        shinydashboard::box
                        (
                            title="Heatmaps",solidHeader=T,status="info",width=12,collapsible=T,
                            column
                            (
                                width=6,
                                plotlyOutput("t_6_hm_1", width = "100%")
                            ),
                            column
                            (
                                width=6,
                                plotlyOutput("t_6_hm_2", width = "100%")
                            )
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                
                #VISUALIZATIONS (JOYPLOTS)
                #===============================================================================================
                tabItem
                (
                    tabName="t_6_jp",
                    column
                    (
                        width=3,
                        shinydashboard::box
                        (
                            title="File Options",solidHeader=T,status="info",width=12,
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_6_jp_ref_sel", "Reference Files", choices = NULL, selected = NULL, multiple = T)
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    selectInput("t_6_jp_marker_sel", "Marker", choices = NULL, selected = NULL)
                                )
                            )
                        )
                    ),
                    column
                    (
                        width=9,
                        shinydashboard::box
                        (
                            title="Joyplot",solidHeader=T,status="info",width=12,collapsible=T,
                            plotOutput("t_6_jp", width = "100%")
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                
                #Decompensate and Detransform
                #===============================================================================================
                tabItem
                (
                    tabName="t_7",
                    column
                    (
                        width=4,
                        shinydashboard::box
                        (
                            width=12,title="Compensation",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            actionButton("t_7_decompensate", "Decompensate Selection", width="100%")
                        ),
                        shinydashboard::box
                        (
                            width=12,title="Transformation",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            selectInput("t_7_detransform_sel", "Transformation Type", choices=c("Logicle"=1, "Arcshinh"=2)),
                            uiOutput("t_7_detransform_param"),
                            actionButton("t_7_detransform", "Detransform Selection", width="100%")
                        )
                    ),
                    column
                    (
                        width=8,
                        shinydashboard::box
                        (
                            width=12,title="Reference Files",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            fluidRow
                            (
                                column
                                (
                                    width=2,
                                    checkboxInput("t_7_file_select_all", "Select All", value = F)
                                ),
                                column
                                (
                                    width=4,style="padding-top:1%",
                                    "Reference File"
                                ),
                                column
                                (
                                    width=3,style="padding-top:1%",
                                    "Compensated"
                                ),
                                column
                                (
                                    width=3,style="padding-top:1%",
                                    "Transformed"
                                )
                            ),
                            uiOutput("t_7_files")
                        )
                    )
                ),
                #===============================================================================================
                
                
                
                
                
                #Download
                #===============================================================================================
                tabItem
                (
                    tabName="t_8",
                    column
                    (
                        width=8,
                        shinydashboard::box
                        (
                            width=12,title="Select Files",solidHeader=T,status="info",style="overflow:auto",
                            fluidRow
                            (
                                column
                                (
                                    width=1,
                                    h4(tags$b("Select"))
                                ),
                                column
                                (
                                    width=9,
                                    h4(tags$b("File"))
                                ),
                                column
                                (
                                    width=2,
                                    h4(tags$b("Type"))
                                )
                            ),
                            uiOutput("t_8_files")
                        )
                    ),
                    column
                    (
                        width=4,
                        shinydashboard::box
                        (
                            width=12,title="Download Options",solidHeader=T,status="info",style="overflow:auto",
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    actionButton("t_8_select_all", "Select All", width = "100%")
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    actionButton("t_8_deselect_all", "Deselect All", width = "100%")
                                )
                            ),
                            fluidRow
                            (
                                column
                                (
                                    width=12,
                                    downloadButton("t_8_dl", "Download Selection", style = "width:100%")
                                )
                            )
                        )
                    )
                )
                #===============================================================================================
            )
        )
        
    )
    
)