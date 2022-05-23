#R6 model class
library(R6)


ModelEngine <- R6Class(
  "ModelEngine",
  public = list(
    model = NULL,
    #Variables
    start_date = NULL,
    ODE = NULL,
    comparts = NULL,
    params_list = NULL,
    wrapper = NULL,
    result = NULL,
    #extra
    model_rules = NULL,
    num_comparts_groups = NULL,
    comparts_names = NULL,
    
    #Initialization====
    initialize = function(model,
                          default_file = NA,
                          load_default = T) {
      #load function
      self$model <- model
      
      #load model rules
      try(private$import_model_rules()
          , options(warn = 2))
      
      #Import ODE
      private$import_ODE()
      
      #Import default values
      if (load_default) {
        self$load_default(file = default_file)
      }
      
      
      #Set compartiments from initial values
      self$comparts_names <-
        private$get_comparts_names(self$comparts)
    },
    
    
    #PUBLIC FUNCTIONS====================================
    #set time
    set_start_date = function(date) {
      stopifnot(class(date) == "Date")
      self$start_date <- date
      invisible(self)
    },
    
    set_comparts = function(comparts) {
      #Check if comparts are all numeric
      if (class(comparts) == "numeric") {
        comparts_names <- private$get_comparts_names(comparts)
        #Check number of comparts
        if (length(comparts_names) == length(self$model_rules$comparts)) {
          #Check names of comparts
          if (all(comparts_names %in% self$model_rules$comparts)) {
            comparts_table <- table(gsub("[[:digit:]]", "", names(comparts)))
            #Check compartment groups numbers
            if (all(sapply(comparts_table, function(x) {
              x == max(comparts_table)
            }))) {
              self$comparts <- comparts
              self$comparts_names <-
                comparts_names
              self$num_comparts_groups <-
                max(comparts_table)
            } else {
              stop("comparts not same group number")
            }
          } else {
            stop("comparts dont match ODE comparts")
          }
        } else {
          stop("comparts number not equal ODE comparts number")
        }
      } else {
        stop("comparts not numeric vector")
      }
    },
    set_initial_params = function(params) {
      #Check format
      if (class(params) == "list") {
        if (length(params) == 2 &
            class(params$timeline) == "numeric" &
            class(params$params) == "list") {
          #Check timelien
          if (length(params$timeline) > 0 &
              params$timeline[1] == 0) {
            #Check params names
            for (i in seq(1, length(params$params))) {
              sapply(names(params$params[[i]]), function(x) {
                if (!(x %in% self$model_rules$variables)) {
                  stop(paste0("$params[[", i, "]]$", x, " not it model rules"))
                }
              })
            }
            self$params_list <- params
            
          } else{
            stop("$timeline must start with 0")
          }
        } else {
          stop("initial_parameters must be a list of 2: numeric timeline and params list")
        }
      } else {
        stop("params not list")
      }
    },
    
    #Adds a new entry to the prams_list
    add_breakpoint = function(time, params) {
      if (is.list(self$params_list)) {
        #convert t to numeric if is.Date
        if (class(time) == "Date") {
          stopifnot(class(self$start_date) == "Date")
          time <-
            as.numeric(time - self$start_date)
        }
        if (is.numeric(time)) {
          #check if lower than tmax
          if (time > self$params_list$timeline[1] &
              time < self$params_list$timeline[length(self$params_list$timeline)]) {
            #if time already exist bk ir replaced
            if ((time %in% self$params_list$timeline)) {
              warning("replacing existing breaking point")
              place_bk <-
                which(self$params_list$timeline == time) - 1
              self$params_list$timeline <-
                self$params_list$timeline[self$params_list$timeline != time]
              self$params_list$params[[place_bk]] <-
                NULL
            }
            #add time of bk to timeline
            self$params_list$timeline <-
              sort(append(self$params_list$timeline, time))
            place_bk <-
              which(self$params_list$timeline == time) - 1
            
            #Check parametets
            private$check_params(params)
            
            #add list with parameters
            self$params_list$params <-
              append(self$params_list$params, list(params), place_bk)
          } else {
            stop("time outside timeline")
          }
        } else {
          stop("time not Data or numeric")
        }
      } else {
        stop("$params_list is NA")
      }
    },
    
    #Remove breakpoints by time, can be date or numeric and can be a vector also
    remove_breakpoint = function(time) {
      if (class(time) == "Date") {
        time <- as.numeric(time - self$startdate)
      } else if (!is.numeric(time)) {
        stop("time must be Date or numeric")
      }
      
      if (all(time %in% self$params_list$timeline)) {
        #index of the matching bk
        bk_index <-
          match(time, self$params_list$timeline)
        #remove from timeline and params the matching times
        self$params_list$params <-
          self$params_list$params[-match(time, self$params_list$timeline)]
        self$params_list$timeline <-
          self$params_list$timeline[-match(time, self$params_list$timeline)]
      } else {
        stop("no breakpoint(s) with such time")
      }
      invisible(self)
    },
    
    #Import wrapper function
    set_wrapper = function(wrapper) {
      if (is.character(wrapper)) {
        self$wrapper <- source(paste0("functions/", wrapper, ".R"))$value
      } else if (is.function(wrapper)) {
        self$wrapper <- wrapper
      } else {
        stop("wrapper must be character or function")
      }
      invisible(self)
    },
    
    #Load default values for time, comparts and params_list
    load_default = function(file = NA,
                            replace = F) {
      if (is.na(file)) {
        load(paste0("models/", self$model, "/defaults.RData"))
      } else if (is.character(file)) {
        load(file)
      } else {
        stop("load_default file must be class character")
      }
      
      if (isTRUE(replace)) {
        self$start_date <- time
        self$comparts <- comparts
        self$params_list <- params_list
        if (exists("wrapper")) {
          self$set_wrapper(wrapper)
        } else {
          self$wrapper <- NA
        }
      } else if (!isTRUE(replace)) {
        if (is.null(self$itme)) {
          self$start_date <- time
        }
        if (is.null(self$comparts)) {
          self$comparts <- comparts
        }
        if (is.null(self$params_list)) {
          self$params_list <- params_list
        }
        
        if (exists("wrapper") &
            is.null(self$wrapper)) {
          self$set_wrapper(wrapper)
        } else {
          self$wrapper <- NA
        }
      } else {
        stop("replace must be logical")
      }
      invisible(self)
    },
    
    #Load args
    set_args = function(start_date = NA,
                        comparts = NA,
                        params_list = NA,
                        wrapper = NA,
                        replace = F) {
      if (isTRUE(replace)) {
        #Warning if varaibles are to be replaced
        if (!is.null(self$start_date)) {
          warning("replacing start_date")
        }
        if (!is.null(self$comparts[[1]])) {
          warning("replacing comparts")
        }
        if (!is.null(self$params_list[[1]][1])) {
          warning("replacing params_list")
        }
        if (is.character(wrapper) |
            is.function(wrapper)) {
          warning("replacing wrapper")
        }
        
        if (!is.na(time)) {
          self$set_start_date(start_date)
        }
        if (!is.na(comparts[1])) {
          self$set_comparts(comparts)
        }
        if (!is.na(params_list[1])) {
          self$set_initial_params(params_list)
        }
        if (is.character(wrapper) |
            is.function(wrapper)) {
          self$set_wrapper(wrapper)
        }
        
      } else if (!isTRUE(replace)) {
        if (!is.na(start_date) &
            is.null(self$start_date)) {
          self$set_start_date(start_date)
        }
        if (!is.na(comparts[[1]]) &
            is.null(self$comparts[[1]])) {
          self$set_comparts(comparts)
        }
        if (!is.na(params_list[[1]][1]) &
            is.null(self$params_list[[1]][1])) {
          self$set_initial_params(params_list)
        }
        if ((is.character(wrapper) |
             is.function(wrapper)) &
            is.null(self$wrapper)) {
          self$set_wrapper(wrapper)
        }
      }
      invisible(self)
    },
    
    #Validation==========================================
    validate = function() {
      #NA inputs
      if (all(is.na(self$comparts))) {
        stop("$comparts missing")
      }
      if (all(is.na(self$params_list))) {
        stop("$params_list missing")
      }
      
      #check time
      if (!is.numeric(self$params_list$timeline) |
          length(self$params_list$timeline) <= 1 |
          is.unsorted(self$params_list$timeline)) {
        stop("time badly defined")
      }
      
      #Check ODE
      if (!is.function(self$ODE)) {
        if (is.na(self$ODE)) {
          stop("$ODE missing")
        } else {
          stop("$ODE not a function")
        }
      }
      
      #Check comparts
      if (class(self$comparts) != "numeric") {
        stop("$comparts class not numeric")
        #Check if corerct number of
      } else if (!all(self$comparts_names == self$model_rules$comparts)) {
        stop("$params_list$comparts_names not equal to $model_rules")
      }
      
      #Check parameters
      #Check data tipes
      if (!class(self$params_list) == "list") {
        stop("$params_list is not list")
      } else if (!class(self$params_list$timeline) ==
                 "numeric") {
        stop("$params_list$timeline is not numeric")
      } else if (!class(self$params_list$params) ==
                 "list") {
        stop("$params_list$params is not list")
      }
      #Check if timeline is in order
      if (is.unsorted(self$params_list$timeline)) {
        stop("$params_list$timeline not in order")
      }
      
      
      #Check if all parameters are provided
      for (i in seq(1, length(self$params_list$params))) {
        if (!all(names(self$params_list$params[[i]]) %in% self$model_rules$variables)) {
          stop("$params_list$params names not equal to $model_rules$variables")
        }
      }
      #Check if timeline and prams compatible
      if (length(self$params_list$timeline) - 1 != length(self$params_list$params)) {
        stop("$params_list$timeline and $params_list$params incompatible sizes")
      }
      #Check wrapper
      if (!is.function(self$wrapper)) {
        if (is.character(wrapper) | is.function(wrapper)) {
          stop("$wrapper is neither NA nor a function")
        } else if (length(self$comparts) != length(self$comparts_names)) {
          warning(
            "age groups detected but no wrapper found,\n to use age groups $set_wrapper(\"hetero_model_wrapper\")"
          )
        }
      }
      
      cat("input validated\n")
      invisible(self)
    },
    
    
    #MODEL RUN===========================================
    run = function() {
      #Load functions to run model
      source("functions/run_model.R")
      
      #Fill params not assigned in bk to previous params value
      if (length(self$params_list$timeline) > 2) {
        self$params_list <- private$add_unchanged_params(self$params_list)
      }
      
      #validate
      self$validate()
      
      #warning if $result already exits
      if (!is.null(self$result)) {
        warning("$result overwritten")
      }
      
      #RUN MODEL
      if (is.function(self$wrapper)) {
        res <-
          run_model(self$ODE, self$comparts, self$params_list, self$wrapper)
      } else {
        #print("NO WRAPPER")
        res <-
          run_model(self$ODE, self$comparts, self$params_list)
      }
      
      self$result <- res
      if (is.data.frame(res)) {
        if (length(res) > 1) {
          cat("Simulation completed with sucess,\nuse $result to acess the results table\n")
        } else {
          warning("$result length 1")
        }
      } else {
        warning("$result not a data.frame")
      }
      
      invisible(self)
    },
    
    #Joins the df result age groups
    result_compact = function() {
      res_comp <- self$result
      names(res_comp) <-
        gsub("[[:digit:]]", "", names(res_comp))
      
      res_comp <- as.data.frame(do.call(cbind,
                                        by(
                                          t(res_comp), INDICES = names(res_comp), FUN = colSums
                                        )))
      
      #order columns
      res_comp <-
        res_comp[, unique(gsub("[[:digit:]]", "", names(self$result)))]
      return(res_comp)
    },
    
    #Print
    print = function(extended = F) {
      stopifnot(is.logical(extended))
      cat(
        "R6 ModelEngine object==========\n",
        "model: ",
        self$model,
        "\n",
        "runned: ",
        !is.null(self$result),
        "\n"
      )
      if (is.function(self$wrapper)) {
        cat("\n wrapper loaded\n")
      }
      cat(
        "==========\n",
        "STATS:\n",
        "starting date: ",
        as.character(self$start_date),
        "\n",
        "timeline: ",
        self$params_list$timeline,
        "\n",
        "parameters: ",
        names(self$params_list$params[[1]]),
        "\n",
        "compartments types: ",
        self$comparts_names,
        "\n",
        "number of compartments: ",
        length(names(self$comparts)),
        "\n"
      )
      if (isTRUE(extended)) {
        cat("==========\n",
            "ARGS:\n",
            "comparts:\n")
        print(self$comparts)
        cat("\nparameters:\n")
        print(self$params_list)
        if (!is.null(self$result)) {
          cat("\nRESULT:\n")
          print(self$result)
        }
      }
      invisible(self)
    },
    
    
    #Plot
    plot = function(legend = T,
                    compact = T,
                    filter = NA,
                    show_breakpoints = T) {
      #Check if model has run
      if (!is.data.frame(self$result)) {
        stop("No $results to be ploted")
      }
      
      if (isTRUE(compact)) {
        res <- self$result_compact()
      } else {
        res <- self$result
      }
      
      #filter comparts
      if (is.character(filter)) {
        res <- res[, c(T, gsub("[[:digit:]]", "", names(res[, -1])) %in% filter)]
      }
      
      
      #if timeline date
      plot(
        x = res[[1]],
        y = res[[2]],
        type = "l",
        col = 2,
        ylab = "Individuas",
        xlab = "",
        xaxt = "n",
        ylim = c(0, max(res[-1]))
      )
      if (length(res) > 2) {
        for (i in seq(3, length(res))) {
          lines(res[[1]], y = res[[i]], col = i)
        }
      }
      
      
      #Breakpoints
      if (show_breakpoints == T) {
        par(xpd = FALSE)
        abline(
          v = c(self$params_list$timeline[3:length(self$params_list$timeline) - 1]),
          col = "red",
          lty = 2,
          lwd = 0.6
        )
      }
      
      #legends
      if (isTRUE(legend)) {
        par(mar = c(5, 4, 4, 8), xpd = TRUE)
        
        legend(
          "topright",
          inset = c(-0.16, 0),
          legend = names(res[2:length(res)]),
          col = seq(2, length(res)),
          cex = 0.6,
          pch = 1,
          y.intersp = 0.6
        )
      }
      
      if (class(self$start_date) == "Date") {
        time_days <-
          format(
            seq.Date(
              from = self$start_date,
              to = self$start_date + self$params_list$timeline[length(self$params_list$timeline)],
              by = "day"
            ),
            format("%Y-%m-%d")
          )
        month_start_days <-
          res$time[format(as.Date(time_days, "%Y-%m-%d"), format("%d")) == "01"]
        
        axis(1, at = month_start_days, labels = format(as.Date(time_days[month_start_days]), "%Y-%m"))
      } else {
        #Plot times, in numeric, according to size of timeline
        axis(1, res$time[(seq(res$time[1], res$time[length(res$time)], by = length(res$time) %/%
                                10)) + 1])
      }
    }
  ),
  
  
  #________________________________________________________
  #PRIVATE=================================================
  #________________________________________________________
  private = list(
    #Import the model equations from model folder (it must be a .R file with the model folder name and "_model" after)
    import_ODE = function() {
      self$ODE <-
        source(paste0("models/", self$model, "/", self$model, "_model.R"))$value
      invisible(self)
    },
    #upload model rules
    import_model_rules = function() {
      load(paste0("models/", self$model, "/model_rules.RData"))
      self$model_rules <- model_rules
      invisible(self)
    },
    #Removes numbers from compartments names
    get_comparts_names = function(vars) {
      return(unique(gsub("[[:digit:]]", "", names(vars))))
    },
    #Check params single
    check_params = function(params) {
      if (class(params) != "list") {
        stop("$params not list")
      } else if (!(all(names(params) %in% self$model_rules$variables))) {
        stop("$params element not in model rules")
      }
      return(1)
    },
    check_params_list = function(vars_list) {
      if (class(vars_list) == "list") {
        if (is.numeric(vars_list$timeline) &
            class(vars_list$params) == "list") {
          #Check timelien
          if (length(vars_list$timeline) > 0 &
              vars_list$timeline[1] == 0) {
            #Check params names
            for (i in seq(1, length(vars_list$params))) {
              return(all(
                lapply(vars_list$params[[i]], private$check_params) == 1
              ))
            }
          } else{
            stop("$timeline must start with 0")
          }
        } else {
          stop("initial_parameters must be a list of 2: numeric timeline and params list")
        }
      } else {
        stop("params not list")
      }
    },
    #Function add params not added in a breakpoint by looking at the previous period
    add_unchanged_params = function(params_list) {
      for (i in seq(2, length(params_list$params))) {
        if (length(params_list$params[[i]]) != length(params_list$params[[1]])) {
          params_list$params[[i]] <-
            append(params_list$params[[i]], (params_list$params[[i - 1]][!(names(params_list$params[[i -
                                                                                                       1]]) %in% names(params_list$params[[i]]))]))
        }
      }
      return(params_list)
    }
    
    
  )
)
