## ROI plugin: GLPK
## based on Rglpk interface

## BASIC SOLVER METHOD
solve_OP <- function( x, control ){
    if(is.null(control))
        control <- list()
    ## ROI has its own translation table, thus we do not need to canonicalize in Rglpk
    control$canonicalize_status = FALSE
    if( all(ROI::types(x) == "C") )
        out <- .solve_LP( x, control )
    else
        out <- .solve_MILP( x, control )
    out
}

## SOLVER SUBMETHODS
.solve_LP <- function( x, control ) {
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    out <- Rglpk_solve_LP( terms(objective(x))[["L"]],
                           constraints(x)$L,
                           constraints(x)$dir,
                           constraints(x)$rhs,
                           bounds = bounds(x),
                           max = x$maximum,
                           control = control)
    ## FIXME: keep oiginal solution (return value)
    ROI_plugin_canonicalize_solution( solution = out$solution,
                                       optimum = out$optimum,
                                       status = out$status,
                                       solver = solver,
                                       message = out)
}

.solve_MILP <- function( x, control ) {
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    out <- Rglpk_solve_LP( terms(objective(x))[["L"]],
                           constraints(x)$L,
                           constraints(x)$dir,
                           constraints(x)$rhs,
                           bounds = bounds(x),
                           types = types(x),
                           max = x$maximum,
                           control = control)
    ROI_plugin_canonicalize_solution( solution = out$solution,
                                       optimum = out$optimum,
                                       status = out$status,
                                       solver = solver,
                                       message = out)
}

## STATUS CODES
.add_status_codes <- function(){
    ## GLPK
    ## from GLPK 4.34 reference manual and glpk.h (symbol, code, message)
    ## FIXME: change in solver interface, canonicalization now done in ROI
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ROI_plugin_add_status_code_to_db(solver,
                                      1L,
                                      "GLP_UNDEF",
                                      "Solution is undefined."
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      2L,
                                      "GLP_FEAS",
                                      "Solution is feasible."
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      3L,
                                      "GLP_INFEAS",
                                      "Solution is infeasible."
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      4L,
                                      "GLP_NOFEAS",
                                      "No feasible solution exists."
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      5L,
                                      "GLP_OPT",
                                      "Solution is optimal.",
                                      0L
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      6L,
                                      "GLP_UNBND",
                                      "Solution is unbounded."
                                      )
    invisible(TRUE)
}

## SOLVER CONTROLS
.add_controls <- function(){
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ## GLPK
    ROI_plugin_register_solver_control( solver,
                                        "verbose",
                                        "verbose" )
    ROI_plugin_register_solver_control( solver,
                                        "presolve",
                                        "presolve" )
    ROI_plugin_register_solver_control( solver,
                                        "tm_limit",
                                        "max_time" )
    invisible( TRUE )
}

## SOLUTION EXTRACTORS
ROI_plugin_solution_dual.glpk_solution <- function( x ){
    x$message$solution_dual
}

## FIXME: final structure of return value not yet decided
ROI_plugin_solution_aux.glpk_solution <- function ( x ){
    x$message$auxiliary
}


read_lp_mps_fixed <- function(file, ...) {
    a <- list(...)
    x <- Rglpk::Rglpk_read_file(file, type = "MPS_fixed", 
                                isTRUE(a$ignore_first_row), isTRUE(a$verbose))
    as.OP.MILP(x)
}

read_lp_mps_free <- function(file, ...) {
    a <- list(...)
    x <- Rglpk::Rglpk_read_file(file, type = "MPS_free",
                                isTRUE(a$ignore_first_row), isTRUE(a$verbose))
    as.OP.MILP(x)
}

read_lp_cplex_lp <- function(file, ...) {
    a <- list(...)
    x <- Rglpk::Rglpk_read_file(file, type = "CPLEX_LP",
                                isTRUE(a$ignore_first_row), isTRUE(a$verbose))
    as.OP.MILP(x)
}

read_lp_math_prog <- function(file, ...) {
    a <- list(...)
    x <- Rglpk::Rglpk_read_file(file, type = "MathProg",
                                isTRUE(a$ignore_first_row), isTRUE(a$verbose))
    as.OP.MILP(x)
}


write_lp <- function(x, file, type, ...) {
    var_types <- if ( all(types(x) == "C") ) NULL else types(x)
    Rglpk_call <- getNamespace("Rglpk")$Rglpk_call
    out <- Rglpk_call( obj = terms(objective(x))[["L"]], 
                       mat = constraints(x)$L, 
                       dir = constraints(x)$dir, 
                       rhs = constraints(x)$rhs, 
                       bounds = bounds(x), 
                       types = var_types, 
                       max = maximum(x), 
                       canonicalize_status = logical(1L), 
                       presolve = logical(1L), time_limit = integer(1L), 
                       verb = logical(1L), ## default values should be ignored
                       file = file, file_type = type )
    invisible( out$status )
}


write_lp_mps_fixed <- function(x, file, ...) {
    write_lp(x, file, type = 1L, ...)
}

write_lp_mps_free <- function(x, file, ...) {
    write_lp(x, file, type = 2L, ...)   
}

write_lp_cplex_lp <- function(x, file, ...) {
    write_lp(x, file, type = 3L, ...)
}

write_lp_math_prog <- function(x, file, ...) {
    write_lp(x, file, type = 4L, ...)
}

as.OP.MILP <- function(x){
    p <- OP()
    ## objective
    obj <- c(x$objective)
    names(obj) <- attr(x, "objective_vars_names")
    objective(p) <- obj

    ## constraints
    constraints(p) <- L_constraint(L = x$constraints[[1]], 
                                   dir = x$constraints[[2]], 
                                   rhs = x$constraints[[3]],
                                   names = attr(x, "constraint_names"))
    
    ## bounds
    bounds(p) <- V_bound( li = x$bounds$lower$ind,
                          ui = x$bounds$upper$ind,
                          lb = x$bounds$lower$val,
                          ub = x$bounds$upper$val,
                          nobj = attr(x,"n_objective_vars") )

    ## types
    types(p) <- x$types

    ## maximum
    maximum(p) <- x$maximum

    p
}


