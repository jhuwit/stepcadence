# minute-level data for one unit, with optional extra columns
make_steps = function(steps, ...) {
  data.frame(time = seq_along(steps), steps = steps, ...)
}

one_day = function() {
  simulated_nhanes_steps[simulated_nhanes_steps$SEQN == "ID_1" &
                           simulated_nhanes_steps$PAXDAYM == 1, ]
}
