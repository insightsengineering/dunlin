# rule printed correctly

    Code
      rule(a = 1, b = NA)
    Output
      Mapping of:
      a  <-  1 
      b  <-  NA 

# emtpy_rule printed correctly

    Code
      empty_rule
    Output
      Empty mapping

# rules printed correctly

    Code
      rules(a = rule(a = 1), b = rule(a = 2))
    Output
      rule  a 
      Mapping of:
      a  <-  1 
      rule  b 
      Mapping of:
      a  <-  2 

