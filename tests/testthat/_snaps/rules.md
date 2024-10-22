# rule printed correctly

    Code
      rule(a = "1", b = NA)
    Output
      Mapping of:
      a  <-  "1" 
      b  <-  <NA> 
      Convert to <NA>: "" 
      Convert to factor: TRUE 
      Drop unused level: FALSE 
      NA-replacing level in last position: TRUE 

# rule with multiple matching printed correctly

    Code
      rule(a = "1", b = c(NA, "b"), x = c("first", "second"), .to_NA = c("",
        "missing"))
    Output
      Mapping of:
      a  <-  "1" 
      b  <-  <NA>, "b" 
      x  <-  "first", "second" 
      Convert to <NA>: "", "missing" 
      Convert to factor: TRUE 
      Drop unused level: FALSE 
      NA-replacing level in last position: TRUE 

