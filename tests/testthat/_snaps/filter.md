# log_filter.list works as expected

    Code
      out
    Output
      [1] "Filter Log:"                                             
      [2] "  - iris:"                                               
      [3] "  Sepal filter min: Sepal.Width >= 2 [150 --> 150 rows.]"

---

    Code
      out2
    Output
      [1] "Filter Log:"                                              
      [2] "  - iris:"                                                
      [3] "  Sepal filter min: Sepal.Width >= 2 [150 --> 150 rows.] "
      [4] "  Sepal filter max: Sepal.Width < 4 [150 --> 146 rows.]"  

