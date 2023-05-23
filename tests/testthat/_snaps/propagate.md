# propagate.list works as expected

    Code
      res
    Output
      $df1
          id  id2 val
      1    a   f1   a
      2    b   f2   b
      3 <NA> <NA>   c
      4    a <NA>   d
      5    k   f1   e
      6    x   f1   f
      
      $df2
          id  id2 num val
      1    a   f1   1   a
      2    b   f2   2   b
      3 <NA> <NA>   3   c
      4    a <NA>   4   d
      5    k   f1   5   e
      6    x   f1   6   f
      
      $df3
        x
      1 x
      

# propagate.list works as expected in safe mode

    Code
      res
    Output
      $df1
          id  id2 val
      1    a   f1   a
      2    a   f1   b
      3 <NA> <NA>   c
      4    a <NA>   d
      5    k   f1   e
      6    x   f1   f
      
      $df2
          id  id2 num  val
      1    a   f1   1    a
      2    a   f1   1    b
      3    b   f2   2 <NA>
      4 <NA> <NA>   3    c
      5    a <NA>   4    d
      6    k   f1   5    e
      7    x   f1   6    f
      

