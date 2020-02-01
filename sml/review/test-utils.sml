fun allTrue bs =
    case bs of 
        [] => true 
      | b::bs' => b andalso allTrue(bs') 