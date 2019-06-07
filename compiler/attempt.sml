structure Attempt = struct

  datatype 'a attempt
    = Success of 'a
    | Failure of string

end
