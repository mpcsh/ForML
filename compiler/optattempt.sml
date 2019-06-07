structure OptAttempt = struct

  datatype 'a optattempt
    = Success of 'a
    | Terminal of 'a
    | Failure of string

end
