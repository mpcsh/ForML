structure SurfaceProgram: sig

  type program = {
    classes: Class.class list,
    instances: Instance.instance list,
    body: AST.term
  }

end = struct

  type program = {
    classes: Class.class list,
    instances: Instance.instance list,
    body: AST.term
  }

  (* fun verifyInstances *) 

end
