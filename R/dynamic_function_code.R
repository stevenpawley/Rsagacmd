func_reference <- "
  args = as.list(current_env())
  lib = expr_text(lib)
  tool = expr_text(tool)
  
  if ('intern' %in% names(args))
      args = args[-which(names(args) == 'intern')]
  
  if ('all_outputs' %in% names(args))
      args = args[-which(names(args) == 'all_outputs')]
  
  saga_results = saga_execute(
    lib = lib,
    tool = tool,
    senv = senv,
    intern = intern,
    all_outputs = all_outputs,
    args
  )
  
  return (saga_results)
  "