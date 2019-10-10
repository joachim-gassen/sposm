test_items <- c(
  "Interpreter", "Compiler", "Breakpoint", "Shell (not the animal)", 
  "Debugger", "Version Control System", "Race condition", "RDBMS", 
  "Docker container", "API", "Call stack", "Exception", "Scope"
)

vars <- c(
  "time" = "integer", 
  "language_1" = "text", "language_2" = "text", "language_3" = "text",
  "usability_1" = "integer", "usability_2" = "integer", "usability_3" = "integer",
  "ease_1" = "integer", "ease_2" = "integer", "ease_3" = "integer",
  "overall" = "integer"
)

vars <- c(vars, rep("integer", length(test_items)))
names(vars)[12:(11 + length(test_items))]  <- 
  tolower(str_replace_all(test_items, "[^[:alnum:]]", ""))
vars <- c(vars, "learning_objectives" = "text", "name" = "text")
