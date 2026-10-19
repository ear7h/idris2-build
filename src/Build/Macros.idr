module Build.Macros


import public Language.Reflection

%language ElabReflection

%macro
export
embed : (name : String) -> Elab String
embed name = do
  mbFile <- readFile CurrentModuleDir name
  case mbFile of
       Just x => pure x
       Nothing => fail "file `\{name}` not found"
