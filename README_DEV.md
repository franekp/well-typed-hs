exception handling guide:
ghc -prof -fprof-auto -fprof-auto-calls Main.hs -o main
./main +RTS -xc

AST processing pipeline:

SyntaxAst
---[Syntax.prepare]--->
UAst
---[TypeSystem.infer]--->
UAst
---[Semantics.Typecheck.typecheck]--->
Poly (Ast Hi '[])
---[Semantics.Eval.polyast_to_monoast]
Mono (Ast Hi '[])
---[Semantics.Records.resolve_field_lookups]--->
Mono (Ast Lo '[])
---[Semantics.Eval.eval_monoast]--->
A Type a => a


TODO:
 - when a generic function (show) is applied to an ill-typed expression, program
   just assumes it's type is Void and ignores this error
