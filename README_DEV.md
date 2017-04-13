exception handling guide:
ghc -prof -fprof-auto -fprof-auto-calls Main.hs -o main
./main +RTS -xc

AST processing pipeline:

SyntaxAst
---[Syntax.prepare]--->
UAst Hi
---[TypeSystem.infer]--->
UAst Lo
---[Semantics.Typecheck.typecheck]--->
Poly (Ast Hi '[])
---[Semantics.Eval.polyast_to_monoast]
Mono (Ast Hi '[])
---[Semantics.Records.resolve_field_lookups]--->
Mono (Ast Lo '[])
---[Semantics.Eval.eval_monoast]--->
A Type a => a
