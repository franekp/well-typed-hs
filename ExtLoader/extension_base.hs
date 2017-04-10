{-# LANGUAGE
  EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving,
  DeriveDataTypeable, ScopedTypeVariables, FunctionalDependencies,
  FlexibleInstances, FlexibleContexts, ExistentialQuantification,
  UndecidableInstances, TypeFamilies, EmptyCase, TypeOperators, KindSignatures,
  LambdaCase, DataKinds, ConstraintKinds, PolyKinds #-}

#define _COUNT_ARGS(...) _COUNT_ARGS_(__VA_ARGS__, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _COUNT_ARGS_(_19, _18, _17, _16, _15, _14, _13, _12, _11, _10, _9, _8, _7, _6, _5, _4, _3, _2, _1, N, ...) N

#define _CAT(A, B) _CAT_(A, B)
#define _CAT_(A, B) A##B

#define BEGIN_FORALL_STEP(a) ForallP 0 (ExistsPoly $

#define BEGIN_FORALL_0()
#define BEGIN_FORALL_1(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_0(__VA_ARGS__)
#define BEGIN_FORALL_2(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_1(__VA_ARGS__)
#define BEGIN_FORALL_3(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_2(__VA_ARGS__)
#define BEGIN_FORALL_4(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_3(__VA_ARGS__)
#define BEGIN_FORALL_5(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_4(__VA_ARGS__)
#define BEGIN_FORALL_6(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_5(__VA_ARGS__)
#define BEGIN_FORALL_7(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_6(__VA_ARGS__)
#define BEGIN_FORALL_8(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_7(__VA_ARGS__)
#define BEGIN_FORALL_9(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_8(__VA_ARGS__)
#define BEGIN_FORALL_10(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_9(__VA_ARGS__)
#define BEGIN_FORALL_11(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_10(__VA_ARGS__)
#define BEGIN_FORALL_12(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_11(__VA_ARGS__)
#define BEGIN_FORALL_13(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_12(__VA_ARGS__)
#define BEGIN_FORALL_14(V, ...) BEGIN_FORALL_STEP(V) BEGIN_FORALL_13(__VA_ARGS__)

#define BEGIN_FORALL(...) _CAT(BEGIN_FORALL_, _COUNT_ARGS(__VA_ARGS__)) (__VA_ARGS__)

#define END_FORALL_STEP(a) :: forall a. A Type a => ExistsPoly Builtin a)

#define END_FORALL_0(...)
#define END_FORALL_1(V, ...) END_FORALL_STEP(V) END_FORALL_0(__VA_ARGS__)
#define END_FORALL_2(V, ...) END_FORALL_STEP(V) END_FORALL_1(__VA_ARGS__)
#define END_FORALL_3(V, ...) END_FORALL_STEP(V) END_FORALL_2(__VA_ARGS__)
#define END_FORALL_4(V, ...) END_FORALL_STEP(V) END_FORALL_3(__VA_ARGS__)
#define END_FORALL_5(V, ...) END_FORALL_STEP(V) END_FORALL_4(__VA_ARGS__)
#define END_FORALL_6(V, ...) END_FORALL_STEP(V) END_FORALL_5(__VA_ARGS__)
#define END_FORALL_7(V, ...) END_FORALL_STEP(V) END_FORALL_6(__VA_ARGS__)
#define END_FORALL_8(V, ...) END_FORALL_STEP(V) END_FORALL_7(__VA_ARGS__)
#define END_FORALL_9(V, ...) END_FORALL_STEP(V) END_FORALL_8(__VA_ARGS__)
#define END_FORALL_10(V, ...) END_FORALL_STEP(V) END_FORALL_9(__VA_ARGS__)
#define END_FORALL_11(V, ...) END_FORALL_STEP(V) END_FORALL_10(__VA_ARGS__)
#define END_FORALL_12(V, ...) END_FORALL_STEP(V) END_FORALL_11(__VA_ARGS__)
#define END_FORALL_13(V, ...) END_FORALL_STEP(V) END_FORALL_12(__VA_ARGS__)
#define END_FORALL_14(V, ...) END_FORALL_STEP(V) END_FORALL_13(__VA_ARGS__)

#define END_FORALL(...) _CAT(END_FORALL_, _COUNT_ARGS(__VA_ARGS__)) (__VA_ARGS__)

#define BEGIN_NONE
#define END_NONE

#define EXPORT(name, quant, def) (name, BEGIN_##quant (MonoP $ Mono $ Builtin (def)) END_##quant)
