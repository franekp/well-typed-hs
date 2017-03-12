{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Letters where
import Base.Pervasives

data A_
data B_
data C_
data D_
data E_
data F_
data G_
data H_
data I_
data J_
data K_
data L_
data M_
data N_
data O_
data P_
data Q_
data R_
data S_
data T_
data U_
data V_
data W_
data X_
data Y_
data Z_

data Cap :: * -> *

type instance T Letter = Letter

data Letter :: * -> * where
  A_UL :: Letter (Cap A_)
  B_UL :: Letter (Cap B_)
  C_UL :: Letter (Cap C_)
  D_UL :: Letter (Cap D_)
  E_UL :: Letter (Cap E_)
  F_UL :: Letter (Cap F_)
  G_UL :: Letter (Cap G_)
  H_UL :: Letter (Cap H_)
  I_UL :: Letter (Cap I_)
  J_UL :: Letter (Cap J_)
  K_UL :: Letter (Cap K_)
  L_UL :: Letter (Cap L_)
  M_UL :: Letter (Cap M_)
  N_UL :: Letter (Cap N_)
  O_UL :: Letter (Cap O_)
  P_UL :: Letter (Cap P_)
  Q_UL :: Letter (Cap Q_)
  R_UL :: Letter (Cap R_)
  S_UL :: Letter (Cap S_)
  T_UL :: Letter (Cap T_)
  U_UL :: Letter (Cap U_)
  V_UL :: Letter (Cap V_)
  W_UL :: Letter (Cap W_)
  X_UL :: Letter (Cap X_)
  Y_UL :: Letter (Cap Y_)
  Z_UL :: Letter (Cap Z_)

  A_LL :: Letter A_
  B_LL :: Letter B_
  C_LL :: Letter C_
  D_LL :: Letter D_
  E_LL :: Letter E_
  F_LL :: Letter F_
  G_LL :: Letter G_
  H_LL :: Letter H_
  I_LL :: Letter I_
  J_LL :: Letter J_
  K_LL :: Letter K_
  L_LL :: Letter L_
  M_LL :: Letter M_
  N_LL :: Letter N_
  O_LL :: Letter O_
  P_LL :: Letter P_
  Q_LL :: Letter Q_
  R_LL :: Letter R_
  S_LL :: Letter S_
  T_LL :: Letter T_
  U_LL :: Letter U_
  V_LL :: Letter V_
  W_LL :: Letter W_
  X_LL :: Letter X_
  Y_LL :: Letter Y_
  Z_LL :: Letter Z_
