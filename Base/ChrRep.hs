{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.ChrRep where
import Base.Pervasives
import Base.ChrSym

type instance T ChrRep = ChrRep

data ChrRep :: Chr -> * where
  A_UL :: ChrRep CapA_
  B_UL :: ChrRep CapB_
  C_UL :: ChrRep CapC_
  D_UL :: ChrRep CapD_
  E_UL :: ChrRep CapE_
  F_UL :: ChrRep CapF_
  G_UL :: ChrRep CapG_
  H_UL :: ChrRep CapH_
  I_UL :: ChrRep CapI_
  J_UL :: ChrRep CapJ_
  K_UL :: ChrRep CapK_
  L_UL :: ChrRep CapL_
  M_UL :: ChrRep CapM_
  N_UL :: ChrRep CapN_
  O_UL :: ChrRep CapO_
  P_UL :: ChrRep CapP_
  Q_UL :: ChrRep CapQ_
  R_UL :: ChrRep CapR_
  S_UL :: ChrRep CapS_
  T_UL :: ChrRep CapT_
  U_UL :: ChrRep CapU_
  V_UL :: ChrRep CapV_
  W_UL :: ChrRep CapW_
  X_UL :: ChrRep CapX_
  Y_UL :: ChrRep CapY_
  Z_UL :: ChrRep CapZ_

  A_LL :: ChrRep A_
  B_LL :: ChrRep B_
  C_LL :: ChrRep C_
  D_LL :: ChrRep D_
  E_LL :: ChrRep E_
  F_LL :: ChrRep F_
  G_LL :: ChrRep G_
  H_LL :: ChrRep H_
  I_LL :: ChrRep I_
  J_LL :: ChrRep J_
  K_LL :: ChrRep K_
  L_LL :: ChrRep L_
  M_LL :: ChrRep M_
  N_LL :: ChrRep N_
  O_LL :: ChrRep O_
  P_LL :: ChrRep P_
  Q_LL :: ChrRep Q_
  R_LL :: ChrRep R_
  S_LL :: ChrRep S_
  T_LL :: ChrRep T_
  U_LL :: ChrRep U_
  V_LL :: ChrRep V_
  W_LL :: ChrRep W_
  X_LL :: ChrRep X_
  Y_LL :: ChrRep Y_
  Z_LL :: ChrRep Z_
