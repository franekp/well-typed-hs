{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.ChrImpl where
import Base.Chr
import Base.Pervasives

instance A Letter (Cap A_) where
  anything = A_UL
instance A Letter (Cap B_) where
  anything = B_UL
instance A Letter (Cap C_) where
  anything = C_UL
instance A Letter (Cap D_) where
  anything = D_UL
instance A Letter (Cap E_) where
  anything = E_UL
instance A Letter (Cap F_) where
  anything = F_UL
instance A Letter (Cap G_) where
  anything = G_UL
instance A Letter (Cap H_) where
  anything = H_UL
instance A Letter (Cap I_) where
  anything = I_UL
instance A Letter (Cap J_) where
  anything = J_UL
instance A Letter (Cap K_) where
  anything = K_UL
instance A Letter (Cap L_) where
  anything = L_UL
instance A Letter (Cap M_) where
  anything = M_UL
instance A Letter (Cap N_) where
  anything = N_UL
instance A Letter (Cap O_) where
  anything = O_UL
instance A Letter (Cap P_) where
  anything = P_UL
instance A Letter (Cap Q_) where
  anything = Q_UL
instance A Letter (Cap R_) where
  anything = R_UL
instance A Letter (Cap S_) where
  anything = S_UL
instance A Letter (Cap T_) where
  anything = T_UL
instance A Letter (Cap U_) where
  anything = U_UL
instance A Letter (Cap V_) where
  anything = V_UL
instance A Letter (Cap W_) where
  anything = W_UL
instance A Letter (Cap X_) where
  anything = X_UL
instance A Letter (Cap Y_) where
  anything = Y_UL
instance A Letter (Cap Z_) where
  anything = Z_UL

instance A Letter A_ where
  anything = A_LL
instance A Letter B_ where
  anything = B_LL
instance A Letter C_ where
  anything = C_LL
instance A Letter D_ where
  anything = D_LL
instance A Letter E_ where
  anything = E_LL
instance A Letter F_ where
  anything = F_LL
instance A Letter G_ where
  anything = G_LL
instance A Letter H_ where
  anything = H_LL
instance A Letter I_ where
  anything = I_LL
instance A Letter J_ where
  anything = J_LL
instance A Letter K_ where
  anything = K_LL
instance A Letter L_ where
  anything = L_LL
instance A Letter M_ where
  anything = M_LL
instance A Letter N_ where
  anything = N_LL
instance A Letter O_ where
  anything = O_LL
instance A Letter P_ where
  anything = P_LL
instance A Letter Q_ where
  anything = Q_LL
instance A Letter R_ where
  anything = R_LL
instance A Letter S_ where
  anything = S_LL
instance A Letter T_ where
  anything = T_LL
instance A Letter U_ where
  anything = U_LL
instance A Letter V_ where
  anything = V_LL
instance A Letter W_ where
  anything = W_LL
instance A Letter X_ where
  anything = X_LL
instance A Letter Y_ where
  anything = Y_LL
instance A Letter Z_ where
  anything = Z_LL

instance Show (Letter a) where
  show A_UL = "A"
  show B_UL = "B"
  show C_UL = "C"
  show D_UL = "D"
  show E_UL = "E"
  show F_UL = "F"
  show G_UL = "G"
  show H_UL = "H"
  show I_UL = "I"
  show J_UL = "J"
  show K_UL = "K"
  show L_UL = "L"
  show M_UL = "M"
  show N_UL = "N"
  show O_UL = "O"
  show P_UL = "P"
  show Q_UL = "Q"
  show R_UL = "R"
  show S_UL = "S"
  show T_UL = "T"
  show U_UL = "U"
  show V_UL = "V"
  show W_UL = "W"
  show X_UL = "X"
  show Y_UL = "Y"
  show Z_UL = "Z"

  show A_LL = "a"
  show B_LL = "b"
  show C_LL = "c"
  show D_LL = "d"
  show E_LL = "e"
  show F_LL = "f"
  show G_LL = "g"
  show H_LL = "h"
  show I_LL = "i"
  show J_LL = "j"
  show K_LL = "k"
  show L_LL = "l"
  show M_LL = "m"
  show N_LL = "n"
  show O_LL = "o"
  show P_LL = "p"
  show Q_LL = "q"
  show R_LL = "r"
  show S_LL = "s"
  show T_LL = "t"
  show U_LL = "u"
  show V_LL = "v"
  show W_LL = "w"
  show X_LL = "x"
  show Y_LL = "y"
  show Z_LL = "z"

instance Show (Mono Letter) where
  show (Mono a) = show a

instance Read (Mono Letter) where
  readsPrec _ ('A':rest) = [(Mono A_UL, rest)]
  readsPrec _ ('B':rest) = [(Mono B_UL, rest)]
  readsPrec _ ('C':rest) = [(Mono C_UL, rest)]
  readsPrec _ ('D':rest) = [(Mono D_UL, rest)]
  readsPrec _ ('E':rest) = [(Mono E_UL, rest)]
  readsPrec _ ('F':rest) = [(Mono F_UL, rest)]
  readsPrec _ ('G':rest) = [(Mono G_UL, rest)]
  readsPrec _ ('H':rest) = [(Mono H_UL, rest)]
  readsPrec _ ('I':rest) = [(Mono I_UL, rest)]
  readsPrec _ ('J':rest) = [(Mono J_UL, rest)]
  readsPrec _ ('K':rest) = [(Mono K_UL, rest)]
  readsPrec _ ('L':rest) = [(Mono L_UL, rest)]
  readsPrec _ ('M':rest) = [(Mono M_UL, rest)]
  readsPrec _ ('N':rest) = [(Mono N_UL, rest)]
  readsPrec _ ('O':rest) = [(Mono O_UL, rest)]
  readsPrec _ ('P':rest) = [(Mono P_UL, rest)]
  readsPrec _ ('Q':rest) = [(Mono Q_UL, rest)]
  readsPrec _ ('R':rest) = [(Mono R_UL, rest)]
  readsPrec _ ('S':rest) = [(Mono S_UL, rest)]
  readsPrec _ ('T':rest) = [(Mono T_UL, rest)]
  readsPrec _ ('U':rest) = [(Mono U_UL, rest)]
  readsPrec _ ('V':rest) = [(Mono V_UL, rest)]
  readsPrec _ ('W':rest) = [(Mono W_UL, rest)]
  readsPrec _ ('X':rest) = [(Mono X_UL, rest)]
  readsPrec _ ('Y':rest) = [(Mono Y_UL, rest)]
  readsPrec _ ('Z':rest) = [(Mono Z_UL, rest)]

  readsPrec _ ('a':rest) = [(Mono A_LL, rest)]
  readsPrec _ ('b':rest) = [(Mono B_LL, rest)]
  readsPrec _ ('c':rest) = [(Mono C_LL, rest)]
  readsPrec _ ('d':rest) = [(Mono D_LL, rest)]
  readsPrec _ ('e':rest) = [(Mono E_LL, rest)]
  readsPrec _ ('f':rest) = [(Mono F_LL, rest)]
  readsPrec _ ('g':rest) = [(Mono G_LL, rest)]
  readsPrec _ ('h':rest) = [(Mono H_LL, rest)]
  readsPrec _ ('i':rest) = [(Mono I_LL, rest)]
  readsPrec _ ('j':rest) = [(Mono J_LL, rest)]
  readsPrec _ ('k':rest) = [(Mono K_LL, rest)]
  readsPrec _ ('l':rest) = [(Mono L_LL, rest)]
  readsPrec _ ('m':rest) = [(Mono M_LL, rest)]
  readsPrec _ ('n':rest) = [(Mono N_LL, rest)]
  readsPrec _ ('o':rest) = [(Mono O_LL, rest)]
  readsPrec _ ('p':rest) = [(Mono P_LL, rest)]
  readsPrec _ ('q':rest) = [(Mono Q_LL, rest)]
  readsPrec _ ('r':rest) = [(Mono R_LL, rest)]
  readsPrec _ ('s':rest) = [(Mono S_LL, rest)]
  readsPrec _ ('t':rest) = [(Mono T_LL, rest)]
  readsPrec _ ('u':rest) = [(Mono U_LL, rest)]
  readsPrec _ ('v':rest) = [(Mono V_LL, rest)]
  readsPrec _ ('w':rest) = [(Mono W_LL, rest)]
  readsPrec _ ('x':rest) = [(Mono X_LL, rest)]
  readsPrec _ ('y':rest) = [(Mono Y_LL, rest)]
  readsPrec _ ('z':rest) = [(Mono Z_LL, rest)]

  readsPrec _ _ = []

instance Eq (Mono Letter) where
  Mono a == Mono b = case cast b of
    Just bb -> a == bb
    Nothing -> False

deriving instance Eq (Letter a)
deriving instance Typeable Letter
deriving instance Typeable Cap

deriving instance Typeable A_
deriving instance Typeable B_
deriving instance Typeable C_
deriving instance Typeable D_
deriving instance Typeable E_
deriving instance Typeable F_
deriving instance Typeable G_
deriving instance Typeable H_
deriving instance Typeable I_
deriving instance Typeable J_
deriving instance Typeable K_
deriving instance Typeable L_
deriving instance Typeable M_
deriving instance Typeable N_
deriving instance Typeable O_
deriving instance Typeable P_
deriving instance Typeable Q_
deriving instance Typeable R_
deriving instance Typeable S_
deriving instance Typeable T_
deriving instance Typeable U_
deriving instance Typeable V_
deriving instance Typeable W_
deriving instance Typeable X_
deriving instance Typeable Y_
deriving instance Typeable Z_
