eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam _ u :@: Unit)    = eval e (sub 0 Unit u)
eval e (Lam _ u :@: Zero)    = eval e (sub 0 Zero u)                                         
eval e (Lam t u :@: v)       = case eval e v of 
                 VLam t' u'    -> eval e (Lam t u :@: Lam t' u')
                 VUnit         -> eval e (Lam t u :@: Unit)
                 VPar v1 v2    -> eval e (sub 0 p u) where p = Pair (quote v1) (quote v2)
                 NV V0         -> eval e (Lam t u :@: Zero)
                 NV (VSuc nv)  -> eval e (sub 0 s u) where s = Suc (quote (NV nv))    
                 _             -> error "BBBBBB Error de tipo en run-time, verificar type checker" 
eval e (u :@: v)             = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 _         -> error "SSSSSSSS Error de tipo en run-time, verificar type checker"

eval e (Let t@(Lam s u) t2) = eval e (sub 0 t t2 )
eval e (Let Unit t2) = eval e (sub 0 Unit t2)
eval e (Let Zero t2) = eval e (sub 0 Zero t2)
eval e (Let t t2) = case eval e t of
                    VLam t' u -> eval e (Let v t2) where v = Lam t' u 
                    VUnit -> eval e (Let Unit t2)
                    VPar v1 v2 -> eval e (sub 0 p t2 ) where p = Pair (quote v1) (quote v2)
                    NV V0         -> eval e (Let t t2 :@: Zero)
                    NV (VSuc nv)  -> eval e (sub 0 s t2) where s = Suc (quote (NV nv))    
                    _         -> error "EEEEEEEEE Error de tipo en run-time, verificar type checker"

eval e (As t tt) = eval e t                     

eval e Unit = VUnit

eval e (Pair (Lam t u) t2) = case eval e t2 of
                    VLam t' u     -> (VPar (VLam t u) v2) where v2 = VLam t' u 
                    VUnit         -> (VPar (VLam t u) VUnit )
                    VPar v1 v2    -> (VPar (VLam t u) (VPar v1 v2) )
                    NV V0         -> (VPar (VLam t u) (NV V0) )
                    NV (VSuc nv)  -> (VPar (VLam t u) (NV (VSuc nv)) )    
                        
                    _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Pair Unit t2) = case eval e t2 of
                    VLam t' u     -> (VPar VUnit v2) where v2 = VLam t' u 
                    VUnit         -> (VPar VUnit VUnit )
                    VPar v1 v2    -> (VPar VUnit (VPar v1 v2) )     	        
                    NV V0         -> (VPar VUnit (NV V0) )
                    NV (VSuc nv)  -> (VPar VUnit (NV (VSuc nv)) )    
                    _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Pair Zero t2) = case eval e t2 of
                   {- VLam t' u     -> (VPar (NV V0) v2) where v2 = VLam t' u 
                    VUnit         -> (VPar (NV V0) VUnit )
                    VPar v1 v2    -> (VPar (NV V0) (VPar v1 v2) )     	        
                    NV V0         -> (VPar (NV V0) (NV V0) )
                    NV (VSuc nv)  -> (VPar (NV V0) (NV (VSuc nv)) )    
                    _         -> error "Error de tipo en run-time, verificar type checker"
                    -}
                     v -> (VPar (NV V0) v)   
eval e (Pair t1 t2) = case eval e t1 of
                    VLam t' u -> eval e (Pair v t2) where v = Lam t' u 
                    VUnit -> eval e (Pair Unit t2)
                    NV V0 -> eval e (Pair Zero t2)
                    VPar v1 v2 -> case eval e t2 of --VER!!!!!!!
                                VLam t' u -> (VPar (VPar v1 v2) v) where v = VLam t' u 
                                VUnit -> (VPar (VPar v1 v2) VUnit )
                                VPar v1' v2' -> (VPar (VPar v1 v2) (VPar v1' v2') )
                                NV V0 -> (VPar (VPar v1 v2) (NV V0) )
                                NV (VSuc nv) -> (VPar (VPar v1 v2) (NV (VSuc nv)) )
                    NV (VSuc nv) -> case eval e t2 of --VER!!!!!!!
                                VLam t' u -> (VPar (NV (VSuc nv)) v) where v = VLam t' u 
                                VUnit -> (VPar (NV (VSuc nv)) VUnit )
                                VPar v1' v2' -> (VPar (NV (VSuc nv)) (VPar v1' v2') )
                                NV V0 -> (VPar (NV (VSuc nv)) (NV V0) )
                                NV (VSuc nv) -> (VPar (NV (VSuc nv)) (NV (VSuc nv)) )                          
                    _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Fst t) = case eval e t of
                VPar t1 t2 -> t1
                _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Snd t) = case eval e t of
                VPar t1 t2 -> t2
                _         -> error "Error de tipo en run-time, verificar type checker"
eval e Zero = NV V0
eval e (Suc t) = case eval e t of
         NV V0        -> NV (VSuc V0)
         NV (VSuc nv) -> NV (VSuc (VSuc nv))                              
         _            -> error "SADADA Error de tipo en run-time, verificar type checker"
                     
eval e (R t1 t2 t3) = case eval e t3 of
                NV V0        -> eval e t1
                NV (VSuc nv) -> eval e ((t2 :@: (R t1 t2 t)) :@: t) where t = quote (NV nv)
               
-- NV (VSuc nv) -> eval e ((t2 :@: (R t1 t2 t)) :@: t) where t = quote (NV nv)
                _          -> error "Error de tipo en run-time AAAAAAAAA, verificar type checker"
