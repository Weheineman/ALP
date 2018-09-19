{-# OPTIONS_GHC -w #-}
module Parse where
import Common
import Data.Maybe
import Data.Char

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t6 t7 t12 t13
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 (LamTerm)
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13

action_0 (16) = happyShift action_10
action_0 (18) = happyShift action_11
action_0 (21) = happyShift action_12
action_0 (23) = happyShift action_5
action_0 (24) = happyShift action_13
action_0 (28) = happyShift action_14
action_0 (30) = happyShift action_15
action_0 (31) = happyShift action_16
action_0 (32) = happyShift action_17
action_0 (33) = happyShift action_18
action_0 (34) = happyShift action_19
action_0 (6) = happyGoto action_22
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_23
action_0 (9) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (11) = happyGoto action_9
action_0 _ = happyFail

action_1 (23) = happyShift action_5
action_1 (7) = happyGoto action_20
action_1 (13) = happyGoto action_21
action_1 _ = happyReduce_31

action_2 (16) = happyShift action_10
action_2 (18) = happyShift action_11
action_2 (21) = happyShift action_12
action_2 (24) = happyShift action_13
action_2 (28) = happyShift action_14
action_2 (30) = happyShift action_15
action_2 (31) = happyShift action_16
action_2 (32) = happyShift action_17
action_2 (33) = happyShift action_18
action_2 (34) = happyShift action_19
action_2 (8) = happyGoto action_6
action_2 (9) = happyGoto action_7
action_2 (10) = happyGoto action_8
action_2 (11) = happyGoto action_9
action_2 _ = happyFail

action_3 (23) = happyShift action_5
action_3 (7) = happyGoto action_4
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (21) = happyShift action_35
action_5 _ = happyFail

action_6 (26) = happyShift action_24
action_6 (36) = happyAccept
action_6 _ = happyFail

action_7 (16) = happyShift action_10
action_7 (18) = happyShift action_11
action_7 (21) = happyShift action_12
action_7 (24) = happyShift action_13
action_7 (28) = happyShift action_14
action_7 (30) = happyShift action_15
action_7 (31) = happyShift action_16
action_7 (32) = happyShift action_17
action_7 (33) = happyShift action_18
action_7 (34) = happyShift action_19
action_7 (10) = happyGoto action_33
action_7 (11) = happyGoto action_34
action_7 _ = happyReduce_8

action_8 _ = happyReduce_11

action_9 _ = happyReduce_6

action_10 (21) = happyShift action_32
action_10 _ = happyFail

action_11 (16) = happyShift action_10
action_11 (18) = happyShift action_11
action_11 (21) = happyShift action_12
action_11 (24) = happyShift action_13
action_11 (28) = happyShift action_14
action_11 (30) = happyShift action_15
action_11 (31) = happyShift action_16
action_11 (32) = happyShift action_17
action_11 (33) = happyShift action_18
action_11 (34) = happyShift action_19
action_11 (8) = happyGoto action_31
action_11 (9) = happyGoto action_7
action_11 (10) = happyGoto action_8
action_11 (11) = happyGoto action_9
action_11 _ = happyFail

action_12 _ = happyReduce_12

action_13 (21) = happyShift action_30
action_13 _ = happyFail

action_14 _ = happyReduce_14

action_15 (16) = happyShift action_10
action_15 (18) = happyShift action_11
action_15 (21) = happyShift action_12
action_15 (24) = happyShift action_13
action_15 (28) = happyShift action_14
action_15 (30) = happyShift action_15
action_15 (31) = happyShift action_16
action_15 (32) = happyShift action_17
action_15 (33) = happyShift action_18
action_15 (34) = happyShift action_19
action_15 (8) = happyGoto action_29
action_15 (9) = happyGoto action_7
action_15 (10) = happyGoto action_8
action_15 (11) = happyGoto action_9
action_15 _ = happyFail

action_16 (16) = happyShift action_10
action_16 (18) = happyShift action_11
action_16 (21) = happyShift action_12
action_16 (24) = happyShift action_13
action_16 (28) = happyShift action_14
action_16 (30) = happyShift action_15
action_16 (31) = happyShift action_16
action_16 (32) = happyShift action_17
action_16 (33) = happyShift action_18
action_16 (34) = happyShift action_19
action_16 (8) = happyGoto action_28
action_16 (9) = happyGoto action_7
action_16 (10) = happyGoto action_8
action_16 (11) = happyGoto action_9
action_16 _ = happyFail

action_17 (18) = happyShift action_11
action_17 (21) = happyShift action_12
action_17 (28) = happyShift action_14
action_17 (34) = happyShift action_19
action_17 (10) = happyGoto action_27
action_17 _ = happyFail

action_18 (16) = happyShift action_10
action_18 (18) = happyShift action_11
action_18 (21) = happyShift action_12
action_18 (24) = happyShift action_13
action_18 (28) = happyShift action_14
action_18 (30) = happyShift action_15
action_18 (31) = happyShift action_16
action_18 (32) = happyShift action_17
action_18 (33) = happyShift action_18
action_18 (34) = happyShift action_19
action_18 (8) = happyGoto action_26
action_18 (9) = happyGoto action_7
action_18 (10) = happyGoto action_8
action_18 (11) = happyGoto action_9
action_18 _ = happyFail

action_19 _ = happyReduce_15

action_20 (23) = happyShift action_5
action_20 (7) = happyGoto action_20
action_20 (13) = happyGoto action_25
action_20 _ = happyReduce_31

action_21 (36) = happyAccept
action_21 _ = happyFail

action_22 (36) = happyAccept
action_22 _ = happyFail

action_23 (26) = happyShift action_24
action_23 _ = happyReduce_4

action_24 (18) = happyShift action_43
action_24 (22) = happyShift action_44
action_24 (29) = happyShift action_45
action_24 (35) = happyShift action_46
action_24 (12) = happyGoto action_42
action_24 _ = happyFail

action_25 _ = happyReduce_30

action_26 _ = happyReduce_21

action_27 (18) = happyShift action_11
action_27 (21) = happyShift action_12
action_27 (28) = happyShift action_14
action_27 (34) = happyShift action_19
action_27 (10) = happyGoto action_41
action_27 _ = happyFail

action_28 _ = happyReduce_20

action_29 _ = happyReduce_19

action_30 (14) = happyShift action_40
action_30 _ = happyFail

action_31 (19) = happyShift action_38
action_31 (26) = happyShift action_24
action_31 (27) = happyShift action_39
action_31 _ = happyFail

action_32 (15) = happyShift action_37
action_32 _ = happyFail

action_33 _ = happyReduce_10

action_34 _ = happyReduce_9

action_35 (14) = happyShift action_36
action_35 _ = happyFail

action_36 (16) = happyShift action_10
action_36 (18) = happyShift action_11
action_36 (21) = happyShift action_12
action_36 (24) = happyShift action_13
action_36 (28) = happyShift action_14
action_36 (30) = happyShift action_15
action_36 (31) = happyShift action_16
action_36 (32) = happyShift action_17
action_36 (33) = happyShift action_18
action_36 (34) = happyShift action_19
action_36 (8) = happyGoto action_54
action_36 (9) = happyGoto action_7
action_36 (10) = happyGoto action_8
action_36 (11) = happyGoto action_9
action_36 _ = happyFail

action_37 (18) = happyShift action_43
action_37 (22) = happyShift action_44
action_37 (29) = happyShift action_45
action_37 (35) = happyShift action_46
action_37 (12) = happyGoto action_53
action_37 _ = happyFail

action_38 _ = happyReduce_13

action_39 (16) = happyShift action_10
action_39 (18) = happyShift action_11
action_39 (21) = happyShift action_12
action_39 (24) = happyShift action_13
action_39 (28) = happyShift action_14
action_39 (30) = happyShift action_15
action_39 (31) = happyShift action_16
action_39 (32) = happyShift action_17
action_39 (33) = happyShift action_18
action_39 (34) = happyShift action_19
action_39 (8) = happyGoto action_52
action_39 (9) = happyGoto action_7
action_39 (10) = happyGoto action_8
action_39 (11) = happyGoto action_9
action_39 _ = happyFail

action_40 (16) = happyShift action_10
action_40 (18) = happyShift action_11
action_40 (21) = happyShift action_12
action_40 (24) = happyShift action_13
action_40 (28) = happyShift action_14
action_40 (30) = happyShift action_15
action_40 (31) = happyShift action_16
action_40 (32) = happyShift action_17
action_40 (33) = happyShift action_18
action_40 (34) = happyShift action_19
action_40 (8) = happyGoto action_51
action_40 (9) = happyGoto action_7
action_40 (10) = happyGoto action_8
action_40 (11) = happyGoto action_9
action_40 _ = happyFail

action_41 (16) = happyShift action_10
action_41 (18) = happyShift action_11
action_41 (21) = happyShift action_12
action_41 (24) = happyShift action_13
action_41 (28) = happyShift action_14
action_41 (30) = happyShift action_15
action_41 (31) = happyShift action_16
action_41 (32) = happyShift action_17
action_41 (33) = happyShift action_18
action_41 (34) = happyShift action_19
action_41 (10) = happyGoto action_49
action_41 (11) = happyGoto action_50
action_41 _ = happyFail

action_42 (20) = happyShift action_48
action_42 _ = happyReduce_7

action_43 (18) = happyShift action_43
action_43 (22) = happyShift action_44
action_43 (29) = happyShift action_45
action_43 (35) = happyShift action_46
action_43 (12) = happyGoto action_47
action_43 _ = happyFail

action_44 _ = happyReduce_24

action_45 _ = happyReduce_27

action_46 _ = happyReduce_29

action_47 (19) = happyShift action_59
action_47 (20) = happyShift action_48
action_47 (27) = happyShift action_60
action_47 _ = happyFail

action_48 (18) = happyShift action_43
action_48 (22) = happyShift action_44
action_48 (29) = happyShift action_45
action_48 (35) = happyShift action_46
action_48 (12) = happyGoto action_58
action_48 _ = happyFail

action_49 _ = happyReduce_23

action_50 _ = happyReduce_22

action_51 (25) = happyShift action_57
action_51 (26) = happyShift action_24
action_51 _ = happyFail

action_52 (19) = happyShift action_56
action_52 (26) = happyShift action_24
action_52 _ = happyFail

action_53 (17) = happyShift action_55
action_53 (20) = happyShift action_48
action_53 _ = happyFail

action_54 (26) = happyShift action_24
action_54 _ = happyReduce_5

action_55 (16) = happyShift action_10
action_55 (18) = happyShift action_11
action_55 (21) = happyShift action_12
action_55 (24) = happyShift action_13
action_55 (28) = happyShift action_14
action_55 (30) = happyShift action_15
action_55 (31) = happyShift action_16
action_55 (32) = happyShift action_17
action_55 (33) = happyShift action_18
action_55 (34) = happyShift action_19
action_55 (8) = happyGoto action_63
action_55 (9) = happyGoto action_7
action_55 (10) = happyGoto action_8
action_55 (11) = happyGoto action_9
action_55 _ = happyFail

action_56 _ = happyReduce_16

action_57 (16) = happyShift action_10
action_57 (18) = happyShift action_11
action_57 (21) = happyShift action_12
action_57 (24) = happyShift action_13
action_57 (28) = happyShift action_14
action_57 (30) = happyShift action_15
action_57 (31) = happyShift action_16
action_57 (32) = happyShift action_17
action_57 (33) = happyShift action_18
action_57 (34) = happyShift action_19
action_57 (8) = happyGoto action_62
action_57 (9) = happyGoto action_7
action_57 (10) = happyGoto action_8
action_57 (11) = happyGoto action_9
action_57 _ = happyFail

action_58 (20) = happyShift action_48
action_58 _ = happyReduce_25

action_59 _ = happyReduce_26

action_60 (18) = happyShift action_43
action_60 (22) = happyShift action_44
action_60 (29) = happyShift action_45
action_60 (35) = happyShift action_46
action_60 (12) = happyGoto action_61
action_60 _ = happyFail

action_61 (19) = happyShift action_64
action_61 (20) = happyShift action_48
action_61 _ = happyFail

action_62 (26) = happyShift action_24
action_62 _ = happyReduce_17

action_63 (26) = happyShift action_24
action_63 _ = happyReduce_18

action_64 _ = happyReduce_28

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (Eval happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 7 happyReduction_5
happyReduction_5 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Def happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  8 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Asc happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (App happy_var_1 happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (App happy_var_1 happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn8
		 (LVar happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn8
		 (LUnit
	)

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn8
		 (Cero
	)

happyReduce_16 = happyReduce 5 10 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Par happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 6 11 happyReduction_17
happyReduction_17 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (LetIn happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 11 happyReduction_18
happyReduction_18 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Abs happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  11 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Pri happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  11 happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Seg happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  11 happyReduction_21
happyReduction_21 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Succ happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 11 happyReduction_22
happyReduction_22 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Rec happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 11 happyReduction_23
happyReduction_23 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Rec happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn12
		 (Base
	)

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Fun happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  12 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn12
		 (TUnit
	)

happyReduce_28 = happyReduce 5 12 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TPar happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn12
		 (TNat
	)

happyReduce_30 = happySpecReduce_2  13 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  13 happyReduction_31
happyReduction_31  =  HappyAbsSyn13
		 ([]
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 36 36 tk (HappyState action) sts stk;
	TEquals -> cont 14;
	TColon -> cont 15;
	TAbs -> cont 16;
	TDot -> cont 17;
	TOpen -> cont 18;
	TClose -> cont 19;
	TArrow -> cont 20;
	TVar happy_dollar_dollar -> cont 21;
	TType -> cont 22;
	TDef -> cont 23;
	TLet -> cont 24;
	TIn -> cont 25;
	TAs -> cont 26;
	TComa -> cont 27;
	Tunit -> cont 28;
	TTUnit -> cont 29;
	TFst -> cont 30;
	TSnd -> cont 31;
	TR -> cont 32;
	TSuc -> cont 33;
	TZero -> cont 34;
	TTNat -> cont 35;
	_ -> happyError' tk
	})

happyError_ 36 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = (\token -> happyError) tk

parseStmt = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parseStmts = happySomeParser where
  happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

term = happySomeParser where
  happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TType
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose 
               | TColon
               | TArrow
               | TEquals
               | TEOF
               | TLet
               | TIn
               | TAs
               | Tunit
               | TTUnit 
               | TComa
               | TFst
               | TSnd  
               | TTNat 
               | TZero 
               | TR
               | TSuc
               deriving Show 


----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
	            ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    (')':cs) -> cont TClose cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    (',': cs) -> cont TComa cs  
                    ('0':cs)  -> cont TZero cs      
                    unknown -> \line -> Failed $ "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                                           ("B",rest)   -> cont TType rest
                                           ("def",rest) -> cont TDef rest
                                                
                                           ("let", rest) -> cont TLet rest
                                           ("in", rest) -> cont TIn rest
                                           ("as",rest) -> cont TAs rest
                                           ("unit", rest) -> cont Tunit rest
                                           ("Unit",rest) -> cont TTUnit rest
                                           ("fst", rest) -> cont TFst rest
                                           ("snd", rest) -> cont TSnd rest
                                           ("suc", rest) -> cont TSuc rest
                                           ("Nat",rest) -> cont TTNat rest                                             
                                           ("R", rest) -> cont TR rest                                        
                                           (var,rest)   -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                                                                      ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
		                                                      ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
		                                                      ('-':('}':cs)) -> case anidado of
			                                                                     0 -> \line -> lexer cont cs (line+cl)
			                                                                     _ -> consumirBK (anidado-1) cl cont cs
		                                                      ('\n':cs) -> consumirBK anidado (cl+1) cont cs
		                                                      (_:cs) -> consumirBK anidado cl cont cs     
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
