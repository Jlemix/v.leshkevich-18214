--1. toDecimal base snumber – переводит строковое представление числа snumber из системы по основанию base в строковое представление десятичного числа.

toDecimal :: Char->String->String
toDecimal base snumber | base == '1' = turing (snumber) 0
		       | base > '1' && base <= '9' = toDecimalFunc (fromEnum base - 48) (reverse snumber) 0 1 -- ord?
		       | base >= 'A' && base <= 'Z' = toDecimalFunc (fromEnum base - 29) (reverse snumber) 0 1
		       | base >= 'a' && base <= 'z' = toDecimalFunc (fromEnum base - 87) (reverse snumber) 0 1
		       | snumber == [] = error "There is no number"
		       | fromEnum(head snumber) < 48 || fromEnum(head snumber) > 122 || fromEnum(head snumber) > 57 && fromEnum(head snumber) < 65 || fromEnum(head snumber) > 90 && fromEnum(head snumber) < 97 = error "Wrong number"
		       | otherwise = error "Wrong Base"
				where
				    turing [] length = show length
				    turing (hed:snumber) length = turing snumber (length + 1)
				    toDecimalFunc base [] num decim = show num
				    toDecimalFunc base (hed:snumber) num decim | hed >= '0' &&  hed <= '9' && fromEnum hed - 48 < base = toDecimalFunc base snumber (num + (decim *(fromEnum hed - 48))) (decim * base)
								               | hed >= 'A' &&  hed <= 'Z' && fromEnum hed - 29 < base = toDecimalFunc base snumber (num + (decim *(fromEnum hed - 29))) (decim * base)
									       | hed >= 'a' &&  hed <= 'z' && fromEnum hed - 87 < base = toDecimalFunc base snumber (num + (decim *(fromEnum hed - 87))) (decim * base)
									       | otherwise = error "Wrong Number"
--Example:
--toDecimal '2' "111" => toDecimalFunc '2' "111" 0 1 => toDecimalFunc '2' "11" (0 + (1 * (1))) (1 * 2) => toDecimalFunc '2' "1" (1 + (2 * (1))) (2 * 2) => toDecimalFunc '2' "" (3 + (4 * (1))) (4 * 2) => 7																	   

--2. fromDecimal toBase snumber — переводит строковое представление числа snumber из системы по основанию 10 в строковое представление числа в системе по основанию toBase.	
																		   
fromDecimal :: Char->String->String
fromDecimal toBase snumber | toBase == '1' = turing [] (read snumber::Int) 
			   | toBase > '1' && toBase <= '9' = fromDecimalFunc (fromEnum toBase - 48) (read snumber::Int) []
			   | toBase >= 'A' && toBase <= 'Z' = fromDecimalFunc (fromEnum toBase - 29) (read snumber::Int) []
			   | toBase >= 'a' && toBase <= 'z' = fromDecimalFunc (fromEnum toBase - 87) (read snumber::Int) []
			   | snumber == [] = error "There is no number"
			   | fromEnum(head snumber) < 48 || fromEnum(head snumber) > 122 || fromEnum(head snumber) > 57 && fromEnum(head snumber) < 65 || fromEnum(head snumber) > 90 && fromEnum(head snumber) < 97 = error "Wrong number"
			   | otherwise = error "Wrong Base"
				    where
					turing num 0 = '1':num
					turing num length = turing ('1':num) (length - 1)
					fromDecimalFunc :: Int -> Int -> String -> String
					fromDecimalFunc base 0 num = num
					fromDecimalFunc base snumber num = fromDecimalFunc base (div snumber base) ((toEnum(asci (mod snumber base) )::Char):num)
						where
						    asci numba | numba < 10 = numba + 48
							       | numba > 9 && numba < 36 = numba + 87
							       | numba > 35 && numba < 62 = numba + 29
							       | otherwise = error "Not in range"
--Example:													   
--fromDecimal '2' "4" => fromDecimalFunc (50 - 48) (4) [] => fromDecimalFunc 2 4 [] => fromDecimalFunc 2 (2) ((toEnum(48)::Char):num) <- ('0' : []) => ... => fromDecimalFunc 2 0 "100" = "100"

--3. convertFromTo fromBase toBase snumber — переводит строковое представление числа snumber из системы по основанию fromBase в строковое представление числа в системе по основанию toBase.
													   
convertFromTo::Char->Char->String->String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal (fromBase) (snumber))													   
