sqroot a b c = sqrot a b c
				where sqrot a b c | a == 0 = error "Wrong inputs"
								  | otherwise = discriminant a b c
												where discriminant a b c | b^2 - 4*a*c < 0 = error "No roots"
																		 | b^2 - 4*a*c == 0 = roots a b c
																		 | otherwise = roots a b c
																						where roots a b c | b^2 - 4*a*c == 0 = -b / (2*a)
																										  | otherwise = ruts a b с
																														where ruts a b с = do 
																																		(-b + sqrt(b^2 - 4*a*c)) / (2*a) 
																																		(-b - sqrt(b^2 - 4*a*c)) / (2*a)
																																		
--                ступор, как вывести два значения?!?