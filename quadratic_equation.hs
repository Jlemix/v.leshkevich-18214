sqroot a b c | a == 0 = error "Wrong inputs"
	     | otherwise = discriminant a b c
			   where discriminant a b c | b^2 - 4*a*c < 0 = error "No roots"
						    | b^2 - 4*a*c == 0 = -b / (2*a)
						    | otherwise = roots x1 x2
								  where roots x1 x2 = (((-b + sqrt(b^2 - 4*a*c)) / (2*a)), ((-b - sqrt(b^2 - 4*a*c)) / (2*a)))
