discriminant a b c = b^2 - 4*a*c 			   
sqroot a b c | (discriminant a b c) < 0 = error "No roots"
	     | (discriminant a b c) == 0 = ((-b / (2*a)), (-b / (2*a)))
	     | otherwise = (((-b - sqrt(discriminant a b c)) / (2*a)), ((-b + sqrt(discriminant a b c)) / (2*a)))
