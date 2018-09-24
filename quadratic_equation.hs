sqroot a b c | ((a == 0) && (b /= 0)) = ((-c / b),(-c / b))
	     | ((a == 0) && (b == 0)) = error "No roots"
	     | otherwise = (koren1,koren2)
			 where
				discriminant a b c = b^2 - 4*a*c
				drot = if ((discriminant a b c) < 0) then error "No roots" 
				     else sqrt(discriminant a b c)
				(koren1,koren2) = ((-b + (drot)) / (2 * a), (-b - (drot)) / (2 * a))

