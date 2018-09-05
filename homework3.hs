f x | sin(2*x) < 0 = error "sqrt(sin(2*x)), => sin(2*x)>=0"
    | sin(3*x) < 0 = error "sqrt(sin(3*x)), => sin(3*x)>=0"
	| otherwise = sqrt(sin(2*x)) - sqrt(sin(3*x))