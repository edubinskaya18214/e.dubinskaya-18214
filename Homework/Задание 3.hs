a1 x = sin(2*x)
a2 x = sin(3*x)
fun x = if (a1 x < 0)then error "You are bad person"
	else if(a2 x < 0) then error "Ohhh"
	else sqrt(sin(2*x)) - sqrt(sin(3*x))
