{def factorial[n]
	{def inner[n t]
		(if (<= n 1)
			t
			inner[(- n 1) (* t n)])}
	inner[n 1]}

println[factorial[5]]
println[factorial[10]]
