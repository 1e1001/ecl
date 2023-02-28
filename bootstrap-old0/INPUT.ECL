{define factorial[n]
  (if (<= n 1)
	    1
      (* factorial[(- n 1)] n))}

(println factorial[5])
