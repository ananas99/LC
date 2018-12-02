let rec comb n k =
	
	if k == 0 || k == n  then 1
	else comb (n - 1) (k - 1) + comb (n - 1) k
;;


Scanf.scanf "%i %i\n" (fun n k ->
	print_int (((comb n k) * comb n (k-1))/n)
	
)	
;;

print_newline();;
