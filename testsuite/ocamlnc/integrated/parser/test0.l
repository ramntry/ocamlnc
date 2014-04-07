pow (k, n) {
 r := 1;

 while n >> 0 do {
   r := r * k;
   n := n - 1;
 }

 return r;
}

fact (n) {
  if n <= 1 then return 1;
  else return n * fact (n-1); 
}

main () {
  read (n);
  write (fact (n));
}
