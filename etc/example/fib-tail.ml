;;
let rec fib_tail n a b = if n = 0 then a else fib_tail (n - 1) b (a + b) in
print_int (fib_tail 30 0 1)
