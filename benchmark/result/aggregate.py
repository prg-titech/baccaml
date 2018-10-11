import statistics as st

def average(lst):
    return sum(lst) / float(len(lst))


def variance(lst):
    return st.variance(lst)


def print_result(name, lst):
    print "%s: \tAverage %f s \tVariance %f" % (name, average(lst), variance(lst))


def calc_fib40():
    from test_fib import *
    print "fib(40)"
    print_result("Method JIT", res_fib_mj)
    print_result("MinCaml", res_fib_mincaml)
    print_result("C (clang)", res_fib_c)
    print_result("PyPy (6.0)", res_fib_pypy)


def calc_fib28():
    from test_fib28 import *
    print "fib(28)"
    print_result("Method JIT", res_fib28_mj)
    print_result("Tracing JIT", res_fib28_tj)
    print_result("MinCaml", res_fib28_mincaml)
    print_result("C (clang)", res_fib28_c)
    print_result("PyPy (6.0)", res_fib28_pypy)

def main():
    calc_fib40()
    calc_fib28()


main()
